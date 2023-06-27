library(palmerpenguins)
library(torch)

penguins %>% glimpse()

penguins_dataset <- dataset(
  
  name = "penguins_dataset",
  
  initialize = function(df) {
    
    df <- na.omit(df) 
    
    # continuous input data (x_cont)   
    x_cont <- df[ , c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g", "year")] %>%
      as.matrix()
    self$x_cont <- torch_tensor(x_cont)
    
    # categorical input data (x_cat)
    x_cat <- df[ , c("island", "sex")]
    x_cat$island <- as.integer(x_cat$island)
    x_cat$sex <- as.integer(x_cat$sex)
    self$x_cat <- as.matrix(x_cat) %>% torch_tensor()
    
    # target data (y)
    species <- as.integer(df$species)
    self$y <- torch_tensor(species)
    
  },
  
  .getitem = function(i) {
    list(x_cont = self$x_cont[i, ], x_cat = self$x_cat[i, ], y = self$y[i])
    
  },
  
  .length = function() {
    self$y$size()[[1]]
  }
  
)

train_indices <- sample(1:nrow(penguins), 250)

train_ds <- penguins_dataset(penguins[train_indices, ])
valid_ds <- penguins_dataset(penguins[setdiff(1:nrow(penguins), train_indices), ])

train_ds[1]

train_dl <- train_ds %>% dataloader(batch_size = 16, shuffle = TRUE)

valid_dl <- valid_ds %>% dataloader(batch_size = 16, shuffle = FALSE)

embedding_module <- nn_module(
  
  initialize = function(cardinalities) {
    
    self$embeddings = nn_module_list(lapply(cardinalities, function(x) nn_embedding(num_embeddings = x, embedding_dim = ceiling(x/2))))
    
  },
  
  forward = function(x) {
    
    embedded <- vector(mode = "list", length = length(self$embeddings))
    for (i in 1:length(self$embeddings)) {
      embedded[[i]] <- self$embeddings[[i]](x[ , i])
    }
    
    torch_cat(embedded, dim = 2)
  }
)

net <- nn_module(
  "penguin_net",
  
  initialize = function(cardinalities,
                        n_cont,
                        fc_dim,
                        output_dim) {
    
    self$embedder <- embedding_module(cardinalities)
    self$fc1 <- nn_linear(sum(purrr::map(cardinalities, function(x) ceiling(x/2)) %>% unlist()) + n_cont, fc_dim)
    self$output <- nn_linear(fc_dim, output_dim)
    
  },
  
  forward = function(x_cont, x_cat) {
    
    embedded <- self$embedder(x_cat)
    
    all <- torch_cat(list(embedded, x_cont$to(dtype = torch_float())), dim = 2)
    
    all %>% self$fc1() %>%
      nnf_relu() %>%
      self$output() %>%
      nnf_log_softmax(dim = 2)
    
  }
)

model <- net(
  cardinalities = c(length(levels(penguins$island)), length(levels(penguins$sex))),
  n_cont = 5,
  fc_dim = 32,
  output_dim = 3
)

optimizer <- optim_adam(model$parameters, lr = 0.01)

for (epoch in 1:20) {
  
  model$train()
  train_losses <- c()  
  
  coro::loop(for (b in train_dl) {
    
    optimizer$zero_grad()
    output <- model(b$x_cont, b$x_cat)
    loss <- nnf_nll_loss(output, b$y)
    
    loss$backward()
    optimizer$step()
    
    train_losses <- c(train_losses, loss$item())
    
  })
  
  model$eval()
  valid_losses <- c()
  
  coro::loop(for (b in valid_dl) {
    
    output <- model(b$x_cont, b$x_cat)
    loss <- nnf_nll_loss(output, b$y)
    valid_losses <- c(valid_losses, loss$item())
    
  })
  
  cat(sprintf("Loss at epoch %d: training: %3.3f, validation: %3.3f\n", epoch, mean(train_losses), mean(valid_losses)))
}

