
# libraries ---------------------------------------------------------------

library(tidyverse)

# setup julia -------------------------------------------------------------

Sys.setenv(JULIA_NUM_THREADS = "7")

library(JuliaCall)
julia <- julia_setup()  

# functions -----------------------------------------------------------------

generate_toy_data <- function(n) {
  map(1:n, function(x) {
    result <- rnorm(n = 89)
    names(result)  <-  str_c("DIM_", formatC(1:89, width = 4, flag = 0))
    result
  }
  ) %>% bind_rows
}

measure_sim_mat_julia <- function(tibble_1, tibble_2) {
  julia$library("Distances")
  
  julia$assign("X", tibble_1 %>% 
                 select(-1) %>%
                 as.matrix %>%
                 t)
  
  julia$assign("Y", tibble_2 %>% 
                 select(-1) %>%
                 as.matrix %>%
                 t)
  
  1 - julia$eval("pairwise(cosine_dist, X, Y, dims=2)")
}


# test --------------------------------------------------------------------

{
tic()
results <- measure_sim_mat_julia(
  generate_toy_data(100),
  generate_toy_data(55000)
  )
toc()
}
