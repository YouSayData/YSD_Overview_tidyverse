
# Libraries ---------------------------------------------------------------

library(purrr)
library(repurrrsive)

# lapply vs map ------------------------------------------------------------------

got_chars[[1]]
old <- lapply(got_chars[1:3],
       function(x) x[["name"]])

new <- map(got_chars[1:3], "name")

identical(old, new)
old

lapply(1:2, rnorm)
map(1:2, rnorm)

lapply(1:2, function(x) rnorm(1, mean = x))
map(1:2, ~rnorm(1, mean = .))

# R >=4.1
# lapply(1:2, \(x) rnorm(1, mean = x))

# sapply ------------------------------------------------------------------

# unsafe base
sapply(got_chars[20:22], function(x) x[["aliases"]])

sapply(got_chars[c(3, 22, 27)], function(x) x[["aliases"]])


# vapply vs map_  ---------------------------------------------------------

vapply(got_chars[1:3],
       function(x) x[["name"]],
       character(1))

map_chr(got_chars[1:3], "name")

# map_dfr -----------------------------------------------------------------

# base

l <- lapply(got_chars[23:25],
            `[`, c("name", "playedBy"))
mat <- do.call(rbind, l)
as.data.frame(mat, stringsAsFactors = FALSE)

# tidyverse

map_dfr(got_chars[23:25],
        `[`, c("name", "playedBy"))

# mapply vs map2 pmap ---------------------------------------------------------

# base 

mapply(function(x, y) paste(x, "was born", y),
       vapply(got_chars[16:18],
              `[[`, character(1), "name"),
       vapply(got_chars[16:18],
              `[[`, character(1), "born"))

# tidy

map2_chr(map_chr(got_chars[16:18], "name"),
         map_chr(got_chars[16:18], "born"),
         ~ paste(.x, "was born", .y))

# or!
map_dfr(got_chars[16:18],
        `[`, c("name", "born")) %>% 
  glue::glue_data("{name} was born {born}")


# furrr & purrr ----------------------------------------------------------------

library(furrr)
library(tictoc)

count_target <- 250000

{
  tic()
  plan(sequential)
  the_count <- future_map_chr(1:count_target, ~str_c("I count ", . , "! Ha ha ha!"))
  toc()
}

{
  tic()
  the_count <- map_chr(1:count_target, ~str_c("I count ", . , "! Ha ha ha!"))
  toc()
}

{
  tic()
  plan(multisession, workers = 7)
  the_count <- future_map_chr(1:count_target, ~str_c("I count ", . , "! Ha ha ha!"))
  toc()
}

plan(sequential)

# replacing for loops with purrr ------------------------------------------

# random data creation function
newData <- function() {
  tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
  )
}

# If we want to calculate the median for each column we could do
# it in multiple ways

# Four steps
(df <- newData())
median(df$a)
median(df$b)
median(df$c)
median(df$d)

# This seems bulky. Let's use a for-loop instead

output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output

# if you interested in the side-effect and not want to generate an output
for (i in seq_along(df)) {            # 2. sequence
  print(median(df[[i]]))      # 3. body
}

# purrr

map_dbl(df, median)
walk(df, ~print(median(.)))
