
# Libraries ---------------------------------------------------------------

library(purrr)
library(repurrrsive)

# lapply vs map ------------------------------------------------------------------

old <- lapply(got_chars[1:3],
       function(x) x[["name"]])

new <- map(got_chars[1:3], "name")

identical(old, new)
old

lapply(1:2, rnorm)
map(1:2, rnorm)

lapply(1:2, function(x) rnorm(1, mean = x))

# R 4.1
# lapply(1:2, \(x) rnorm(1, mean = x))

map(1:2, ~rnorm(1, mean = .))


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

plan(sequential)
future_map_chr(1:3, ~str_c("I count ", . , "! Ha ha ha!"))

