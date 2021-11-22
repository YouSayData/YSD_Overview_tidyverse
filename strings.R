# libraries ---------------------------------------------------------------

library(tidyverse)

# data --------------------------------------------------------------------

helloWorld <- c("hello", "tidy", "world")

# base R ------------------------------------------------------------------

grep("l", helloWorld, value = TRUE)
grep("l", helloWorld)
grepl("l", helloWorld)

nchar(helloWorld)
paste(helloWorld, collapse = ",")

# tidyverse ---------------------------------------------------------------

str_subset(helloWorld, "l")
str_which(helloWorld, "l")
str_detect(helloWorld, "l")

str_length(helloWorld)
str_c(helloWorld, collapse = ",")

