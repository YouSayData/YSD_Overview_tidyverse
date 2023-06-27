
# libraries ---------------------------------------------------------------

library(tidyverse)
library(nycflights13)


# code readability --------------------------------------------------------

flights %>% 
  glimpse

flights %>% 
  filter(year == 2013,
         month == 5,
         day == 4) %>%
  drop_na %>%
  group_by(hour, origin) %>%
  summarise(distance = mean(distance),
            count = n(),
            .groups = "drop") %>%
  ggplot(aes(hour, distance, fill = origin)) +
  geom_col()


# add-on ------------------------------------------------------------------
# base R has a pipe now too -----------------------------------------------
# Introduced in 4.1.0 and further functionality added in 4.2.0 and 4.3.0

flights |>
  filter(year == 2013,
         month == 5,
         day == 4) |>
  drop_na() |>
  group_by(hour, origin) |>
  summarise(distance = mean(distance),
            count = n(),
            .groups = "drop") |>
  ggplot(aes(hour, distance, fill = origin)) +
  geom_col()


# differences -------------------------------------------------------------

mtcars %>%
  lm(mpg ~ hp, data = .)

mtcars |>
  lm(mpg ~ hp, data = _)

mtcars %>% .$cyl
mtcars |> _$cyl

10 %>% rnorm(3, .)
10 |> rnorm(3, mean = _)

mtcars %>%
  glimpse

mtcars |>
  glimpse()
