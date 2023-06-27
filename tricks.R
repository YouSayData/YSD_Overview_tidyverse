
# libraries -----------------------------------------------------------------

library(tidyverse)


# 1. names_glue parameter ----------------------------------------------------

(storms_sum <- storms %>% 
  filter(year %in% 1975:1977) %>% 
  group_by(year, status) %>% 
  summarise(mean = mean(pressure, na.rm = TRUE),
                   median = median(pressure, na.rm = TRUE),
                   .groups = "drop"))

storms_sum %>% 
  pivot_wider(names_from = "year", 
              values_from = c("mean", "median"))

(wide_storms <- storms_sum %>% 
  pivot_wider(names_from = "year", 
              values_from = c("mean", "median"), 
              names_glue = "{.value}_of_{year}"))


# 2. Custom break pivot_longer ---------------------------------------------

wide_storms %>% 
  pivot_longer(-status, 
               names_to = c("stat", "year"),
               names_pattern = "(.*)_of_(.*)",
               names_transform = list(year = as.integer),
               values_to = "value")


# 3. summarise multiple columns with multiple functions ----------------------
colnames(mtcars)

mtcars %>% 
  group_by(cyl) %>% 
  summarise(across(starts_with("d"),
                   list(mean = mean, median = median),
                   .names = "{fn}_of_{col}"))


# 4. create a fake legend with cowplot, ggplot, patchwork -----------------

p1 <- faithful %>% 
  ggplot(aes(eruptions, waiting)) +
  geom_hex(show.legend = F)

p2 <- mpg %>%
  ggplot(aes(displ, cty, col = cyl)) +
  geom_point()

layout <- c(
  area(t = 1, l = 1, b = 5, r = 4),
  area(t = 2, l = 5, b = 3, r = 5)
)

p1 + cowplot::get_legend(p2) + 
  plot_layout(design = layout)



