
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

