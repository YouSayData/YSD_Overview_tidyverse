
# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(nycflights13)

# comparison --------------------------------------------------------------

# base R

# POSIXct = seconds post 1 Jan 1970
# POSIXlt stores the date as a list

datestring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
strptime(datestring, "%B %d, %Y %H:%M")
?strptime

# tidy
datestring %>%
  mdy_hm

"5 January 1999, 4 o'clock" %>% 
  dmy_h

"1999: January the 5th, 4:30:01, Germany" %>% 
  ymd_hms(tz = "CET")


# fun with time zones -----------------------------------------------------

now() %>% with_tz("CET")
now() %>% force_tz("CET")


# create datetime and dates from components --------------------

flights %>% 
  select(year, month, day, hour, minute)

flights %>% 
  select(year, month, day, hour, minute) %>%
  transmute(date = make_date(year, month, day),
            datetime = make_datetime(year, month, day, hour, minute))


# components from dates ---------------------------------------------------

flights %>%
  select(1:3, contains("dep_time")) %>%
  filter(is.na(dep_time)) %>%
  mutate(sched_dep_date = make_date(year, month, day),
         `Day of the Week` = wday(sched_dep_date, label = TRUE)) %>%
  ggplot(aes(x = `Day of the Week`)) +
  geom_bar() +
  ggthemes::theme_clean()


# Time spans --------------------------------------------------------------

# How old is Thomas?

t_age <- now() - ymd_hms(19810105190000)
class(t_age)
t_age
t_age %/% dyears(1)
as.duration(t_age) 

# Intervals

next_year <- today() + years(1)
(today() %--% next_year) / days(1)
