
# libraries ---------------------------------------------------------------

library(tidyverse)

# connect to DB -----------------------------------------------------------

con <- DBI::dbConnect(RSQLite::SQLite(), path = ":dbname:")

# Copy some data to DB ----------------------------------------------------

copy_to(con, nycflights13::flights, "flights",
        temporary = FALSE, 
        indexes = list(
          c("year", "month", "day"), 
          "carrier", 
          "tailnum",
          "dest"
        )
)


# interact with it smoothly  --------------------------------------------

# this is technicallly not a tibble
flights_db <- tbl(con, "flights")

# it almost looks like one though
flights_db

# it is a remote database query though and you interact with it with dbplyr
# The great thing about dbplyr is that you can use it like dplyr

flights_db %>%
  select(year:day, dep_delay, arr_delay)

flights_db %>% filter(dep_delay > 240)

flights_db %>% 
  group_by(dest) %>%
  summarise(delay = mean(dep_time))

# all the work is done on the db site and not the client side.
# the client just looks at everything that needs doing and then will send one request
# that keeps traffic down

# this doesn't touch the db
tailnum_delay_db <- flights_db %>% 
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay),
    n = n()
  ) %>% 
  filter(n > 100)

# only this does
tailnum_delay_db

# see the query
tailnum_delay_db %>% show_query


# use collect to bring it to a local tibble -------------------------------

tailnum_delay <- tailnum_delay_db %>% collect
tailnum_delay

# good to know
nrow(tailnum_delay_db)
tail(tailnum_delay_db)

head(tailnum_delay_db)

