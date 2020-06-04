library(nycflights13)
library(tidyverse)

#Currently dep_time and sched_dep_time are convenient to look at, 
#but hard to compute with because they’re not really continuous numbers. 
#Convert them to a more convenient representation of number of 
#minutes since midnight.

flights %>% 
  mutate(dep_time_in_minutes = (dep_time %/% 100)*60 + dep_time %% 100,
         arr_time_in_minutes = (arr_time %/% 100)*60 + arr_time %% 100,
         sched_dep_time_in_mint = (sched_dep_time %/% 100)*60 + sched_dep_time %% 100,
         sched_arr_time_in_mint = (sched_arr_time %/% 100)*60 + sched_arr_time %% 100,
         diff_arr_dep = arr_time_in_minutes - dep_time_in_minutes,
         diff_sched_arr_dep = sched_arr_time_in_mint - sched_dep_time_in_mint) %>% 
  select(diff_arr_dep, diff_sched_arr_dep, air_time)

# Compare air_time with arr_time - dep_time. What do you expect to see? 
# What do you see? What do you need to do to fix it?

flights %>% 
  mutate(diff = arr_time - dep_time) %>% 
  filter(diff==air_time) %>% 
  select(diff, air_time)

# I should convert the time to number of minutes since midnight
flights %>% 
  mutate(dep_time_in_minutes = (dep_time %/% 100)*60 + dep_time %% 100,
         arr_time_in_minutes = (arr_time %/% 100)*60 + arr_time %% 100,
         sched_dep_time_in_mint = (sched_dep_time %/% 100)*60 + sched_dep_time %% 100,
         sched_arr_time_in_mint = (sched_arr_time %/% 100)*60 + sched_arr_time %% 100,
         diff_arr_dep = arr_time_in_minutes - dep_time_in_minutes,
         diff_sched_arr_dep = sched_arr_time_in_mint - sched_dep_time_in_mint) %>% 
  select(diff_arr_dep, diff_sched_arr_dep, air_time)

# However, they are still not the same


