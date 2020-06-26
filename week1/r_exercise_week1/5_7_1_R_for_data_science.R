# What time of day should you fly if you want to avoid delays as much as possible?

library(nycflights13)
library(tidyverse)

# add column day_in_hour, which round time into hour ignor year month day
# add column total_delay_mints, which is the sum of dep_delay and arr_delay
flights <- flights %>% 
  mutate( day_in_hour = hour(flights$time_hour),
          total_delay_mints = dep_delay + arr_delay) 


flights %>% 
  group_by(day_in_hour) %>% 
  summarise(count=n(), 
            sum_delay =sum(total_delay_mints, na.rm = T)) %>% 
  arrange(sum_delay ,count) %>% 
  select(sum_delay, day_in_hour, count) %>% 
  slice(1)
# fly at 7:00 am



