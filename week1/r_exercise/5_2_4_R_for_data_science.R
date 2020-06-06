library(nycflights13)
library(tidyverse)

#Find all flights that had an arrival delay of two or more hours

flights %>% 
  filter(arr_delay >= 120) %>% 
  select(flight, arr_delay)

#Flew to Houston (IAH or HOU)
flights %>% 
  filter(dest == 'IAH'|dest == 'HOU') %>% 
  select(flight, dest)

#Were operated by United, American, or Delta
# use airlines to get the abbreviation.
airlines
flights %>% 
  filter(carrier == 'UA'| carrier == 'AA'| carrier == 'DL') %>% 
  select(flight, carrier)

#Departed in summer (July, August, and September)

flights %>% 
  filter(month == 7 | month == 8 | month == 9) %>% 
  select(flight, month)

#Arrived more than two hours late, but didn’t leave late

# Departure and arrival delays, in minutes. Negative times represent early departures/arrivals.

flights %>% 
  filter(dep_delay == 0 & arr_delay >=120) %>% 
  select(flight, dep_delay, arr_delay)



# Were delayed by at least an hour, but made up over 30 minutes in flight

flights %>% 
  filter(dep_delay >= 60 & arr_delay <= (dep_delay-30)) %>% 
  select(flight, dep_delay, arr_delay)

# Departed between midnight and 6am (inclusive)
flights %>% 
  mutate(dep_hour = hour(flights$time_hour)) %>% 
  filter(dep_hour <= 6 & dep_hour >= 0 )%>% 
  select(flight, dep_hour)



# 3. How many flights have a missing dep_time? What other variables are missing? 
    #What might these rows represent?

# flights that were canceled 

sum(is.na(flights$dep_time))# or 

flights %>% 
  filter(is.na(dep_time)) %>% 
  select(flight)


for (i in 1:ncol(flights)){
  if (sum(is.na(flights[,i]))>0){
    print(names(flights)[i])
    print(sum(is.na(flights[,i])))
  }
}



