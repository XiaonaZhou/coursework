########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)

library(scales)
library(lubridate)
# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides (compare a histogram vs. a density plot)

# convert the unit of tripduration to minutes and apply log scale on x-axis
trips <- trips %>% 
  mutate(trip_time_mint = round(tripduration/60))
trips %>% 
  ggplot(aes(x= trip_time_mint))+
  geom_histogram()+
  scale_x_log10()

trips <- trips %>% 
  mutate(trip_time_mint = round(tripduration/60))
trips %>% 
  ggplot(aes(x= trip_time_mint))+
  geom_density(fill = "grey")+
  scale_x_log10()

# another way: plot only 99% of the data. 
max_x <- quantile(trips$tripduration, probs = 0.99)# 99% of the data are under 3558 seconds.
trips %>% 
  ggplot(aes(x=tripduration)) +
  geom_histogram(bins = 25)+
  xlim(c(0, max_x))

max_x <- quantile(trips$tripduration, probs = 0.99)# 99% of the data are under 3558 seconds.
trips %>% 
  ggplot(aes(x=tripduration)) +
  geom_density(fill="grey")+
  xlim(c(0, max_x))

# plot the distribution of trip times by rider type indicated using color and fill (compare a histogram vs. a density plot)

trips %>% 
  group_by(usertype) %>% 
  ggplot(aes(x=trip_time_mint,  fill = usertype))+
  geom_histogram()+
  scale_x_log10()



# another way: plot only 99% of the data.

# overlap plot
trips %>% 
  group_by(usertype) %>% 
  ggplot(aes(x=tripduration,  fill = usertype, color = usertype))+
  geom_histogram(alpha = 0.3, position = "identity")+
  xlim(c(0, max_x))


# use facet to get two plots
trips %>% 
  group_by(usertype) %>% 
  ggplot(aes(x=tripduration,  fill = usertype, color = usertype))+
  geom_histogram(alpha = 0.3)+
  xlim(c(0, max_x))+facet_wrap(~ usertype)
  
# can also use facet_wrap(~ usertype, scale = "free_y") 
trips %>% 
  group_by(usertype) %>% 
  ggplot(aes(x=tripduration,  fill = usertype, color = usertype))+
  geom_histogram(alpha = 0.3)+
  xlim(c(0, max_x))+facet_wrap(~ usertype, scale = "free_y") 


# plot the total number of trips on each day in the dataset
trips %>%
  mutate(trip_day = floor_date(starttime,'day')) %>% 
  group_by(trip_day) %>% 
  summarise(trip_num = n()) %>% 
  ggplot(aes(x=trip_day, y = trip_num))+
  geom_point()



#use ymd

trips %>%
  group_by(ymd) %>% 
  summarise(trip_num = n()) %>% 
  ggplot(aes(x=ymd, y = trip_num))+
  geom_point()

# plot the total number of trips (on the y axis) by age (on the x axis) and gender (indicated with color)
trips %>% 
  filter(! is.na(birth_year)) %>% 
  mutate( age = 2014 - birth_year) %>% 
  group_by(age,gender) %>% 
  summarise(trip_num = n()) %>% 
  ungroup() %>%
  ggplot(aes(x= age, y = trip_num, color = gender))+
  geom_point()



# plot the ratio of male to female trips (on the y axis) by age (on the x axis)
# hint: use the spread() function to reshape things to make it easier to compute this ratio
# (you can skip this and come back to it tomorrow if we haven't covered spread() yet)

trips %>% 
  filter(! is.na(birth_year)) %>% 
  mutate( age = 2014 - birth_year) %>% 
  group_by(age,gender) %>% 
  summarise(trip_num = n()) %>% 
  pivot_wider(names_from = gender, values_from = trip_num) %>% 
  mutate(ratio = Male/Female) %>% 
  ggplot(aes(x=age, y =ratio)) + 
  geom_point()

########################################
# plot weather data
########################################
# plot the minimum temperature (on the y axis) over each day (on the x axis)
weather %>% 
  ggplot(aes(y = tmin, x = date)) +
  geom_line()



# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
# hint: try using the gather() function for this to reshape things before plotting
# (you can skip this and come back to it tomorrow if we haven't covered gather() yet)
weather %>% 
  gather(temperature, value, tmin, tmax) %>% 
  ggplot(aes(x = date, y = value, colour = temperature)) +
  geom_point()


########################################
# plot trip and weather data
########################################

# join trips and weather

trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
trips_with_weather %>% 
  group_by(ymd, tmin) %>% 
  summarise(n_trips = n()) %>% 
  ggplot(aes(x = tmin, y = n_trips))+
  geom_point()


# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this

# I decided that prcp > 0.5 means rain

trips_with_weather %>% 
  mutate(rain = ifelse(prcp > 0.5, T, F)) %>% 
  group_by(ymd, tmin, rain) %>% 
  summarise(n_trips = n()) %>% 
  ggplot(aes(x = tmin, y = n_trips, color = rain))+
  geom_point()


# add a smoothed fit on top of the previous plot, using geom_smooth

trips_with_weather %>% 
  mutate(rain = ifelse(prcp > 0.5, T, F)) %>% 
  group_by(ymd, tmin, rain) %>% 
  summarise(n_trips = n()) %>% 
  ggplot(aes(x = tmin, y = n_trips, color = rain))+
  geom_point()+
  geom_smooth()




# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package


trips %>%
  mutate(hour = hour(starttime)) %>%
  group_by(ymd, hour) %>%
  summarise(count = n()) %>%
  group_by(hour) %>%
  summarise(mean_trip_hour = mean(count),
            sd_trip_hour = sd(count)) %>%
  gather(stats,value, mean_trip_hour, sd_trip_hour) %>%
  ggplot(aes(x=hour, y=value, colour=stats)) +
  geom_line()+
  geom_point()+
  xlab("hour") +
  ylab("number of trips") +
  labs(title = "mean and standard deviation of number of trips by day")





# # This was my first attempt
# reshape_trips <-trips %>% 
#   mutate(hour = hour(starttime)) %>% 
#   group_by(ymd, hour) %>% 
#     summarise(count = n()) %>% pivot_wider(names_from = ymd, values_from = count)
# 
# 
# mean_sd <- data.frame()
# for (i in 1:24){
#   mean_sd[i,1] <- mean(as.numeric(reshape_trips[i,-1]),na.rm = T)
#   mean_sd[i,2] <- sd(as.numeric(reshape_trips[i,-1]),na.rm = T)
# 
# }
# names(mean_sd)[1] <- "mean_trips"
# names(mean_sd)[2] <- "sd_trips"
# 
# 
# mean_sd$hour <- reshape_trips$hour
# 
# # plot the above
# ggplot(mean_sd, aes(x = hour, y = mean_trips)) +
#   geom_point()
# ggplot(mean_sd, aes(x = hour, y = sd_trips)) +
#   geom_point()
# 
# mean_sd %>%
#   gather(stats,value, mean_trips, sd_trips) %>%
#   ggplot(aes(x=hour, y=value, colour=stats)) +
#   geom_line()+
#   geom_point()+
#   xlab("hour") +
#   ylab("number of trips") +
#   labs(title = "mean and standard deviation of number of trips by day")







# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package

trips %>% 
  mutate(weekday = weekdays(as.POSIXct(starttime), abbreviate = T)) %>% 
  group_by(ymd, weekday) %>% 
  summarise(count = n()) %>% 
  group_by(weekday) %>% 
  summarise(mean_trip_week = mean(count),
            sd_trip_week = sd(count)) %>% 
  gather(stats,value, mean_trip_week, sd_trip_week) %>%
  ggplot(aes(x=weekday, y=value, colour=stats)) +
  geom_line()+
  geom_point()+
  #coord_flip()+
  xlab("weekday") +
  ylab("number of trips") +
  labs(title = "mean and standard deviation of number of trips by weekdays")
  
  
