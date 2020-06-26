library(tidyverse)
library(lubridate)

########################################
# READ AND TRANSFORM THE DATA
########################################

# read one month of data
trips <- read_csv('201402-citibike-tripdata.csv')

# replace spaces in column names with underscores
names(trips) <- gsub(' ', '_', names(trips))

# convert dates strings to dates
#trips <- mutate(trips, starttime = mdy_hms(starttime), stoptime = mdy_hms(stoptime))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
trips <- mutate(trips, gender = factor(gender, levels=c(0,1,2), labels = c("Unknown","Male","Female")))


########################################
# YOUR SOLUTIONS BELOW
########################################

# count the number of trips (= rows in the data frame)

# Method 1
nrow(trips)

#Method 2

trips %>% 
  summarise(count = n())

# find the earliest and latest birth years (see help for max and min to deal with NAs)
trips %>% 
  filter(birth_year!="\\N") %>% 
  summarise(min_birth_year = min(birth_year, na.rm = T),
            max_birth_year = max(birth_year, na.rm = T))
  

# use filter and grepl to find all trips that either start or end on broadway
trips %>% filter(grepl('Broadway', start_station_name) | 
                   grepl('Broadway', end_station_name)) %>% 
  select(start_station_name, end_station_name)


# do the same, but find all trips that both start and end on broadway
trips %>% 
  filter(grepl('Broadway', start_station_name) & 
           grepl('Broadway',end_station_name)) %>%
  select(start_station_name, end_station_name)
  
# find all unique station names
# Method 1
x <- names(table(trips$start_station_name))
y <- names(table(trips$end_station_name))
z <- names(table(c(x,y)))
z

#Method 2

union(trips$start_station_name, trips$end_station_name)



# count the number of trips by gender, the average trip time by gender, and the standard deviation in trip time by gender
# do this all at once, by using summarize() with multiple arguments
trips %>% 
  group_by(gender) %>% 
  summarise(num_by_gender=n(),
          mean_trip_time = mean(tripduration),
          sd_trip_time = sd(tripduration))


# find the 10 most frequent station-to-station trips
trips$station_to_station <-  paste(trips$start_station_name,trips$end_station_name,sep=' to ') 

trips %>%
  group_by(station_to_station) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:10) #use slice or the following

  #mutate(station_rank = row_number()) %>%
  #filter(station_rank <= 10)

# Method 2:

trips %>% 
  group_by(start_station_name, end_station_name) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  ungroup() %>% 
  slice(1:10)

# find the top 3 end stations for trips starting from each start station
trips %>%
  group_by(start_station_name,end_station_name) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:3)#or if want to see the rank use the following instead
  #mutate(station_rank = row_number()) %>%
  #filter(station_rank <= 3)


# find the top 3 most common station-to-station trips by gender
trips %>%
  group_by(gender,station_to_station) %>%
  summarize(count = n()) %>%
  
  slice(1:3) #or if want to see the rank use the following instead
  #mutate(station_rank = row_number()) %>%
  #filter(station_rank <= 3)


# find the day with the most trips
# tip: first add a column for year/month/day without time of day (use as.Date or floor_date from the lubridate package)

trips %>%
  mutate(trip_day = floor_date(starttime,'day')) %>% 
  group_by(trip_day) %>%
  summarize(count = n()) %>%
  arrange(desc(count))%>%
  slice(1)#or if want to see the rank use the following instead

  #mutate(station_rank = row_number()) %>%
  #filter(station_rank == 1)


# compute the average number of trips taken during each of the 24 hours of the day across the entire month

trips %>% 
  mutate(trip_hour = hour(floor_date(trips$starttime,"hour"))) %>% 
  group_by(trip_hour) %>%
  summarize(count = n(),
            aver_num = count/28) %>% 
  arrange(desc(aver_num))


#Method 2

trips %>% 
  mutate(trip_hour = hour(starttime),
         date = floor_date(starttime,'day')) %>% 
  group_by(date, trip_hour) %>% 
  summarize(count = n()) %>% 
  group_by(trip_hour) %>% 
  summarise(averg = mean(count))


# what time(s) of day tend to be peak hour(s)?

# Answers are slightly different due to various ways to get time of the trip taken place.

#Method 1: Count the total number of trips taken during each of the 24 hours of the day across the entire month
trips %>% 
  mutate(trip_hour = hour(floor_date(trips$starttime,"hour"))) %>% 
  group_by(trip_hour) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% 
  slice(1) # peak hour at round 5:00 pm

# Method 2: Use the computation done in previouse question to decide the peak hour
trips %>%
  mutate(hourTime = hour(round_date(starttime, 'hour'))) %>%
  group_by(hourTime) %>% 
  summarise(count = n(), 
            aver_num = count/28) %>% 
  arrange(desc(aver_num))
  
# Method 3: 

trips %>% 
  mutate(trip_hour = hour(starttime),
         date = floor_date(starttime,'day')) %>% 
  group_by(date, trip_hour) %>% 
  summarize(count = n()) %>% 
  group_by(trip_hour) %>% 
  summarise(averg = mean(count)) %>% 
  arrange(desc(averg)) %>% 
  slice(1)
