#!/bin/bash
#
# add your solution after each of the 10 comments below
#

# count the number of unique stations
echo '1. count the number of unique stations'
cut -d, -f5,9 201402-citibike-tripdata.csv | sort | uniq | wc -l

# count the number of unique bikes
echo '2. count the number of unique bikes'
cut -d, -f12 201402-citibike-tripdata.csv |sort | uniq |  wc -l

# count the number of trips per day
echo '3. count the number of trips per day'
cut -d, -f2 201402-citibike-tripdata.csv | cut -d ' ' -f1 |sort | uniq -c
# find the day with the most rides
echo '4. find the day with the most rides'
cut -d, -f2 201402-citibike-tripdata.csv | cut -d ' ' -f1 |sort | uniq -c | sort -nr | head -n1

# find the day with the fewest rides
echo '5. find the day with the fewest rides'
cut -d, -f2 201402-citibike-tripdata.csv | cut -d ' ' -f1 |sort | uniq -c | sort -n | head -n2

# find the id of the bike with the most rides
echo '6. find the id of the bike with the most rides'
cut -d, -f12 201402-citibike-tripdata.csv |sort | uniq -c|sort -nr|head -n1

# count the number of rides by gender and birth year
echo '7. count the number of rides by gender and birth year'
cut -d, -f14,15 201402-citibike-tripdata.csv |sort | uniq -c
echo '7. count the number of rides by birth year'
cut -d, -f14 201402-citibike-tripdata.csv |sort | uniq -c |sort -n
echo '7. count the number of rides by gender'

cut -d, -f15 201402-citibike-tripdata.csv |sort | uniq -c |sort -n


# count the number of trips that start on cross streets that both contain numbers (e.g., "1 Ave & E 15 St", "E 39 St & 2 Ave", ...)
echo '8. count the number of trips that start on cross streets that both contain numbers'
cut -d, -f5 201402-citibike-tripdata.csv | grep '.*[0-9].* & .*[0-9].*'| wc -l

# compute the average trip duration
echo '9. compute the average trip duration'
cut -d, -f1 201402-citibike-tripdata.csv | tr -d '"' | awk '{s+=$1} END{print s/NR}'

