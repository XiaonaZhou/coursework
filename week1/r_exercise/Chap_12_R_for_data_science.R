library(tidyverse)


#Section 12.2.1, exercise 2


# Compute the rate for table2, and table4a + table4b. You will need to perform four operations:
# Extract the number of TB cases per country per year.
# Extract the matching population per country per year.
# Divide cases by population, and multiply by 10000.
# Store back in the appropriate place.
# Which representation is easiest to work with? Which is hardest? Why?

# rate = cases / population * 10000


table2 %>% 
  pivot_wider(names_from = type, values_from = count) %>% 
  mutate(rate = cases / population * 10000)


new_table4a <- table4a %>% pivot_longer( names_to = "year", values_to = "cases", cols = c(`1999`,`2000`))
new_table4b <- table4b %>% pivot_longer( names_to = "year", values_to = "population", cols = c(`1999`,`2000`))

table4 <- inner_join(new_table4a, new_table4b)
table4 %>% 
  mutate(rate = cases / population *10000)


# Section 12.3.3 exercises 1 and 3

# 1. Why are pivot_longer() and pivot_wider() not perfectly symmetrical?
# Carefully consider the following example:

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")

pivot_wider(stocks, names_from = year, values_from = return) 
names_ptype = list(year = double())



# What would happen if you widen this table? Why? 
# How could you add a new column to uniquely identify each value?


people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)


people %>% 
  pivot_wider(names_from = names, values_from = values)


people$num <- c(1,1,2,3,3)

people %>% 
  pivot_wider(names_from = names, values_from = values)

