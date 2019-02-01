#Day 4
#Nobuhle Mpanza
#01 Feb 2019
#Tidier-Tidy Data 2.0

#Loading Libraries and press control enter to activate them,everytime before you start coding on a new script.
library(tidyverse)
library(lubridate)

#Loading Data,just use load() because it is in .RData set.

load("data/SACTNmonthly_v4.0.RData")

#assign a new name SACTN from the SACTNmonthly_4.0.
SACTN <- SACTNmonthly_v4.0

#Then we use rm to remove the original dataset from which SACTN Was created from.
rm(SACTNmonthly_v4.0)


#Comparison Operator: Some examples include the ff:
  # Greater than: >
  # Greater than or equal to: >=
  #  Less than: <
  # Less than or equal to: <=
  # Equal to: ==
  # Not equal to: !=
#From the SACTN dataset we only want to display or filter out variable site equal to(==) Amanzimtoti.
SACTN %>%
  filter(site == "Amanzimtoti")

#Change site and put a different one to see what happens in your output.
SACTN %>%
  filter(site == "Port Nolloth")


#Logical operators: Some examples include the ff:
#and: &
# or: |
# not: !
#From the SACTN data filter site Pollock Beach and in the date column select month is equal to(==)12 or month equal to(==) 1.
#Question:Find temperature in occuring in December and January?
SACTN %>%
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1)

#try and change the months 
SACTN %>%
  filter(site == "Pollock Beach", month(date) == 05 | month(date) == 2)


#Arrange function:
#Select dataset SACTN
#Arrange function arranges the depth and temp columns from the lowest value to the highest value(ascending order).
SACTN %>%
  arrange(depth, temp)

#Arrange data in descending format/from highest to lowest of temperature column.
SACTN %>%
  arrange(desc(temp))

#From SACTN data filter/show only Humewood from site column and from date column select the year equal to (==) 1990.
SACTN %>%
  filter(site == "Humewood", year(date) == 1990)

#Change the site to a different one and also change year to a different one/extra practise.
SACTN %>%
  filter(site == "Port Nolloth", year(date) == 1994)



# Assign new dataset called try1.Then select the following sites.
try1 <- SACTN %>%
  select(site, src, date, temp)



# Assign new data try2 and Select all columns between site and temp like a sequence
try2 <- SACTN %>%
  select(site:temp)


# Assign new data try3 but now you Select all columns except/except/leave out those stated individually
try3 <- SACTN %>%
  select(-date, -depth)


#Assign new data try 4 and select the ff variables.
try4 <- SACTN %>%
  select(-(date:depth))

#Creating new variables:
#But first assign name try5
#From SACTN data create new column with functin mutate,then identify the name of the column youre mutating eg kelvin,
#Tell R that new column name is temp+273.15.

try5 <- SACTN %>%
  mutate(kelvin = temp + 273.15)

#example question: Create new column with half the temperature.
#Because we already the temp column we just divide by 2 to get 
SACTN %>%
  mutate(kelvin = temp/2)


#Summarise function:calculates the mean,min, max etc and puts them as variables in columns.
#Not the same as summary function which gives you the overview of your dataset.
#na.rm means exclude/leave out the N/A values because they affect your overrall results.
SACTN %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE), #You can just write T instead of TRUE
            sd_temp = sd(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE))

#If you remove na.rm=True what happens?
#Answer: your output is just N/As. That is why we must always remove them.
#ALWAYS REMOVE N/As
SACTN %>%
  summarise(mean_temp = mean(temp),
            sd_temp = sd(temp), 
            min_temp = min(temp),
            max_temp = max(temp))


  







