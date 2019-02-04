#Nobuhle Mpanza
#01 Feb 2019
#Homework-Section 3
#Due: Tuesday Before 10am

#Loading Libraries
library(tidyverse)
library(scales)
library(ggsn)
library(ggpubr)
library(lubridate)
library(boot)
library(grDevices)

#Loading data
#Assign new name SACTN so you can have it on environ.
SACTN <- read.csv("data/SACTN_day_1.csv")

#Untidy to Tidy data: Lets tidy our dataset;
#Viewed the data to see how it is layed out and noticed that it is not tidy.Must first fix this.
#Our data is not tidy so we must fix the index columns: The index column has two variables within it(src and site)
#Using a function separate to separate the index column into site and src.
#So we assign a new name SACTN_tidy from the original data SACTN


SACTN_tidy <- SACTN %>%
  separate(col = index, into = c("site", "src"), sep = "/ ") +
  

#First remove/delete the untidy dataset/original dataset.
rm(SACTN)

#Then all following functions will be performed using the tidy data SACTN_tidy.

#Then arrange the columns so that set as follows: site,src,date and temp
#Using the select function

SACTN_tidier <- SACTN_tidy %>% 
select(site, src, date, temp)

#Creating a graph to show temperature variation between all sites.
#
ggplot(data = SACTN_tidier,aes(x=date, y= temp)) +
  geom_line(aes(colour = site, group = paste0(site))) +
  labs(x = "", y = "Temperature (°C)", colour = "Site") +
  theme_bw()


# Selecting temperatures recorded at site Port Nolloth during August or September.
#Assign new name the filter site POrt Nolloth(remember R is case sensitive)
# Then filter by date extracting the 8th and 9th months(august and september)

PortN <- SACTN_tidier%>% 
filter(site == "Port Nolloth", month(date) == 08 | month(date) == 09)

#Selecting the monthly temperatures recorded in Port Nolloth during the year 1994.

SACTN_tidier %>% 
filter(site == "Port Nolloth",year(date) == 1994)







