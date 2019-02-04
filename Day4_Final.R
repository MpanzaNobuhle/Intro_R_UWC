#Day 4
#Nobuhle Mpanza
#01 Feb 2019
# Chapter12-Final Chapter

#Clean enviro and console

#Loading Libraries
library(tidyverse)

#Loading Data
load("data/SACTNmonthly_v4.0.RData")

#Assign SACTN from the loaded data.
SACTN <- SACTNmonthly_v4.0

#Remove the original dataset once youve assigned the new SACTN dataset, just to ensure uour enviro. stayes clean.
rm(SACTNmonthly_v4.0)


# From the SACT data,assign/create SACTN_depth_mean.
#Use group_by function and group depth, then you summarise(calc. mean_temp from temp variable excluding N/A values).
# Then 
SACTN_depth_mean <- SACTN %>%
  group_by(depth) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            count = n())
#Display table in console         
SACTN_depth_mean

#Plot a 
ggplot(data = SACTN_depth_mean, mapping = aes(x = depth, y = mean_temp)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)+
ggtitle("A")+ #Add title to the graph
 labs(X="depth", y="mean_temp") #Add labels

#Assign new name then group_by source and source then filter only those values that are greater than 360.
SACTN_30_years <- SACTN %>%
  group_by(site, src) %>%
  filter(n() > 360)

#Create a set of inputts you want run to look at,use c function.
 selected_sites <- c("Paternoster", "Oudekraal", "Muizenberg", "Humewood")
 
#Calc the mean and sd using the summarise function of these 
# Filter only the sites selected or filtered in above funtion.R focuses on the above selected sites.
#group_by site and source then summarise to calc by first giving a name eg. mean_temp(column name).
SACTN %>%
filter(site %in% selected_sites) %>%
        group_by(site, src) %>%
        summarise(mean_temp = mean(temp, na.rm = TRUE),
        sd_temp = sd(temp, na.rm = TRUE))

#If we have one site,we run the ff code.
SACTN <- 
Filter(site == "Port Nolloth")

# [A.A]
# Script runs
# Neat script
# Sufficient comments
# Nicely done