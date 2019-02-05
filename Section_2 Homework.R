#Nobuhle Mpanza
#01 Feb 2019
#Homework-Section2
#Due: Tuesday Before 10am

#Loading Libraries
library(tidyverse)
library(scales)
library(ggsn)
library(ggpubr)
library(lubridate)
library(grDevices)


#loading data 
#Assigned the name ecklonia so it can appear in environment.
ecklonia <- read.csv("data/ecklonia.csv")

#Exploring the data

#Checking the data strcuture to see what type of data we I have.
glimpse(ecklonia)

#Checking names within columns of all the variables in the dataset
names(ecklonia)

#Checking the first 5 top half as well as 5 bottom half of the dataset.
head(ecklonia)
tail(ecklonia)

#First assign new name
#Use select function to display all variables but exclude ID and digits.
eckl1 <- ecklonia %>% 
  select(-ID, -digits)

# We want only the site Batsata Rock and we assign it into a new name.
Eck_Bats <- ecklonia %>% 
filter(site == "Batsata Rock")


#To show data dimensions
dim(ecklonia)

#Adding colour list in console so i can choose colours from to use in graphs
grDevices::colors()

#Creating different graphs that will answer three stated hypothesis
# Graphs to be plotted are bargraph, line graph and boxplot.

# Graph A Hypothesis 1: an increase in fond length causes an increase in fond length.

#The size of the points must be based on the ID and then grouped by species
#Then changing the legend postion to the bottom use theme(legend.position).
#The geom_smooth function was used to see the relationship between frond length and frond mass.

LineGr <- ggplot(data = ecklonia, aes(x = frond_length, y= frond_mass)) +
  geom_point(aes(size = ID)) +
  geom_smooth(method = "lm")+
  geom_line(aes(group= species, colour= "brown3")) +
  theme(legend.position = "bottom") +
labs(x= "Frond length", y= "Frond mass") +
ggtitle("A")
LineGr
#conclusion: When the frond length of the maxima species increases the diameter also increases.

#Graph B Hypothesis 2: Batsasa Rock has the greatest epiphyte length when compared to Boulders Beach.

#looking at epiphyte length difference between the two sites
#Then fill the boxplot based on the two sites
#Adding labels and title to the graph

Boxpl <- ggplot(data =ecklonia, aes(x = site, y = epiphyte_length)) +
  geom_boxplot(aes(fill = site)) +
  labs(x = "Site", y = "Epiphyte len") +
ggtitle("B")
Boxpl
#conclusion: The distribution of epiphyte length by site showed that in Batsasa Rock the epiphyte length was greater than that of the Boulders Beach.

# Graph C Hypothesis 3: The primary blade width of both sites does not vary among sites. 
#plotting a bar graph we use geom_bar

BarGr <- ggplot(data=ecklonia,aes(x= primary_blade_width)) +
  geom_bar(aes(fill= site), position = "dodge") +
      labs(x = "Primary blwid", y= "Count") +
ggtitle("C")
BarGr
 

#Combining all the above three graphs into one plot.Use ggarrage function.
#Set number of rows and columns
#Then use common.legend to ensure they all have one legend.

ggarrange(LineGr, Boxpl, BarGr,
          ncol = 2, nrow = 2,
          common.legend = TRUE)
  

#Calc. mean,max,min,median and variance for both the stipe_length, stipe_diameter for each of the sites. 
#First assign new name then group by site so i can calc. for each site.
Calc_sites <- ecklonia %>% 
  group_by(site) %>% 
summarise(mean_stip_len= mean(stipe_length),
          min_stip_len=min(stipe_length),
          max_stip_len=max(stipe_length),
          med_stip_len=median(stipe_length),
          var_stip_len=var(stipe_length),
          mean_stip_dia= mean(stipe_diameter),
          min_stip_dia=min(stipe_diameter),
          max_stip_dia=max(stipe_diameter),
          med_stip_dia=median(stipe_diameter),
          var_stip_dia=var(stipe_diameter))


# Calculate standard error
#First we need the varience for stipe length
#n counts the number of values for stipe diameter
#Then cal se for stipe diameter

se1_site <- ecklonia %>%
  group_by(site) %>% 
summarise(var_stlen = var(stipe_length),
          n_bl = n()) %>% 
  mutate(se_stlen = sqrt(var_stlen / n_bl))

#First we need the varience of the stipe diameter 
#n counts the number of values for stipe diameter
#Then cal se for stipe diameter

se2_site <- ecklonia %>%
  group_by(site) %>% 
summarise(var_stdia = var(stipe_diameter),
          n_bl = n()) %>% 
  mutate(se_stlen = sqrt(var_stdia / n_bl))

# Determine the min and maximum frond length and stipe length.
ecklonia %>% 
  summarise(hi_st_len = max(stipe_length),
            lo_st_len =min(stipe_length))

ecklonia %>% 
  summarise(max_frond = max(frond_length),
            min_st_mass =min(frond_length))

# Checking the overall summary of the dataset
summary(ecklonia)
  



