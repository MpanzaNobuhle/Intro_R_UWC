#exercise2.R
#05 February 2019
#Nobuhle Mpanza


#LOading Libraries
library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(ggplot2)

grDevices::colours() #Loading these colour so i have them close by to choose from.


#loading all the three datasets to be used for exercise2.

load("data/SACTNmonthly_v4.0.RData") #RData file use load
Laminaria <- read.csv("data/laminaria.csv") #csv file use read_csv to load
ToothGrowth <- datasets::ToothGrowth #built-in dataset

#Part 1: Using SACTNmonthly dataset

#Assigg a new name to the original dataset:

SACTN <-  SACTNmonthly_v4.0 


#Then use this newly created dataset to create temp dataset which is used for the seperation of date column into the year.month and day.

temp <- SACTN %>% 
  filter(src=="KZNSB") %>% #We only need the KZNSB from the variable src.
separate(col =date, into =c("year", "month","day"), sep= "-") %>% 
  group_by(site, year) 
#Group the data by site and year.

  
#Then using the above temp dataset,we plot the annual means only for KZNSB.

ggplot(data=temp, aes(x= year, y=temp))+
  geom_line(aes(group= site),colour= "blueviolet") + #Changing the colour lines
  facet_wrap(~site, nrow=9) + #Putting the different sites into 9 rows.
  labs(x = "Year", y= "Temperature (C)") +
  ggtitle("KZNSB: series of annual means") +
  scale_x_discrete(breaks=c(1980,2000),labels=c("1980","2000"))+
scale_y_continuous(breaks=c(24,22,20),labels=c("24","22","20"))

#scale of the x-axis must not be continuous so we use the scale_x_discrete funtion and breaks within the two years(1980 and 2000) so it appears on the x-axis with breaks.
#Scale of the y-axis we use the scale_x_continuous and specify the breaks and labels as it should appear on the graph.
  

#Part 2: Using Laminaria dataset
#Using the laminaria data to recreate the specified graph.
#First assign new name lam from the Laminaria data.
#Then we extract/filter only the region FB(False Bay) because we want only the false bay sites.
#Then we group by sites


lam <- Laminaria %>% 
  filter(region=="FB") %>% 
  group_by(site) 
  

#Then from the lam data we plot the facetted line graph identical to the one in the exercise.
#Plotting the graph with the Roman Rock mistake, we use scale_colour_brewer and pallet accent which will leave out the values for Roman Rock.

plot1 <-   ggplot(data=lam, aes(x=blade_length, y=blade_weight, colour=site))+ #colour by site to have it all the plots coloured differently.
    scale_colour_brewer(palette= "Accent")+
  geom_point()+ #Adding the points.
  geom_line(aes(group=site))+
    labs(x= "Blade length (cm)", y= "Blade mass (kg)")+
  ggtitle("A crazy graph of some data for False Bay sites")+
  facet_wrap(~site, nrow=3) 
plot1
 

#Plot the same graph as above but fixing the Roman Rock mistake of not having the values plotted.
#colour by site to make sure all the plots of the different sites are coloured differently.

 plot2 <-  ggplot(data=lam, aes(x=blade_length, y=blade_weight, colour=site))+ 
    geom_point()+ #Adding the points.
  geom_line(aes(group=site))+
  labs(x= "Blade length (cm)", y= "Blade mass (kg)")+
  ggtitle("A crazy graph of some data for False Bay sites")+
  facet_wrap(~site, nrow=3) 
 plot2
  
  
#Combining the two above plots.
#Use common.legend=TRUE so that the plots share the legend.
 
ggarrange(plot1, plot2,
  labels= c (), common.legend = TRUE)+
  ggtitle("A and B")




#Part 3: Using ToothGrowth dataset
#Then group by supp and dose
#Calc the mean and standard deviation of the length.

#Assigning a new name toothgrowth from the original dataset

toothgrowth <- ToothGrowth %>% 
group_by(supp,dose) %>% 
summarise(mean_len =mean(len),
          sd_len =sd(len))

#Then using the newly assgined data toothgrowth, plot the graph with sd using an errorbar.
ToothGr <- ggplot(data= toothgrowth, aes(x=dose, y=mean_len, fill=supp)) +
  geom_col(aes(fill=supp),position="dodge", colour="black") + #columns must be filled by site and use position dodge to have them next to each other.Then colour=blacks to have the columns outlined black.
  geom_errorbar(aes(ymin =mean_len-sd_len,
                    ymax =mean_len+sd_len), position="dodge")+ #Aligning the errorbars position
labs(x= "Dose (mg/d)", y= "Tooth length (mm)") #Labelling the axes
 ToothGr
 


