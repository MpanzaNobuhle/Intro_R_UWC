#Nobuhle Mpanza
#01 Feb 2019
#Homework-Section 1
#Due: Tuesday Before 10am

#Loading  all libraries that i will make use of their functions.
library(tidyverse)
library(scales)
library(ggsn)
library(ggpubr)
library(lubridate)
library(grDevices)

#Loading data (rast_feb and rast_aug)
#Because both datasets are RData files we will use the function load().

load("data/rast_feb.RData")
load("data/rast_aug.RData")

#Exploring both my datasets individually:
#Summary function to get the overview of the rast_feb and rast_aug dataset.
#Using rast_feb dataset to explore the top half and bottom half of the data.
summary(rast_feb) 
head(rast_feb)  
tail(rast_feb)

summary(rast_aug)
head(rast_aug)
tail(rast_aug)

#Then count the number of rows. Pipe after to add other codes 
#Use the head and tail functions to display only the first 4 top and 5 bottom lines of the dataset.
#Select the temp

nrow(rast_feb) 
head(rast_feb, n =4)  
tail(rast_feb, n =5)

#Display 3 top and 2 bottom list of the data.
nrow(rast_aug)
head(rast_aug,n= 3)
tail(rast_aug, n =2)

#Display only the season and temperature and this assigned a new name and be imn enviro.
#So you assign a new name first
seas_temp  <- rast_feb %>%
  select(season, temp)

#Assign new name then display all variables except the index variable
aug_new <- rast_aug %>% 
  select(-index)


#Then display the highest and lowest temperature.
max_temp <-  rast_feb %>%
  select(season, temp) %>% 
  filter(temp == max(temp))

#Display the lowest temperature for august
min_temp <- rast_aug %>% 
  select(season, temp) %>% 
  filter(temp ==min(temp)) 

#Using glimpse() and str() to preview our data and see howe console.
glimpse(rast_aug)
str(rast_aug) #new function
dim(rast_feb) #looking at the data dimensions

#Calculations
rast_aug %>% 
  summarise(temp = mean(temp),
            sd_temp =sd(temp),
            var_temp =var(temp))



# Plotting/Putting the points for both datasets using the lon and lat variables.Showing only the 

  ggplot(data =rast_aug, aes(x = lon,y = lat)) +
           geom_point(colour ="grey") 
  
  
  ggplot(data =rast_feb, aes(x = lon,y = lat)) +
      geom_point(colour ="blue2")
  
 
#Creating own colour pallete using the link
  colPal <- c("#75E1FA,#5CB9CC,#4592A0,#316D76,
              #1E4A4F,#0D292C#5389A0,#49748A,
              #3F6174,#344E5F,#293D4B,#1E2C37")

#Listing all colours available so i can choose any i want. 
grDevices::colors() #function not taught in class.

load("data/sa_provinces.RData")


#Making use of the colour pallete I created on both maps. 
  ggplot(data =rast_aug, aes(x = lon,y = lat)) +
    geom_point(colour ="grey") +
    geom_polygon(colour = "black", fill = "grey70", aes(group = index))+
    geom_path(data = rast_aug, group= index)
    labs(x ="Longitude", y ="Latitude") +
    ggtitle("Aug_Map") +
    geom_raster(data = rast_aug, aes(fill = bins)) +
  scale_fill_manual("Temp. (°C)", values = colPal)
  
  ggplot(data =rast_feb, aes(x = lon,y = lat)) +
    geom_point(colour ="blue2") +
    labs(x ="Longitude", y ="Latitude") +
    ggtitle("Aug_Map") +
    geom_raster(data = rast_feb, aes(fill = bins)) +
    scale_fill_manual("Temp. (°C)", values = colPal)
  
  
  
#Adding labels and title on both graphs.
  ggplot(data =rast_aug, aes(x = lon,y = lat)) +
    geom_point(colour ="grey") +
    labs(x ="Longitude", y ="Latitude") +
    ggtitle("Aug_Map") 
  
  
  ggplot(data =rast_feb, aes(x = lon, y = lat)) +
    geom_point(colour ="blue2") +
    geom_path(data = sa_provinces, aes(group = group))+
  labs(x ="Longtde", y ="Latd") +
    ggtitle("Feb-Map")
 
   
#Adding oceans names (Atlanic and indian ocean) on the map then increase the size of the labels.
  
  ggplot(data =rast_aug, aes(x = lon,y = lat)) +
    geom_point(colour ="grey") +
    labs(x ="Longitude", y ="Latitude", size =) +
    ggtitle("Aug_Map") +
    geom_raster(data = rast_aug, aes(fill = bins)) +
    geom_polygon(colour = "ivory2", fill = "grey75")+
    scale_fill_manual("Temp. (°C)", values = colPal)
  
  #Code to add ocean names
    annotate("text", label = "Atlantic\nOcean",
             x = 15.1, y = -34.0,
             size = 4.0,
             angle = 35,
             colour = "navy") +
    annotate("text", label = "Indian\nOcean",
             x = 33.2, y = -34.2,
             size = 5.0,
             angle = 330,
             colour = "springgreen")
    
  ggplot(data =rast_feb, aes(x = lon,y = lat)) +
    geom_point(colour ="grey") +
    labs(x ="Longitude", y ="Latitude") +
    ggtitle("Aug_Map") +
    geom_raster(data = rast_feb, aes(fill = bins)) +
    geom_polygon(colour = "turquoise", fill = "grey50")+
    scale_fill_manual("Temp. (°C)", values = colPal) 
    
    
#Scale Bar
  scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
           dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance
    north(x.min = 22.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
          scale = 1.2, symbol = 16)
  
#Inserting smaller map inside my map:
  

       