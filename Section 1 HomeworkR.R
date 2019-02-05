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
#Only just plotting the outlineof the path.

feb1 <-  ggplot(data =rast_feb, aes(x = lon,y = lat)) +
  geom_point(colour ="blue2")
feb1


aug1 <-  ggplot(data =rast_aug, aes(x = lon,y = lat)) +
  geom_point(colour ="salmon")
aug1

#Creating my own colour pallete using the link
colPal <- c("#75E1FA","#5CB9CC","#4592A0","#316D76",
            "#1E4A4F","#0D292C","#5389A0","#49748A",
            "#3F6174","#344E5F","#293D4B","#1E2C37")

#Colour palette from class (refer to this one to create my own,NB:must have atleast 8 values/codes)
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")

#Listing all colours available so i can choose any i want. 
grDevices::colors() #function not taught in class.

#Load the sa provinces data that will be used to specify the path within the polygone
load("data/sa_provinces.RData") 



#Adding the polygone and specifiying borders path with the polygone based on the sa_provinces data.
#Making use of the colour pallete I created on both maps.
#Used ggtitle to give the map a title on top.
#From the simple graph above which just shows only the points of along the coast,we start to add all the other variables,
#So it can appear with the polygone and temperature


#For February
feb2 <- ggplot(data =rast_feb, aes(x = lon,y = lat))+
  geom_raster(data = rast_feb, aes(fill = bins)) +
  geom_polygon(colour = "blue2", fill = "grey56")+
  geom_tile(data = rast_feb, aes(x = lon, y = lat, fill = bins),
            colour = "blue2", size = 0.3) +
  geom_raster(data = rast_feb, aes(fill = bins))+
  geom_path(data=sa_provinces, aes(group = group))+
  scale_fill_manual("Temp. (°C)", values = colPal)+
  coord_equal(xlim = c(15, 34), ylim = c(-36, -29), expand = 0) +
  ggtitle("February temperature")
feb2




#For August
aug2 <- ggplot(data =rast_aug, aes(x = lon,y = lat))+
  geom_raster(data = rast_feb, aes(fill = bins)) +
  ggtitle("August temperature")+
  geom_polygon(colour = "salmon", fill = "grey56")+
  geom_tile(data = rast_feb, aes(x = lon, y = lat, fill = bins),
            colour = "salmon", size = 0.3) +
  geom_path(data=sa_provinces, aes(group = group))+
  scale_fill_manual("Temp. (°C)", values = colPal)+
  coord_equal(xlim = c(15, 34), ylim = c(-36, -29), expand = 0) 
aug2



#Adding oceans names (Atlanic and indian ocean) on the map then increase the size of the labels.
#Using the function annotate to add the names of the two oceans and used /n to have the name "ocean" below the name Atlantic or Indian.

#For February
feb3 <- feb2+
  annotate("text", label = "Atlantic\nOcean",
           x = 17.0, y = -34.0,
           size = 6.0,
           angle = 35,
           colour = "tan2") +
  annotate("text", label = "Indian\nOcean",
           x = 30.0, y = -34.2,
           size = 6.0,
           angle = 330,
           colour = "violetred2")
feb3



#For August
aug3 <- aug2+
  annotate("text", label = "Atlantic\nOcean",
           x = 17.0, y = -34.0,
           size = 5.0,
           angle = 35,
           colour = "tan2") +
  annotate("text", label = "Indian\nOcean",
           x = 30.0, y = -34.2,
           size = 5.0,
           angle = 330,
           colour = "violetred2")
aug3




#Inserting the Scale Bar

#February
feb4 <- feb3+
  scalebar(x.min = 22, x.max = 26, y.min = -35, y.max = -34,
           dist = 200, height = 1, st.dist = 0.8, st.size = 3,
           dd2km = TRUE, model = "WGS84") + 
  north(x.min = 22.5, x.max = 25.5, y.min = -35, y.max = -32,
        scale = 1.0, symbol = 16)
feb4

#August
aug4 <- aug3+
  scalebar(x.min = 22, x.max = 26, y.min = -35, y.max = -34,
           dist = 200, height = 1, st.dist = 0.8, st.size = 3,
           dd2km = TRUE, model = "WGS84") + 
  north(x.min = 22.5, x.max = 25.5, y.min = -35, y.max = -32,
        scale = 1.0, symbol = 16)
aug4




#Inserting smaller map inside my map:
#First I load  the african map data. Use the load() fucntion because its already in the RData format.
#Then from that newly loaded data i can insert the african map within  my map.
load("data/africa_map.RData")


#February
feb5 <- feb4+
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 20.9, xmax = 26.9,
                    ymin = -32, ymax = -29)
feb5


#August
aug5 <- aug4+
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 20.9, xmax = 26.9,
                    ymin = -32, ymax = -29)
aug5




#Changing the position of the scale bar
feb6 <- feb5+
  scale_x_continuous(breaks = seq(16, 32, 4),
                     labels = scales::unit_format("°E", sep = ""),
                     position = "bottom") + 
  scale_y_continuous(breaks = seq(-36, -24, 4), 
                     labels = c("36.0°S", "32.0°S", "28.0°S", "24.0°S"),
                     position = "right") + 
  labs(x = "", y = "") 
feb6   

#The last step can be repeated for august.
#Ask Amieroh why temperature doesnt fill the ocean and show variation ?























