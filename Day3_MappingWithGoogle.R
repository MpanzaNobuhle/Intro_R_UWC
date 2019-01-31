#Day 3-Afternoon Session
#Mapping with google
#Nobuhle Mpanza
#31 January 2019



# Loading libraries(do this first before you do anything-always)
library(tidyverse)
library(ggmap)

# Loading data, so we can have it in our environment.
# When loading a RData file we use load,
#When loading csv file we use read_csv,
load("data/cape_point_sites.RData")


#Issues with satellite directory
cape_point <- get_map(location = c(lon = 18.36519, lat = -34.2352581),
                      zoom = 10, maptype = 'satellite')
# load("data/cape_point.RData")

cp_1 <- ggmap(cape_point) +
  geom_point(data = cape_point_sites, aes(x = lon+0.002, y = lat-0.007),
             colour = "red", size = 2.5) +
  labs(x = "", y = "")
cp_1


#Site Labels
#geom_text adds labels to the graphs/maps itself not on the x and y axis.
cp_2 <- cp_1 +
  geom_text(data = cape_point_sites, # Choose dataframe
            aes(lon+0.002, lat-0.007, label = site), # Set aesthetics
            hjust = 1.15, vjust = 0.5, # Adjust vertical and horizontal
            size = 3, colour = "white") # Adjust appearance
cp_2


