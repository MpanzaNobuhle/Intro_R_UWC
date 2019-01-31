#Day 3
#Nobuhle Mpaza
#Mapping with style
#31 January 2019

#Loading libraries
library(tidyverse)
library(scales)
library(ggsn)

#Load Africa map
load("data/africa_map.RData")

#Creating a world map
#borders gives you outline of area
ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat

#sa_1 means we assign a name and have it in environ.
#we didnt use any dataset but we used a built in dataset
#coord-equal means we want south africa only and we state those within the coord-equal function.
sa_1 <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(12, 36), ylim = c(-38, -22), expand = 0) # Force lon/lat extent
sa_1

# We can change colour that is specified.
sa_2 <- sa_1 +
  annotate("text", label = "Atlantic\nOcean",
           x = 15.1, y = -32.0,
           size = 5.0,
           angle = 30,
           colour = "navy") +
  annotate("text", label = "Indian\nOcean",
           x = 33.2, y = -34.2,
           size = 5.0,
           angle = 330,
           colour = "springgreen")
sa_2

#Refer here for mapping codes of our coastline maps
#All maps must have scale bars,make sure every map has it before even having it in your project.
# scale bar x and y values change position of the scale bar.Height changes size.
sa_3 <- sa_2 +
  scalebar(x.min = 32, x.max = 26, y.min = -34, y.max = -35, # Set location of bar
           dist = 200, height = 4, st.dist = 0.8, st.size = 4, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 22.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
        scale = 1.2, symbol = 16)
sa_3

#Insetting maps
#When insetting data on your excel keep it as points and not as commas,R cannot read comms.
africa_map

sa_4 <- sa_3 +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 20.9, xmax = 26.9,
                    ymin = -30, ymax = -24)


sa_4



