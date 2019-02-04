#Day 3
#Nobuhle Mpanza
#Mapping with ggplot
#31 January 2019

#Loading libraries
library(tidyverse)
library(ggpubr)

#To load data into R

load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
load("data/MUR.RData")
load("data/MUR_low_res.RData")


#Use low resolution data to prevent computers from crashing.
#sst stands for sea surface temperature collected using satellites.
#We are changing MUR_low_res to sst name in environment.
sst <- MUR_low_res



# Create colour pallette in R we use c
#We are working with sea surface temperature so we working with the ocean,so its shades of blue we will use.
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")


#Assign name to the data youre using, in this case chickweight so it appears in environm.
#Plot graph from chickweight data
ChickWeight <- datasets::ChickWeight
ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_point()


#Plot using below data,already loaded and in environment,so no need to assign a name again.
#Plotting a path.
#labs names and controls x and y axis.
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_point()+
labs(x="longtde", y="lat")+
ggtitle("Path")

#Plotting polygon graph,fill fills the plot with colour eg grey70 or blue,colour outlines/borders the path
#group is a column in the dataset.
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "red", fill = "blue", aes(group = group))

#NB: Because we are in South Africa, we can just run this same code for our thesis to show the SA coast.
#NB:longitude is always on x-axis and latitude on y-axis
#geom_polygon gives us our outline and controls the colour within that outline.
#First line is parent line,
#geom_path gives us the different sites within outline,gives us different paths
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group))


#coord_equal function indicates minimum x and y values.expand means expand or dont expand 
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)


#We are adding a new function called geom_raster looking at the dataset sst,
#see sst datase=bins says which temp will be put in one range
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)


# We are now making use of our pallette/colour created in the beginning.
#scale_fill_manual allows us to specify our colour which we created in the beginning.We can give our scale a name eg."Temp (c)" and also value from cols11 which we created earlier.
#coord_equal sets limits for your x and y.
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) + # Set the colour palette
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

#
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins),
            colour = "white", size = 0.1) + # The coastal temperature values
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)


#Final Map if first it does'st show up, run the code from the middle then run again at the end when no errors occur.
#final_map at beginning means save the graph in enviro, so do not run where you are assigning the name,just read from ggplot.
final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins),
            colour = "white", size = 0.1) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 7), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(0.55, 0.4)) # Fine tune position of legend
final_map

#Refer to this file when doing my project and also consult Amieroh, shes cool and willing to help only if im willing to learn.

# [A.A]
# Nice and neat script
# Good commenting throughout the script
# Try new things, this will only increase you marks and help with yout understading
# Script runs


