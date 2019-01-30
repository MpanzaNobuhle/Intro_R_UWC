#Day 2
#Plotting in R using ggplot2
#Nobuhle Mpanza
#30 January 2019

#Load Libraries

library(tidyverse)
chicks <- datasets::ChickWeight
??ChickWeight

#The function ?? gives you a description of what you're working with

ggplot(data = chicks, aes(x = Time, y = weight)) +
  geom_point() +
  geom_line(aes(group = Chick))

#The function above was to plot a graph using the chick data with time on the x-axis and weight on the y-axis.
#ggplot is a function within tidyverse package,aes means astetics,controls everything
#geom_point means make scatterplot and for line and histogram you say geom_line or geom_histo.
#Plus sign works as a pipe in plotting but %>% works as a pipe in coding.
#Group = Chick means group by Chick,case sensitive. So make you keep your data as is in he dataset.

ggplot(chicks, aes( x = Time, y = weight, colour = Diet))+
  geom_point()+
  geom_line(aes(group=Chick))

#Colour in the first code means each diet must have different colour on the graph.

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm")

#geom_smooth creates a smooth line.
#lm means linear models 
#Diet 3 most effective diet as it started at low weights but went constant up.

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(colour="blue") +
  geom_line(aes(group=Chick))

# We specified the colour of the points.

ggplot(chicks, aes(x=Time, y=weight, colour=Diet))+
  geom_point(aes(size=weight))+
  geom_smooth(method="lm") +
  labs(x = "Days", y = "Weight (kg)") +
  ggtitle("Port Nolloth")+
  theme_bw()

#Points must present a different size as weights,thats why size=weight.Heavier weight means larger point size.
#aes controls output of your funtion.
#Labs means we are changing the labels of the y and x axis.

#Loading Chickweight Data and practice alone what we have just finished doing
#Loading Libraries
library(tidyverse)
chicks_2<- datasets::ChickWeight
ggplot(aes(x=Time, y=weight, colour=Diet))+
  geom_point()+
  geom_line(aes(group=Chick))


#Faceting in ggplot
library(ggpubr)

ggplot(chicks, aes(x = Time, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm") +
facet_wrap(~Diet, ncol = 4)
facet_wrap(~Diet, nrow = 2)


#Facet wrap functin splits the diet into different panes, and ncol splits into different columns,specifies the number of columns.
#nrow specify numberof rows.

chicks_2<-chicks %>% 
filter(Time == 21)
#Ask for help here

plot_1 <- ggplot(chicks, aes(x = Time, y = weight, colour = Diet))+
  geom_point()+
  geom_line(aes(group = Chick))+
  labs(x ="Days", y = "weight")+
  ggtitle("A")
plot_1

#Assign a name for the graph and move it to the environments.
#We are saving the graph in the environment.

plot_2 <- ggplot(chicks, aes(x = Time, y = weight, colour = Diet))+
  geom_point()+
  geom_smooth(method="lm")+
ggtitle("B")
plot_2


plot_3 <- ggplot(data=chicks_2, aes(x=weight))+
  geom_histogram(aes(fill=Diet), position="dodge", binwidth=100)+
  labs(x = "Final Mass (g)", y = "Count")
plot_3
#NB:position =dodge places your histogtram along side each other and not on top of each other.


plot_4 <-ggplot(data=chicks_2, aes(x=Diet, y=weight))+
  geom_boxplot(aes(fill=Diet))+
  labs(x="Diet", y="Final mass (g)")
plot_4

plot_combined <- ggarrange(plot_1,plot_2, plot_3, plot_4)
plot_combined
#ggarrange combines all the plots you've plotted.


#3rd Library
library(boot)
urine <- boot::urine
??urine
urine %>% 
  select(-cond)
#To remove cond from the variables you use select and then (start with minus brfore cond).

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = cond))
#aes controls the function 

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = as.factor(r)))
#as. factor means 0 or 1.


