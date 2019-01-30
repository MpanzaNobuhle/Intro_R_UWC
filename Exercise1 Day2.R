#Day 2
#NP Mpanza
#30 Janaury 2019
#Class Exercise 1

#Select any 3 of the built-in datasets and create two graphs for each. Calculate mean of one column in that dataset.
#For each of the datasets selected, 2 graphs were created and both graphs were used to answer one hypothesis.

#Loading Libraries
library(tidyverse)
library(ggpubr)

cars <- datasets::cars
??cars

#Part 1:Using the cars dataset two graphs were created to answer the hypothesis.
#Hypothesis1: Cars travelling at higher speed covers the longest distance.

ggplot(data = cars, aes(x=speed, y=dist))+
  geom_point(aes(colour ="red"))+
  geom_line()+
  geom_smooth(method = "lm")+
  ggtitle("Distance travelled at various speed")

ggplot(data = cars, aes(x=speed, y=dist))+
  geom_boxplot(aes(group=speed))+
  labs(x="speed", y="distance")
#Conclusion of the above hypothesis: At lower speed the distance travelled is shorter and starts to increase when the speed increases.

#Calculating the mean of speed for cars.
cars %>% 
  summarise(aveg_speed = mean(speed))
#Calculating the mean of distance for cars.
cars %>% 
  summarise(aveg_distance = mean(dist))


library(tidyverse)

#Part 2:Using the orange data.
Orange <- Orange
#Hypothesis 1: The circumeference of the older trees is bigger than that of the young ones.
??orange

ggplot(Orange, aes(x=age, y=circumference, colour=Tree))+
  geom_point()+
  geom_line(aes(group=Tree))
#Plotting the datase orange and grouping  it based on tree number.
library(boot)

ggplot(Orange,aes(x = age, y = circumference))+
  geom_point(aes(colour = Tree))
#Showing the circumeference of the tree in relation to age while looking at each Tree. Tree 4 showed to have the biggest circumeference in comparison to 1,2 and 3.
          
#conclusion: From the graph plotted it can be concluded that younger trees have smaller circumferences compared to older trees.


#Calculating the mean of age.
Orange%>% 
  summarise(aveg_age=mean(age))
#Calculating the mean of circumference.
Orange%>% 
  summarise(aveg_circumference=mean(circumference))




#Part 3:Using the chickwts data to draw graphs and interpret dataset.
#Hypothesis: The chicks fed with sunflower gained more weight than the rest of the chicks fed other feed.
chick3 <-  chickwts
chick3 datasets:: chickwts
??chickwts

ggplot(data = chickwts, aes(x = feed, y = weight)) +
  geom_point() +
  geom_line(aes(group = feed))
#Plotting the weight in relation to feed to see which feed is more effective in chick growth.

ggplot(chickwts, aes(x=feed, y=weight, colour=weight))+
  geom_point(aes(size=weight))+
  geom_smooth(method="lm") +
  labs(x = "feed", y = "weight (kg)") +
  ggtitle("Effect of various feeds on chick weight")+
  theme_bw()
#Plot to show the weight of the chicks when fed different feeds.

#conclusion: The hypothesis was accepted as the highest weight was found to be of the chicks fed with sunflower.

#Calculating the mean,max and min of weight of the chicks. And arranging it in three colmuns.
chickwts %>% 
  summarise(aveg_weight=mean(weight),
            max_weight=max(weight),
            min_weight=min(weight))


#Built in datasets used were:
#cars
#chickswts
#orange

#A total of 6 graphs have been plotted.
#The means in all the datasets have been calculated.




