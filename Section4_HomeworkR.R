#Nobuhle Mpanza
#01 Feb 2019
#Homework-Section 4
#Due: Tuesday Before 10am

#Loading Libraries
library(tidyverse)
library(scales)
library(ggsn)
library(ggpubr)
library(boot)


#Loading Data
Women1 <- datasets::women
Rocky<- datasets::rock
#View the data individually to get familiar with the variables before i start coding.

#Analyse each dataset individually and make use of the :summarise, select, group_by.

#calc the highest and lowest weight of the women.
Women1 %>% 
summarise(max_weight = mean(weight),
          min_weight = min(weight))

#Select only the two variables area and shape
Rocky %>% 
select(area,shape)

#Another way to only select area and shape as above is the ff code:
Rocky %>% 
select(-peri,-perm)

#Using group_by
Rocky %>% 
  group_by(shape)

#Visualizations not done in the R intro workshop

#Using geom_jitter then adding geom_line to connect he jitter points,then able to read the pattern of the data.


Rocky %>% 
ggplot(aes(x=area, y=shape)) +
  geom_jitter()+
  geom_line()

#Using the women1 data,geom_bin2d function creates a pattern based on the specified data and then adding geom_smooth makes it easy to read that patten.
Women1 %>% 
ggplot(aes(x= height, y= weight))+
  geom_bin2d()+
    geom_smooth(method = "lm")

#using crossbar
Women1 %>% 
  ggplot(aes(x= height, y= weight))+
  geom_crossbar(ymin=115, ymax=155)


#Using geom_violin
Rocky %>% 
ggplot(aes(x= peri, y= perm)) +
geom_violin()


  
