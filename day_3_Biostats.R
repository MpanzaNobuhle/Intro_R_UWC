#Day3 Biostats
#Chapter8 Simple Linear Regressions
#Nobuhle Mpanza


#Linear regression asks if there is a change in slope between two variables.

#load libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)

eruption.lm<-lm(eruptions~waiting, data = faithful) #loading built in data faithful
str(eruption.lm)
summary(eruption.lm) 
#R sqaure value
#pvalue

#Plot a graph/do extra stuff as practise 
ggplot(data = faithful,aes(x = waiting, y = eruptions))+
  geom_point() +
  geom_smooth(method = "lm", colour= "green")+
  labs(x = "Waiting (s)", y ="Eruptions")+
  ggtitle("Waiting period before eruption occurs") +
  annotate("text", x = 45, y = 5,label = paste0("slope ==", slope, "~(min/min)"), parse = TRUE, hjust=0) +
  annotate("text", x = 45, y = 4.75,label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
             annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2),parse = TRUE, hjust=0)


#Chapter 9-Correlations

#loading libraries
library(corrplot)
library(ggpubr)
library(tidyverse)

ecklonia <-read_csv("data/ecklonia.csv") #load data and assign a name to it

cor.test(x = ecklonia$stipe_length, ecklonia$frond_length, 
         use = "everything", 
         method = "pearson")


#assign new name ecklonia_sub from original data then use select function to remove the variables.
ecklonia_sub<-ecklonia %>% 
  select(-species, -site, -ID) #To remove the variables in the brackets

ecklonia_pearson <- cor(ecklonia_sub) 
ecklonia_pearson

ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

#Run the test on the variable
cor.test(ecklonia$length, ecklonia$digits) 


ecklonia_norm <- ecklonia_sub %>% #Assign new name
  gather(key = "variable") %>% #gather collects what you want
  group_by(variable) %>%
  summarise(variable_norm = as.numeric(shapiro.test(value)[2])) #shapiro test tests for normality
ecklonia_norm
#data not nromally distributed so use kendal test below

cor.test(ecklonia$primary_blade_length, 
         ecklonia$primary_blade_width, method = "kendall") #method=kendal test for not normal data.

#read tau value:sample estimates

r_print <- paste0("r = ", 
          round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))


ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") + 
    geom_label(x = 300, y = 240, label = r_print) +
             labs(x = "Stipe length (cm)", y = "Frond length (cm)") + 
               theme_pubclean()

corrplot(ecklonia_pearson, method = "circle") 
#size of dot is the correlation, bigger dot =stronger correlation
#lighter dots and small size=weak coreelation
#no dots means no correlation at all

#produce a heat map using ggplot2-google this and adjust code to your data.
#load library ggplot
library(ggplot2)

# Script complete and runs
# Its good to practice other things in class as well and not just copy from the book
# Good comments



                                                                                       
