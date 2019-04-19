#Biostats Exercises
#Due Friday 19 April 2019
#Nobuhle Mpanza 3951932

#Loading libraries
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(plotly)



#Exercise 6.7.1: Create own normally distributed data and create hypothesis to test

r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1), #specify sample size, mean and sd.
                            rnorm(n = 20, mean = 5, sd = 1)), 
                    sample = c(rep("A", 20), rep("B", 20)))

#Hypothesis for the above data: There is no difference between the mean of A and B

#Running the t test
t.test(dat ~ sample, data = r_two, var.equal = TRUE) 
#t = -3.2719, df = 38, p-value = 0.002278; therefore p<0.05 reject null hypothesis, there is a difference between sample A and B.


ownNormal <- rnorm(n=200, mean=3, sd=2) #shortcut to creating normal data, one sample
#visualise data using histogram
hist(ownNormal, main = "Normal DIstribution") #draw histogram to visualise my ownNormal data

t.test(ownNormal) #t test on my OwnData



#CHAPTER7
#7.4: Construct suitable null and alternative hypotheses,test your hypothesis using an ANOVA.

#loading built in data
teeth <- datasets::ToothGrowth

#Constructing hypothesis
#Null-There is no difference in tooth length with respect to the dose levels of vitamin C.
#Alternative- An increase in dose level will cause an increase in teeth length.


#Running the anova test
len_teeth <- aov(len ~ dose, data = teeth) 
summary(len_teeth)

TukeyHSD(len_teeth)

#Visualising the teeth data.
ggplot(data = teeth, aes(x = dose, y = len)) + 
  geom_boxplot(aes(fill = dose, aes(group=supp)), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)




# CHAPTER 9- 9.6.1: Producing a heat map 
# Load libraries 
library(tidyverse) 
library(ggpubr) 
library(corrplot)
library(ggplot2)
library(reshape2)


#loading data from external source-the nba data
nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv", sep=",")

nba <- nba[order(nba$PTS),]

row.names(nba) <- nba$Name
nba <- nba[,2:20]
nba_matrix <- data.matrix(nba)
nba_heatmap <- heatmap(nba_matrix, Rowv=NA, 
     Colv=NA,
    col = heat.colors(256), 
scale="column", margins=c(0,8)) #specify height and width of the figure.

                        #OR
#We can plot a heatmap as follows:using ggplot2 and using external data

name_genes <- paste(rep("GEN", 20), 
                   LETTERS[1:20], sep="_") 
name_patients <- paste(rep("PATIENT", 20),  #specify number of patients
                       seq(1,20,1), sep="_")

value_expression <- data.frame(genes = name_genes, 
       matrix(rnorm(400, 2, 1.8),
              nrow = 20, ncol = 20))

names(value_expression)[2:21] <- name_patients

df_heatmap <- melt(value_expression, id.vars = "genes")
names(df_heatmap)[2:3] <- c("patient", "expression_level")

#Plotting the heat map using ggplot
ggplot(df_heatmap, aes(patient, genes )) +
  geom_tile(aes(fill = expression_level), color = "white") +
  scale_fill_gradient(low = "black", high = "red") +
  ylab("List of genes ") +
  xlab("List of patients") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Expression level")




#Extra practise of things not taught in class.
library(RmarineHeatWaves) #new package RmarineHeatWaves
library(ggplot2)
library(ggpubr)

#Random extra work for fun

#rnorm is a function that creates random numbers
x <- rnorm(1000)
#creating random numbers and assign name X
h <- hist(x, breaks=100, plot=FALSE) #drawing histogram with the above numbers
plot(h, col=ifelse(abs(h$breaks) < 1.5, 4, 2))

shapiro.test(x) #check normality of the data
#pvalue =0.04696 (pvalue < 0.05) the data is not normally distributed

dnorm(-1.96) #dnorm generates density probability
x <- seq(0, 4, 0.1)
plot(x, dnorm(x, 2, 0.5), type = "l") #plotting density prop curve.


