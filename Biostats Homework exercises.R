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


#Using probability data
bees <- matrix(c(70,85,50,35), ncol=2)
colnames(bees) <- c("yes", "no")
rownames(bees) <-c("managed", "wild")

#null hyp- Probability of mating in both wild and managed bees is not different
#Alt hyp- There is a higher probability of mating in managed bees than wild bees
prop.test(bees)
#pvalue>0.05

#CHAPTER7

#Exercise 7.4.1
feed_A <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_B <- c(68.7,67.7, 74.0, 66.3, 69.8)
feed_C <- c(102.6, 102.1, 100.2, 96.5)
feed_D <- c(87.9, 84.2, 83.1, 85.7, 90.3)

bacon <- as.tibble(data.frame(
  feed=c(
  rep("Feed 1", length(feed_A)),
  rep("Feed 2", length(feed_B)),
  rep("Feed 3", length(feed_C)),
  rep("Feed 4", length(feed_D)),
  mass=c(feed_A, feed_B, feed_C, feed_D))))
  
  #null hyp-feed type has no effect on pig mass.
  #alt hyp-feed type has an effect on pig mass.
  
  mass<- compare_means(mass~feed, data=bacon, method="t.test")
  bacon.aov1 <- aov(mass~feed, data=bacon)
  summary(bacon.aov1)
  
  #7.4.2: Construct suitable null and alternative hypotheses,test your hypothesis using an ANOVA.
  
  #loading built in data
  teeth <- datasets::ToothGrowth
  
  #Constructing hypothesis
  #Null-There is no difference in tooth length with respect to the dose levels of vitamin C.
  #Alternative- An increase in dose level will cause an increase in teeth length.
  
  #Running the anova test
  len_teeth <- aov(len ~ supp, data =filter(Tooth,dose %in% c(0.5, 1, 2))) 
  summary(len_teeth)

#7.4.3
oranges<-datasets::Orange

oranges$Tree=as.factor(oranges$Tree)
#Null HYp-Age does not determine growth of the tree circumference
#Alt Hyp- Age determines growth of the tree ciicumference.

oranges.aov1 <- aov(age~circumference, data=filter(oranges, Tree %in% c(1,2,3,4,5)))
summary(oranges.aov1)

#pvalue for all masses >0.05, therefore we reject null hyp and feed type has an effect on pig mass.

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


