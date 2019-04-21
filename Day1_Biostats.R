#09/04/2019
#Nobuhle Mpanza
#Day 1 Biostats
#Time -08:30-12:00 Session 1/Morning

#Basic_stats folder created within the Intro_R_UWC

#Load libraries
library(tidyverse)

#Load ChickWeight data/ built in data
chicks <- as_tibble(ChickWeight)

#Extra practice to explore data, do extra function.
ncol(chicks) #number of columns
head(chicks)
summary(chicks) #look at your data closely with all columns and rows
colnames(chicks) #display only column names
dim(chicks) #find dimensions
mean(chicks$weight) #calc the mean weight of all the chicks

#For every diet, only for day zero, calc the average weight
chicks %>% #specify the dataset youre using first
filter(Time == 0) %>% #filter only the time column
  group_by(Diet) %>% 
  summarise(mean_wt =mean(weight),
            sd_wt =sd(weight)) %>% 
  ungroup() #you can add all min, max within the same function and run.

chicks %>% 
  summarise(length = n()) #gives the same as nrow(chicks), number of rows
length(chicks$weight) #same as above.

#Mean
nums <- c(13,666,13,776,35,13)
sum(nums)/(6) #but for large dataset
mean_nums <- sum(nums) / length(nums) #manual cal of mean

chicks %>% 
round(mean(Chick$weight),1)

chicks %>% 
  summaise(mean_wt = mean(weight))

#What happens when you have missing values in your data.
numbers<- c(20, 15,10, NA,30, 50)
 numbers %>% 
   mean(numbers, na.rm = TRUE) #error, my code doesnt run here, WHY?
 mean(na.omit(numbers)) #removes the NA values
   

 
#Calculating the median
chicks %>% 
   summarise(med_wt = median(weight))

#Using given numbers and see change in median.
nums2<- c(5, 2, 6, 13, 1) 
mean(nums2)
median(nums2)

#median is still the same eve after changing numbers.
nums3<- c(5, 2, 6, 100, 1) 
mean(nums2)
median(nums2)

#Skewness and Kurtosis-will be done later.


#Variation and Spread
chicks %>% 
summarise(sd_wt = sd(weight))
quantile(chicks$weight) #Quantiles calculation

chicks %>% #representation of variability around data.
  summarise(min_wt = min(weight), 
                     qrt1_wt = quantile(weight, p = 0.25), 
                     med_wt = median(weight),
                     qrt3_wt = median(weight, p = 0.75),
                     max_wt = max(weight))

#Cal the range
range(chicks$weight) #Gives you the min and max values



#chicks %>% 
 # summarise(lower_wt = range(weight)[1], 
            upper_wt = range(weight)[2]

#Groupwise Summary section on pdf           
#Chicks younger than 10 days old, for every diets, cal summary stat
 summary(chicks$weight)
 
 group_stat <- chicks %>% #Fix this section, ask someone who did so i dont get lost.
 filter(Time<10) %>% 
   group_by(Diet) %>%
   summarise(mean_wt = round(mean(weight, na.rm = TRUE), 2), 
             med_wt = median(weight, na.rm = TRUE),
             sd_wt = round(sd(weight, na.rm = TRUE), 2), 
             sum_wt = sum(weight), min_wt = min(weight), 
             qrt1_wt = quantile(weight, p = 0.25), 
             med_wt = median(weight),
             qrt3_wt = median(weight, p = 0.75), 
             max_wt = max(weight), n_wt = n()) 
 group_stat
 

#Session 2: 14-17pm
#Day 1
 
#Load libraries for plotting
library(ggpubr)
 
#we want day 21 weights of different diets
plot_1 <- chicks %>% 
  filter(Time <10) %>% 
  ggplot(aes(x= Diet, y = weight)) + #pipe changes to + because we are doing graphs.
geom_point(data=group_stat, aes(x = Diet, y = mean_wt),
           col = "black", fill ="red", shape=23, size=3)+
  geom_jitter(width = 0.05) #not done here

#Plotting bar graph with errorbars/display mean weight with st dev
plot_2<- ggplot(data =group_stat, aes(x=Diet, y=mean_wt))+
  geom_bar(position=position_dodge(),stat ="identity", 
           col = NA, fill ="salmon")+ #col here means the outline of the bars, youcan change the colour.
  geom_errorbar(aes(ymin=mean_wt-sd_wt, 
                    ymax=mean_wt +sd_wt), #adding st.dev on bars
                width =0.2) #

#Plotting box plot
plot_3<-chicks %>% #we need only 21 from chicks data to do this plot,so we assign new name
  filter(Time==21) %>%
  ggplot(aes(x=Diet,y=weight))+
  geom_boxplot(fill= "salmon")+
  geom_jitter(width = 0.05, fill ="white", col ="blue", shape = 21)
  
#Plotting
plot_4<-chicks %>%   
  filter(Time %in% c(10,21)) %>%
  ggplot(aes(x=Diet,y=weight))+
  geom_boxplot(fill= "salmon")+
  geom_jitter(width = 0.05, fill ="white", col ="blue", shape = 21)

  
#Use summarise function to produce the same results as summary function
summary(chicks)

#Chapter4: Session 2 with Dr AJ Smith
#Qualitative data
#load libraries
library(tidyverse) 
library(ggpubr) 
library(RColorBrewer) 
library(ggthemes)

#load data build in
iris.cnt <- iris %>% 
  count(Species) %>% #count the number of species 
  mutate(prop = n / sum(n)) #create new column called proportion 
iris.cnt

#
plt1 <- ggplot(data = iris.cnt, aes(x = "", y = n, fill = Species)) +
  geom_bar(width = 1, stat = "identity") + 
  labs(title = "Stacked bar graph", subtitle = "cumulative sum", 
       x = NULL, y = "Count") + 
  theme_pubclean() + 
  scale_color_few()

#
plt2 <- ggplot(data = iris.cnt, aes(x = "", y = prop, fill = Species)) +
  geom_bar(width = 1, stat = "identity") + 
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions", 
       x = NULL, y = "Proportion") + 
  theme_pubclean() +
  scale_color_few() + 
  scale_fill_few()

# Plotting a pie chart, which you should never present in your research.
plt3 <- plt1 + 
  coord_polar("y", start = 0) + 
  labs(title = "Friends don't let...", subtitle = "...friends make pie charts", 
                                                  x = NULL, y = NULL) + 
  scale_fill_brewer(palette = "Blues") + 
  theme_minimal()

#Plot pie chart but show only the proportion, not the total original data.
#Change colour pallet also.

pltTry <- plt1 + 
  coord_polar("y", start = 0) + 
  labs(title = "We do not present...", subtitle = "...pie charts on research", 
       x = NULL, y = NULL) + 
  scale_fill_brewer(palette = "Blues") + 
  theme_minimal()

plt4 <- ggplot(data = iris, aes(x = Species, fill = Species)) +
  geom_bar(show.legend = FALSE) + 
  labs(title = "Side-by-side bars", subtitle = "n per species", y = "Count") + 
  theme_pubclean() +
  scale_color_few() +
  scale_fill_few()

#All the above plots in obe plot.
ggarrange(plt1, plt2, plt3, plt4, nrow = 2, ncol = 2, labels = "AUTO")

#Continuous data and section into bins and count how many timeseruption cocurs within each bin.
#Draw histogram to observe this
hist1 <- ggplot(data = faithful, aes(x = eruptions)) + 
  geom_histogram(colour = "black", fill = "salmon", alpha = 1) + #alpha is the transparency
  labs(title = "Old Faithful data",subtitle = "A vanilla frequency histogram", 
       x = "Eruption duration (min)", y = "Count") + 
  theme_pubclean()
#Graph produced is bimodal distribution, short or very long eruptions, nothing in between.
#NB:AJ prefers this kind of graph

summary(faithful) #to see better how long eruptions last 


hist2 <- ggplot(data = faithful, aes(x = eruptions)) + 
  geom_histogram(aes(y = ..density..), position = 'identity', binwidth = 1, 
                 colour = "black", fill = "salmon", alpha = 0.6) +
  labs(title = "Old Faithful data", subtitle = "Relative frequency histogram",
       x = "Eruption duration (min)", y = "Count") + 
  theme_pubclean()
#Density function rescales everything to =1, dont do this, stick with the first hist.

nrow(faithful) # To see number of rows

hist3 <- ggplot(data = faithful, aes(x = eruptions)) + 
  geom_histogram(aes(y = 0.5 * ..density..),
                 position = 'identity', binwidth = 0.5, 
                 colour = "black", fill = "salmon", alpha = 0.6) + 
  labs(title = "Old Faithful data", subtitle = "Relative frequency histogram", 
       x = "Eruption duration (min)", y = "Relative contribution") + 
  theme_pubclean()
#Use these codes for your own data, just adjust values.

#Not commonly used in research, just stick to histograms.
hist4 <- ggplot(data = faithful, aes(x = eruptions)) + 
  stat_ecdf() + 
  labs(title = "Old Faithful data", subtitle = "ECDF",
       x = "Eruption duration (min)", y = "Relative contribution") +
  theme_pubclean()

ggarrange(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2, labels = "AUTO") #All graphs combined

#Using iris data now.
#Create one long table- All Data in R 
iris.long <- iris %>% 
  gather(key = "variable", value = "size", -Species)

ggplot(data = iris.long, aes(x = size)) + 
  geom_histogram(position = "dodge", # ommitting this creates a stacked histogram
              colour = NA, bins = 20, aes(fill = Species)) +
             facet_wrap(~variable) +
              labs(title = "Iris data", subtitle = "Grouped frequency histogram", 
                   x = "Size (mm)", y = "Count") +
                   theme_pubclean()

#4.2.2. Box Plots
#hinges show first and third quartiles(25 and 75th percentile)
#Whiskers
#Dot outside plots are outliers
#Sertosa smaller sepal length than the other two species.

plt1 <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) + #each species box must be a different colour, so we use fill=species.
  geom_boxplot(show.legend = FALSE, notch = FALSE) + #We dont want legend cause theres already a difference in species, by colour
  theme_pubclean() + 
  labs(y = "Sepal length (mm)") +
  theme(axis.text.x = element_text(face = "italic"))

summary(iris) #above graph shows all this data seen using this function

#This graph doesnt need to be accompanied by stat,it shows significance between the species already.
plt2 <- ggplot(data = iris.long, aes(x = Species, y = size)) +
  geom_boxplot(fill = "red", alpha = 0.4, notch = TRUE) + 
  geom_jitter(width = 0.1, shape = 21, colour = "blue", fill = NA, alpha = 0.2) +
  facet_wrap(~variable, nrow = 1) + 
  labs(y = "Size (mm)") + 
  theme_pubclean() +
  theme(axis.text.x = element_text(face = "italic")) + 
  theme(axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.margin=unit(0.5, "cm"))

#Pairwise scatter plots
plt1 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) + 
  geom_point() + 
  labs(x = "Petal length (mm)", y = "Petal width (mm)") +
  theme(legend.position = c(0.18, 0.85)) + 
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() +
  theme_pubclean()

plt2 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) + geom_point(show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
  scale_color_fivethirtyeight() + 
  scale_fill_fivethirtyeight() + 
  labs(x = "Petal length (mm)", y = "Petal width (mm)") + 
  theme_pubclean()

ggarrange(plt1, plt2, ncol = 2, nrow = 1, labels = "AUTO") #combine both graphs.
#correlation graphs, not dependent on each other.Not regression.

#Bargraphs.
facet.names <- c(Petal.Length = "Petal length",
                 Petal.Width = "Petal width", 
                 Sepal.Length = "Sepal length", 
                 Sepal.Width = "Sepal width")

#PLotting four different graphs in one plot. Box plots
iris.long %>% group_by(Species, variable) %>% #by species plot for each
  summarise(mean.size = mean(size), sd.size = sd(size)) %>%
  ggplot(aes(x = Species, y = mean.size)) +
  geom_bar(stat = "identity") + #state youre plotting bar graph
  geom_errorbar(aes(ymin = mean.size - sd.size, ymax = mean.size + sd.size), width = 0.2) +
  facet_wrap(~variable, labeller = labeller(variable = facet.names)) + 
  labs(y = "Size (mm)", title = "A box plot...", subtitle = "...of the Iris data") +
  theme(axis.text.x = element_text(face = "italic"))

#Violin plots-shows frequency distribution. 


# Neat script
# Script runs
# good comments
# Dont simply just copy from the book but try new things
