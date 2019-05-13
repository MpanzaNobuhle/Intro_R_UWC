#Nobuhle Mpanza 3951932
#Biostatics Asssignment
#Due: 13 May 2019 Monday
#Coding for the results section of my Assignment 

#*********************************************************************************************************

#Loading all possible Libraries to use for analyses
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)1
library(ggthemes)
library(fitdistrplus)
library(plotly)
library(scales)
library(ggsn)
library(ggmap)
library(e1071)
library(dplyr)
library(lattice)
#************************************************************************************************************************************

#loading Data
grass_tolerance <- datasets::CO2

#Infor about the data: Looks at the carbon dioxide uptake by grass plants at two sites in two treatments(chilled and nonchilled)


#**********************************************************************************************************************************
#Exploring my data

head(grass_tolerance) #viewing top half of my data
tail(grass_tolerance) #viewing bottm half of my data
colnames(grass_tolerance) #displaying all the columns to know what im working with
row.names(grass_tolerance) #displaying all rows
glimpse(grass_tolerance) #checking the whole structure of the data

#Creating colour pallete in case i need it for visualisations 
colourPal <- c("#004dcd", "#0068db", "#007ddb", "#008dcf","#66B6D2","#67B5D1","#68B4D1","#69B3D0","#69B2D0","#6AB1D0")
               
               
#More exploration;selecting certain variables only

By_site <- grass_tolerance %>% #assign new name from original dataset
select(Type, Treatment, uptake) #pickk out only the specified variables and create new dataset call By_site

filter(uptake == max(uptake)) #filter only the highest uptake of carbon dioxide

#More exploration of data before statistical tests are done
grass_tolerance %>% 
  group_by(Type) %>% #specifying that we are looking at each site/type
  summarise(avr_uptake =mean(uptake), #calc mean co2 uptake
            min_uptake =min(uptake), #minimum co2 uptake
            max_uptake =max(uptake), #highest  co2 uptake
            sd_uptake = sd(uptake))
             
#***********************************************************************************************************************************************************************************
#Statistical analysis of the data as well as hypothesis to be tested:

#Null Hypothesis: Hypothesis to be tested

#H01: There is no difference in co2 uptake by grass plants in Quebec and Mississippi.
#H02: Treatment (chilled and nonchilled) has no effect on the uptake of co2 by grass plants.


#The test i will use is: A t-test because we have two independent samples


#Assumptions:
#1.the dependent variable must be continuous
#2. the observations in the groups being compare dare independent of each other
#3.the data are normally distributed
#4.the data are homoscedastic

#First two assumtpions are met because we using data from two different sites.
#Last two assumptions must be tested: Using the code below

#Use histogram to test for normality
ggplot(data = grass_tolerance, aes(x = uptake, fill = Type)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = Type), colour = NA, alpha = 0.4) + 
  labs(x = "value")

#OR

#Test normality using Shapiro-Wilk Test
shapiro.test(grass_tolerance$uptake) 
#p-value=0.0007908

#Or we can run the same test but specify as numeric:same outcome
shapiro.test( as.numeric(grass_tolerance$uptake))




#Checking Homoscedasticity
#uptake by site/type
grass_tolerance %>%
  group_by(Type) %>% 
  summarise(Type_var = var(uptake))

#uptake by treatment
grass_tolerance %>%
  group_by(Treatment) %>% 
  summarise(Treatment_var = var(uptake))

#Or we can run the code below: To test for homoscedasticity
  var.test(uptake~Treatment, grass_tolerance, alternative="greater")

#Running the t-test (if data is normally distributed) because we are comparing two sites against each other.

#Uptake by treatment
t.test(uptake~ Treatment, data = grass_tolerance, var.equal = TRUE,
       alternative = "greater")

#Uptake by site
t.test(uptake~ Type, data = grass_tolerance, var.equal = TRUE,
       alternative = "greater")

#*******************************************************************************************
#If the above assumptions were not met i would have used the code below:
#Do not include on results:Just for future reference when i encounter data needing it
#Running Non parametric test-Wilcoxon Test when data is not normally distributed :

#Uptake by treatment
compare_means(uptake ~ Treatment, data = filter(grass_tolerance), 
              method = "wilcox.test")
#Uptake by site
compare_means(uptake ~ Type, data = filter(grass_tolerance), 
              method = "wilcox.test")

#***************************************************************************************************************************
#VISUALIZATION OF DATA: After statistic tests have been done.


#visualising the uptake of co2 at different treatments using a boxplot

#With Treatment on the x axis
ggplot(data = By_site, aes(x = Treatment, y = uptake)) +
  geom_boxplot(aes(fill = Type)) + 
  labs(x = "Treatment", y = " CO2 Uptake (umol/m-2 sec)")


#With site on the x axis:I will use this one in results section
ggplot(data = By_site, aes(x = Type, y = uptake)) +
  geom_boxplot(aes(fill = Treatment)) + 
  labs(x = "Site", y = " CO2 Uptake (umol/m-2 sec)")


#Line graph for ambient co2 against co2 uptake in both sites and treatments
xyplot(uptake~conc|Type,groups=Treatment, auto.key=TRUE,type=c("p","smooth"),data=grass_tolerance,xlab="ambient CO2 (mL/L )",ylab=" CO2 Uptake (umol^2/sec)")


#creating a heat map to see the uptake of co2 by grass plants from each site in two different treatment (chilled and nonchilled).
ggplot(By_site, aes(Treatment, Type)) + 
  geom_raster(aes(fill = uptake))+
  labs(x = "Treament", y = "Site")+
  scale_fill_continuous(name = "uptake")    


#Same above plot can be plotted as follows but specify high and low colours
ggplot(By_site, aes(Treatment, Type)) + 
  geom_raster(aes(fill = uptake))+
  labs(x = "Treament", y = "Site")+
  scale_fill_continuous(name = "uptake")+
  scale_fill_gradient(low = "#55E7F2", high = "#288A84",
                      space = "Lab", na.value = "grey50", guide = "colourbar",
                      aesthetics = "fill")
#explainng second heat map to use for results section:
#The darker green portion indicates the highest co2 uptake and the lighter green portion indicates the lowest co2 uptake.
#co2 uptake is much higher in Quebec (non chilled) than in (chilled) treatment.
#co2 uptake is overall the lowest in a chilled treatment Mississippi.
#Site with the overall highest co2 uptake is Quebec in both chilled and nonchilled treatment

#***********************************************************************************************************************************************  
#Creating the world map in order to highlight Mississippii and Quebec

ggplot() +
  borders() +  
  coord_equal()

#*********************************************************************************************************
#More graphs to visualise the data


#Bargraph showing the proportion of how much co2 uptake occured in chilled and nonchilled environment
 ggplot(data = By_site, aes(x = "", y = uptake, fill = Treatment)) +
   geom_bar(width = 2, stat = "identity") + 
   scale_y_continuous(breaks = c(5, 10, 30, 50)) + 
   labs(title = "", subtitle = "", x = NULL, y = "CO2 uptake proportion") +
   theme_pubclean() +
   scale_color_few() +
   scale_fill_few()
#leave the above graph out from results, its not too informative
 

#Visualisation of carbon dioxide uptake in relation to the ambient carbon dioxide.

#Plot showing the co2 uptake by each plant in relation to the ambient co2 concentration.
 coplot(uptake ~ conc | Plant, data = grass_tolerance, show.given = TRUE, type = "b")+
   
         
 
##**********************************************************************************************************
#Extra work on the data:not for inclusion in assignment: 
 
 
#scatterplot for uptake at different conc
ggplot(grass_tolerance,aes(x=conc,y=uptake, colour=Type,shape=Treatment))+
  geom_point()+
  scale_x_log10()
 
 #same as above scatterplot but coloured by Type only and shape not specified
 ggplot(grass_tolerance,aes(x=conc,y=uptake, colour=Type))+
   geom_point()+
   scale_x_log10()

#multiple linear regression to predict uptake usinglog of co2 conc and type and treatment assign a name
fit <- lm(uptake~log(conc)+Type+Treatment,data=grass_tolerance)

#display summary of fit
summary(fit)
#pvalue=1.11e-08

#boxplot
bwplot(uptake~Treatment|Type, data=grass_tolerance, xlab="Treatment", ylab = " CO2 uptake(umol^2/sec)")



