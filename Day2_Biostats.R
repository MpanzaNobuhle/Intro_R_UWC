#Day2 Biostats first session
#Nobuhle Mpanza
#15 April 2019
#Chapter 5 

#loading libraries
library(tidyverse)
library(fitdistrplus)
library(logspline)

#First create random numbers y <-c(numbers in here)
#calc length of the numbers you created, calc mean and st.dev, median-use summarise function
#if mean and median are different, the data is skewed.
#Create hist graph using the random numbers you created ro see visualisation


y <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00, 38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40, 42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40, 49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60, 45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30, 36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00, 38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)

length(y)
mean(y)
sd(y)
median(y)


par(mfrow = c(2, 2)) 
plot(x = c(1:length(y)), y = y) 
hist(y) 
descdist(y, 
discrete = FALSE, boot = 100)

#Random numbers created by class
y_class <- c(18,9,31,7,47,28,20,300,19,6,19,21,99,85,52,68,69,3,48,116,15,27,51,100,105,99,73,58,1,89,222,56,27,36,300,121,5,42,184,88,24,127,67,93,85,60,92,23,39,140,60,71,333,42,16,51,151,625,624,200,350,4,105,199,88,742)
length(y_class) #how many numbers are in the dataset
mean(y_class)
sd(y_class)
median(y_class)
hist(y_class) #draw histogram.

#Then plot the three graphs here usinng our the class generated data.
#par(mfrow = c(2, 2)) 
#plot(x = c(1:length(y_class), y = y_class) 
#hist(y_class) 
#descdist(y, 
         #discrete = FALSE, boot = 100)


#rnorm means random numbers
y2 <- rnorm(10,13,2)
length(y2)
mean(y2)
sd(y2)
hist(y2)
descdist(y2,discrete = FALSE)

#Example from notes
#y <-rnorm(n = 200, m = 13, sd = 2) 
#par(mfrow = c(2, 2))
#hist(y, main = "Histogram of observed data") 
#plot(density(y), main = "Density estimate of data")
  # plot(ecdf(y),
     # z.norm <- (y - mean(y)) / sd(y) 
     # qqnorm(z.norm) main = "Empirical cumulative distribution function")
   #abline(0, 1)
