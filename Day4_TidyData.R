#Day 4_last session
#Nobuhle Mpanza
#01 February 2019
#Tidy data

#loading libraries
library(tidyverse)


#Loading Data
#For .RData file just use load() and for .csv file use read_csv.()
#Explore your data before running any codes just so you can have a clear picture of what youre working with.

load("data/SACTN_mangled.RData")

#Plotting a graph using SACTN1 data,specify which data to use by using aes function.
#paste0 function in front of group is used when youre grouping by two different variables. But for one variable you use groub_by.
#geom_line makes line graph.
#colour means each site must have a different colour.

ggplot(SACTN1,aes(x= date, y = temp)) +
geom_line(aes(colour =site, group = paste0(site,src))) +
  labs(x= "Date", y= "Temp")

#tidyverse also has gather function.
#Gather function is used to tell R to gather put the sources under one column and temp on its own column.

SACTN2_tidy <- SACTN2 %>% #assign a new name SACTN2_tidy from the SACTN datasets
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp")

#Spreading function
#explore your first, the problem here is the var and val columns(untidy) so we run a code to make depth a comlumn and put the values below it

SACTN3_tidy <- SACTN3 %>%
  spread(key = var, value = val)

#Seperate function-separates variables into different columns.
#Explore SACTN4a and you see that site and source are placed in one column,so you need to seperate them
#If in exam they ask select site port nolloth but you look at your data and you dont have site,so just use separate function to make site a column and src a column of it own then run whatever youve been asked.
#c means seperate into two columns,when you seperate anything you use c.
#using SACTN4a dataset assign a new dataset named SACTN4a_tidy.

SACTN4a_tidy <- SACTN4a %>%
  separate(col = index, into = c("site", "src"), sep = "/ ")

#Unite function-puts variables in different columns together
#Explore SACTN4a, snd create new datase
#Problem:year,month and day are in seperate columns=unite and put it in one column under variable date.
#Then seperate them using  a -

SACTN4b_tidy <- SACTN4b %>%
  unite(year, month, day, col = "date", sep = "-")

#Joining function- usin the left-join dictates similar words and joins them under those columns.
#Assign new dataset called SACTN_tidy by uniting SACTN4a_tidy and SACTN4b_tidy datasets using the lef_join function.

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy)

# [A.A]
# Shows a clear understanding of the code
# Comments throughout th script
# Could try new things and explore the functions a bit more
# NIcely done



