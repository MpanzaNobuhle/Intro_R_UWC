
#Day 1
#Laminaria dataset exploring and learning
#NP Mpanza
#29 Jan 2019

#Loading Libraries

#Create a new data frame from the `laminaria` dataset that meets the following criteria: contains only the `site` column and a new column called `total_length_half` containing values that are half of the `total_length`. In this `total_length_half` column, there are no `NA`s and all values are less than 100.
#Hint**: think about how the commands should be ordered to produce this data frame!

total_length_half <- lam %>% 
  mutate(total_length_half=total_length / 2) %>% 
  filter(total_length_half < 100) %>% 
  select(site,total_length_half)

#Use `group_by()` and `summarize()` to find the mean, min, and max blade_length for each site. Also add the number of observations (hint: see `?n`).

group_by(site) %>% 
  summarize(mean_blade_length = mean(blade_length),
            min_blade_length = min(blade_length),
             max_blade_length = max(blade_length),
            n=n())
#What was the heaviest stipe measured in each site? Return the columns `site`, `region`, and `stipe_length`.

lam %>% 
  group_by(site) %>% 
  filter(stipe_mass ==max(stipe_mass)) %>% 
  select(site, region, stipe_length)

