### This is a script for Spring 2021 Computer Modeling covering data wrangling using joins
### Created by: Emily Wilson
### Created on: 02/22/2021
############################################################################################

### Load libraries #########################################################################
library(tidyverse)
library(here)

### Read in data ###########################################################################

# Environmental data from each site
enviro<-read_csv(here("Week_5","data", "site.characteristics.data.csv"))

# Thermal performance data
tpc<-read_csv(here("Week_5","data","Topt_data.csv"))

# View data
view(enviro)
view(tpc)

### Start wrangling! #######################################################################

enviro_wide <- enviro %>% # pivot enviro data to wider to match tpc data
  pivot_wider(names_from = parameter.measured, # column names from parameters measured
              values_from = values) %>%  # values from numbers column
  arrange(site.letter) # rearrange by site letter so they're in order


fulldata_left <- left_join(tpc, enviro_wide) %>%  # left join tpc and wide enviro data to combine sets
  relocate(where(is.numeric), .after = where(is.character)) # relocate all numeric columns after character columns


fulldata_long <- fulldata_left %>% # get joined data into long format
  pivot_longer(cols = E:substrate.cover, # choose what columns to pivot
               names_to = "measurement", # choose where to put column names
               values_to = "values") # choose where to put values


fulldata_summ <- fulldata_long %>% # summarize mean and var
  group_by(site.letter, measurement) %>% # group by site and measurement
  summarise(mean_val = mean(values), var_val = var(values)) # summary stats


t1 <- tibble(Site.ID = c("A", "B", "C", "D"), # make tibble 1
             Temperature = c(14.1, 16.7, 15.3, 12.8))

t2 <- tibble(Site.ID = c("A", "B", "D", "E"), # make tibble 2
             pH = c(7.3, 7.8, 8.1, 7.9))


left_join(t1, t2) # try out a left join

right_join(t1, t2) # try out a right join

inner_join(t1, t2) # try out inner join (only cases complete in both tibbles)

full_join(t1, t2) # try out full join (keeps all cases)

semi_join(t1, t2) # only keeps rows from first data set where there are matching cases in right data set

anti_join(t1, t2) # only keeps rows from first data set that are not present in second data set
