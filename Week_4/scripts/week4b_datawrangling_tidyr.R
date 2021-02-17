### This is a script for Spring 2021 Computer Modeling covering data wrangling with tidyr
### Created by: Emily Wilson
### Created on: 02/17/2021
############################################################################################

### Load libraries #########################################################################
library(tidyverse)
library(here)

### Read in data ###########################################################################

chem <- read.csv(here("Week_4/", "data", "chemicaldata_maunalua.csv"))

### Start wrangling! #######################################################################

chem_tidy <- chem %>%
  filter(complete.cases(.)) # filter out anything that isn't complete


chem_tidy <- chem %>%
  filter(complete.cases(.)) %>% 
  separate("Tide_time", c("tide", "time"), # separate out tide and time column into different columns
           remove = FALSE) # retain original column


chem_tidy <- chem %>%
  filter(complete.cases(.)) %>% 
  separate("Tide_time", c("tide", "time"), 
           remove = FALSE) %>% 
  unite("site_zone", c("Site", "Zone"), # unite site and zone into one column (ew)
        remove = FALSE) # retain original column


chem_tidy <- chem_tidy %>% 
  pivot_longer(cols = Temp_in:percent_sgd, # what columns we want to pivot to longer
               names_to = "variables", # what name we want the variables to be under
               values_to = "values") # what column we want the values for the variables to go to

view(chem_tidy) # check that it worked


chem_summ <- chem_tidy %>% 
  group_by(variables, Site) %>% # group by variable and site
  summarise(p_mean = mean(values, na.rm = TRUE), # get mean of values
            p_variance = var(values, na.rm = TRUE)) # get variance of values

view(chem_summ)


chem_summ_total <- chem_tidy %>% 
  group_by(Site, Zone, tide) %>% # group by site, zone, tide
  summarise(p_mean = mean(values, na.rm = TRUE), # get mean of values)
            p_variance = var(values, na.rm = TRUE), # get variance of values
            p_sd = sd(values, na.rm = TRUE))
view(chem_summ_total)  


chem_tidy %>% 
  ggplot(aes(x = Site, y = values)) + # assign aesthetics
  geom_boxplot() + # add boxplot geom
  facet_wrap(~variables, # facet wrap by parameters
             scales = "free")  # frees up x and y scales


chem_wide <- chem_tidy %>% # make the data wide again
  pivot_wider(names_from = variables, # which column to get new column names from
              values_from = values) # which column to get values from

view(chem_wide)


# Calculate summary stats and export; put everything together with pipes so there aren't like 30 data frames.

chem_clean <- chem %>%
  filter(complete.cases(.)) %>% 
  separate("Tide_time", c("tide", "time")) %>%  # separate out tide and time column into different columns
  pivot_longer(cols = Temp_in:percent_sgd, # what columns we want to pivot to longer
               names_to = "variables", # what name we want the variables to be under
               values_to = "values") %>%  # what column we want the values for the variables to go to
group_by(variables, Site, time) %>% # group by variable, site, and time
  summarise(mean_vals = mean(values, na.rm = TRUE)) %>% # get mean of values
  pivot_wider(names_from = variables, # which column to get new column names from
              values_from = mean_vals) %>% # which column to get values from
    write_csv(here("Week_4","outputs","summary.csv"))  # export as a csv to the correct folder

view(chem_clean)









