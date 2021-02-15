### This is a script for Spring 2021 Computer Modeling covering data wrangling in dplyr
### Created by: Emily Wilson
### Created on: 02/15/2021
############################################################################################

### Load libraries #########################################################################
library(tidyverse)
library(palmerpenguins)
library(here)

### View data ##############################################################################
glimpse(penguins)

### Start wrangling! ########################################################################

# Using filter()

filter(penguins, sex == "female") # filter only females; == is "exactly"

filter(penguins, year == "2008") # only ones from 2008

filter(penguins, body_mass_g > 5000) # only ones with body mass over 5000g

filter(penguins, sex == "female", body_mass_g > 4000) # gives you both females where body mass is over 4000

filter(penguins, sex == "female" | body_mass_g > 4000) # gives you either females or ones where body mass is over 4000

filter(penguins, year == 2008 | year == 2009) # gives you 2008 or 2009

filter(penguins, island != "Dream") # gives you any island except dream

filter(penguins, species %in% c("Adelie", "Gentoo")) # gives you only Adelie and Gentoo 


# Using mutate()

(penguinmass <- mutate(penguins, body_mass_kg = body_mass_g / 1000)) # adds a column of body mass converted to kg

(penguinmass <- mutate(penguins, body_mass_kg = body_mass_g / 1000, 
                       bill_length_depth = bill_length_mm / bill_depth_mm)) # adds on another column with  ratio of bill length to depth

(penguinyear <- mutate(penguins, 
                      after_2008 = ifelse(year >= 2008, "After 2008", "Before 2008"))) # gives you a new column with before or after 2008


(penguinlength <- mutate(penguins,
                         lengthmass = (flipper_length_mm + body_mass_g))) # gives column with sum of flipper length and body mass

(penguincaps <- mutate(penguins,
                       sex_cap = ifelse(sex == "male", "Male", "Female"))) # capitalizes sex


# Finally use pipes

#And select


(penguins %>%
  filter(sex == "female") %>% #only select females
  mutate(log_mass = log(body_mass_g))) #calculate log biomass

(penguins %>%
  filter(sex == "female") %>% #only select females
  mutate(log_mass = log(body_mass_g)) %>% #calculate log biomass
  select(species, island, sex, log_mass)) # select these columns to remain in the data frame

(penguins %>% 
  filter(sex == "female") %>% # only select females
  mutate(log_mass = log(body_mass_g)) %>% #calculate log biomass
  select(Species = species, island, sex, log_mass)) # rename species column to be capitalized

(penguins %>%  
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE))) # get mean flipper length and exclude NAs

(penguins %>% 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE), # get mean flipper length
            min_flipper = min(flipper_length_mm, na.rm=TRUE))) # get minimum flipper length


# Use group_by()

(penguins %>%
  group_by(island) %>% # group by island
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE), # calc mean bill length for each island
            max_bill_length = max(bill_length_mm, na.rm=TRUE))) # calc max bill length for each island

(penguins %>%
  group_by(island, sex) %>% # group by island and sex
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE), # calc mean bill length for each sec on each island
            max_bill_length = max(bill_length_mm, na.rm=TRUE))) # calc max bill length for each sec on each island


#Removing NAs

(penguins %>%
  drop_na(sex))

(penguins %>%
  drop_na(sex) %>% #take out NAs for sex
  group_by(island, sex) %>% # group by island and sex
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))) # calc mean bill length by island and sex


# Integrate this with ggplot

penguins %>%
  drop_na(sex) %>% #remove NAs for sex
  ggplot(aes(x = sex, y = flipper_length_mm)) + # throw it all in with ggplot
  geom_boxplot() # and give me a boxplot






















