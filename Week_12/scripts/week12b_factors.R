### This is a script for Spring 2021 Computer Modeling covering working wth factors in R
### Created by: Emily Wilson
### Created on: 04/21/21
############################################################################################

# Load libraries ###########################################################################
library(tidyverse)
library(here)

# Load data ################################################################################
tuesdata <- tidytuesdayR::tt_load(2021, week = 7)
income_mean<-tuesdata$income_mean


# Mess with factors ########################################################################

fruits <- factor(c("Apple", "Grape", "Banana")) 
fruits # default leveling is alphabetical unless you mess with it yourself

test<-c("A", "1", "2")
as.numeric(test) # converting that character to numeric results in NAs

test<-factor(test) # covert to factor
as.numeric(test) # changes "A" to "3"


# Reordering bar graphs

starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) # how many individuals of each species are present across starwars films?

star_counts <- starwars %>%
  filter(!is.na(species)) %>%
  mutate(species = fct_lump(species, n = 3)) %>% # lump soecies together that have fewer than 3 individuals
  count(species)

star_counts %>%
  ggplot(aes(x = species, y = n)) +
  geom_col() # shows in alphabetical order

star_counts %>%
  ggplot(aes(x = fct_reorder(species, n), y = n))+  # reorder the factor of species by n; this is ascending order
  geom_col()

star_counts %>%
  ggplot(aes(x = fct_reorder(species, n, .desc = TRUE), y = n))+ # make it descending instead
  geom_col() +
  labs(x = "Species")

total_income <- income_mean %>%
  group_by(year, income_quintile) %>%
  summarise(income_dollars_sum = sum(income_dollars)) %>%
  mutate(income_quintile = factor(income_quintile)) # make it a factor

total_income%>%
  ggplot(aes(x = year, y = income_dollars_sum, color = income_quintile))+
  geom_line()

total_income %>%
  ggplot(aes(x = year, y = income_dollars_sum, 
             color = fct_reorder2(income_quintile,year,income_dollars_sum))) + # reorder the legend
  geom_line() +
  labs(color = "Income Quintile")

# Relevel by whatever we want
x1 <- factor(c("Jan", "Mar", "Apr", "Dec"))
x1

x1 <- factor(c("Jan", "Mar", "Apr", "Dec"), levels = c("Jan", "Mar", "Apr", "Dec"))
x1

# Subset data with factors

starwars_clean <- starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor #<< 
  filter(n>3) # only keep species that have more than 3

starwars_clean

levels(starwars_clean$species) # check levels; still shows everything :(

starwars_clean <- starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor 
  filter(n>3)  %>% # only keep species that have more than 3 
  droplevels() # drop extra levels

levels(starwars_clean$species) # check levels; fixed it

# Recode levels

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor 
  filter(n>3)  %>% # only keep species that have more than 3 
  droplevels() %>% # drop extra levels 
  mutate(species = fct_recode(species, "Humanoid" = "Human")) # recode human to humanoid

starwars_clean