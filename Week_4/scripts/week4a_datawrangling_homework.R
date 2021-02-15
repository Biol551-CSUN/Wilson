### This is a script for Spring 2021 Computer Modeling Homework; Data wrangling
### Created by: Emily Wilson
### Created on: 02/15/2021
############################################################################################

### Load libraries #########################################################################
library(tidyverse)
library(palmerpenguins)
library(here)
library(calecopal)

### View data ##############################################################################
glimpse(penguins)

### Start wrangling! ########################################################################

# Part 1:

penguins %>% # tell it to use the penguin data
  drop_na(sex, body_mass_g) %>% # drop NAs for sex and body mass
  group_by(species, island, sex) %>% # group by species, island, and sex
  summarise(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE), #get one column of mean body mass
         variance = var(body_mass_g, na.rm = TRUE)) # and another for variance of body mass

# Part 2:

pal = cal_palette()

penguins %>% # select data
  filter(sex == "female") %>% #only include females
  mutate(log_mass_g = log(body_mass_g)) %>% # get log of body mass
  select(species, island, sex, log_mass_g) %>% # only keep these columns
  ggplot(aes(x = island, y = log_mass_g, color = species)) + #give aesthetics
  geom_jitter(alpha = 0.5, size = 3, # make the points slightly transparent, and bigger
              position = position_jitterdodge(dodge = 0.7, jitter.width = 0.4)) + # keep species and individual values from overlapping too badly
  scale_color_viridis_d() + # use viridis color scale
  coord_flip() + # flip coordinates
  labs(x = "Island", y = "Log of Body Mass (g)", # reassign axis labels
       title = "Penguin Body Mass by Island",
       subtitle = "For Female Adelie, Chinstrap, and Gentoo Penguins", # and give it a title
       caption = "Source: Palmer Station LTER / palmerpenguins package", # caption with data source
       color = "Species") + 
  theme_classic() + #love me a good classic theme
  theme(panel.background = element_blank()) +  # but detest panel backgrounds, so get rid of that 
  ggsave(here("Week_4","outputs","datawranglingplot.png"), #tell R where to save this plot as a png
         width = 8, height = 5) #and tell R how big I want that plot/what dimensions
  
  













