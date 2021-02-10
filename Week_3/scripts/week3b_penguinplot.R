### Week 3b: Penguin Plot Lab/Homework Assignment ###
### Created by: Emily Wilson ##########
### Updated on: 2021-02-10 ############

#### Load libraries ############################################
library(palmerpenguins)
library(tidyverse)
library(here)


### Load data ##################################################
# The data is part of the package and is called penguins
glimpse(penguins) 


###################################################################################################################################################################################################

### Lab/Homework: Make your own plot with the penguin data

penguin <- drop_na(penguins) #remove NAs so they don't show as an additional sex in the legend

ggplot(data = penguin,
       aes(x = island,
           y = body_mass_g))+
  geom_boxplot(fill = "grey90",  # overlay boxplot geom, change background fill to light grey so the boxplot sticks out more against white background
               outlier.shape = NA) + #remove that one outlier from the boxlot geom so it doesn't count twice when jitter geom is overlayed
  geom_jitter(data = penguin, #overlay jtter geom on top of boxplot geom
              aes(x = island,
                  y = body_mass_g,
                  color = sex), #color jittered points by sex
              alpha = 0.5, # make jittered points more transparent so you can see them all, even if they're overlapping
              position = position_jitterdodge(dodge = 0.5, jitter.width = 0.9)) + #separate the jittered values according to taste so they can be seen more easily
  labs(title = "Penguin Body Mass by Sex", # add title 
       subtitle = "For Biscoe, Dream, and Torgersen Islands", # add subtitle
       caption = "Source: Palmer Station LTER / palmerpenguins package", # ad caption with source for the data
       x = "Island", # capitlize x axis label
       y = "Body Mass (g)", # capitalize y axis label
       color = "Sex") + #capitalize legend title
  scale_color_manual(labels = c("Female", "Male"), # capitalize legend labels
                     values = c("green", "blue")) + # assign different colors to jittered values based on sex
  theme_classic() + # I love a good classic theme
  ggsave(here("Week_3","outputs","grouppenguinplot.png"), #tell R where to save this plot as a png
         width = 7, height = 5) #and tell R how big I want that plot/what dimensions


### Alternate violin plot, for funsies - we considered using this one, but liked the boxplot more ####################################################################################################

ggplot(data = penguin,
       aes(x = island,
           y = body_mass_g))+
  geom_violin(fill = "grey90") +  # overlay violin plot geom, change background fill to light grey so the boxplot sticks out more against white background
  geom_jitter(data = penguin, #overlay jtter geom on top of boxplot geom
              aes(x = island,
                  y = body_mass_g,
                  color = sex), #color jittered points by sex
              alpha = 0.5, # make jittered points more transparent so you can see them all, even if they're overlapping
              position = position_jitterdodge(dodge = 0.5, jitter.width = 0.9)) + #separate the jittered values according to taste so they can be seen more easily
  labs(title = "Penguin Body Mass by Sex", # add title 
       subtitle = "For Biscoe, Dream, and Torgersen Islands", # add subtitle
       caption = "Source: Palmer Station LTER / palmerpenguins package", # ad caption with source for the data
       x = "Island", # capitlize x axis label
       y = "Body Mass (g)", # capitalize y axis label
       color = "Sex") + #capitalize legend title
  scale_color_manual(labels = c("Female", "Male"), # capitalize legend labels
                     values = c("green", "blue")) + # assign different colors to jittered values based on sex
  theme_classic()  # I love a good classic theme

