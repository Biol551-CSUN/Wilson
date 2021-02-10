### This is a script for Spring 2021 Computer Modeling covering intro to plotting in ggplot
### Created by: Emily Wilson
### Created on: 02/08/2021
################################################################

### Load libraries; data loaded in w/ package ##################
library(tidyverse)
library(palmerpenguins)

### View data #############################################
glimpse(penguins) #lets you see columns w/ types of data


### Start plotting! ############################################
ggplot(data = penguins) #nothing shows b/c you didn't give it any aesthetics

ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm)) #don't get any data b/c you didn't specify a geom()

ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm)) +
    geom_point() #progress :)

ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species)) + #added color to points by species
  geom_point()

ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species)) +
  geom_point()+
  labs(title = "Bill Depth and Length") #add title

ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species)) +
  geom_point()+
  labs(title = "Bill Depth and Length",
       subtitle = "Dimensons for Adelie, Chinstrap, and Gentoo Penguins") # add subtitle

ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species)) +
  geom_point()+
  labs(title = "Bill Depth and Length",
       subtitle = "Dimensons for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill Depth (mm)", y = "Bill Length (mm)") # add axis labels


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species)) +
  geom_point()+
  labs(title = "Bill Depth and Length",
       subtitle = "Dimensons for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill Depth (mm)", y = "Bill Length (mm)",
       color = "Species") # change legend title (capitalize, in this case)

ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species)) +
  geom_point()+
  labs(title = "Bill Depth and Length",
       subtitle = "Dimensons for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill Depth (mm)", y = "Bill Length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package") # add caption for data source info

ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species)) +
  geom_point()+
  labs(title = "Bill Depth and Length",
       subtitle = "Dimensons for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill Depth (mm)", y = "Bill Length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
    scale_color_viridis_d() # overlay viridis for colorblind viewers

ggplot(data = penguins, 
       aes(x = bill_depth_mm,
          y = bill_length_mm,
          color = species,
          shape = island)) + # add shape by island for each data point
  geom_point()+
  labs(title = "Bill Depth and Length",
       subtitle = "Dimensons for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill Depth (mm)", y = "Bill Length (mm)",
       color = "Species", shape = "Island", # and capitalize the shape legend
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d()

ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species,
           shape = species)) + # add shape by species for each data point; makes colors redundant
  geom_point()+
  labs(title = "Bill Depth and Length",
       subtitle = "Dimensons for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill Depth (mm)", y = "Bill Length (mm)",
       color = "Species", shape = "Species",  # and make sure the legends match up
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d()

######################################################################################################

ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species,
           size = body_mass_g)) + # change point size by body mass
    geom_point()+
    scale_color_viridis_d()

ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species,
           size = body_mass_g,
           alpha = flipper_length_mm)) + #add transparency by flipper length
  geom_point()+
  scale_color_viridis_d()

######################################################################################################

ggplot(data=penguins, 
  aes(x = bill_depth_mm,
      y = bill_length_mm,
      size = body_mass_g,
      alpha = flipper_length_mm)) + # mapping applies settings based on values of the points
  geom_point()


ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm)) +
  geom_point(size = 2, alpha = 0.5) # setting applies settings to all points irregardless of value

####################################################################################################

ggplot(penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm)) +
  geom_point() +
  facet_grid(species~sex) # makes a bunch of plots separated by sex and species; facet_grid only gives you a grid


ggplot(penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm)) +
  geom_point() +
  facet_grid(species~sex) # what you get on top/side depends on the order you type in


ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) + 
  geom_point() +
  facet_wrap(~ species) # facet_wrap basically does the same thing as faced_grid but gives you more control; sep by spp


ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) + 
  geom_point() +
  facet_wrap(~ species, ncol = 2) # makes it two columns instead of 3

ggplot(data=penguins, 
    aes(x = bill_depth_mm,
        y = bill_length_mm,
        color = species)) + # lets you work with colors still (though this is redundant)
  geom_point() +
  scale_color_viridis_d() +
  facet_grid(species~sex)


ggplot(data=penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species)) +
  geom_point() +
  scale_color_viridis_d() +
  facet_grid(species~sex) +
  guides (color = FALSE) #lets you remove color legend w/out removing colors

