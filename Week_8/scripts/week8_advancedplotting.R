### This is a script for Spring 2021 Computer Modeling covering advanced plotting in R
### Created by: Emily Wilson
### Created on: 03/22/21
############################################################################################

# Load libraries ###########################################################################

library(tidyverse)
library(here)
library(patchwork)
library(ggrepel)
library(gganimate)
library(magick)
library(palmerpenguins)


# Start plotting! ##########################################################################

# Patchwork:

p1 <-penguins %>% # penguin plot 1
  ggplot(aes(x = body_mass_g, 
             y = bill_length_mm, 
             color = species))+
  geom_point()


p2 <- penguins %>% # penguin plot 2
  ggplot(aes(x = sex, 
             y = body_mass_g, 
             color = species))+
  geom_jitter(width = 0.2)


p1 + p2 + # bring them together using patchwork
  plot_layout(guides = 'collect') + # bring legends together
  plot_annotation(tag_levels = 'A') # label plots

p1 / p2 + # put one plot on top of another
  plot_layout(guides = 'collect') + # bring legends together
  plot_annotation(tag_levels = 'A') # label plots



# ggrepel:

ggplot(mtcars, aes(x = wt,  # use mtcars
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_text() + # creates text labels
  geom_point(color = 'red') 

ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_text_repel() + # repel text
  geom_point(color = 'red') 

ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) + 
  geom_label_repel() + # repel labels
  geom_point(color = 'red') 



# gganimate: 

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states( # add in transitions
    year, # what we want to transition by
    transition_length = 2, # length of transition
    state_length = 1) + # pause length between transitions
  ease_aes("bounce-in-out") + # control animation
  ggtitle('Year: {closest_state}') + # add title for closest transition state (year, in this case)
  anim_save(here("Week_8","outputs","pengiungif.gif")) # save as gif


# magick

penguin <- image_read("https://pngimg.com/uploads/penguin/pinguin_PNG9.png") # read in a penguin png

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  ggsave(here("Week_8","outputs","penguinplot.png")) # save plot as an image before you can put anything on it


pengplot <- image_read(here("Week_8","outputs","penguinplot.png")) # read in plot image (NEEDS to be png)
out <- image_composite(pengplot, penguin, # composite the images
                       offset = "+70+30") # tell R where to put the penguin
out


pengif <- image_read("https://media3.giphy.com/media/H4uE6w9G1uK4M/giphy.gif") # read in image as gif file
outgif <- image_composite(pengplot, pengif, gravity = "center") # composite (gravity can be east, west, center, bottom)
animation <- image_animate(outgif, fps = 10, optimize = TRUE) # have to tell R this is an animation, optimize it for image processing
animation

