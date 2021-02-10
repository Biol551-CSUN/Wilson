### Week 3b: Plotting penguin data  ###
### Created by: Emily Wilson ##########
### Updated on: 2021-02-10 ############

#### Load Libraries ############################################
library(palmerpenguins)
library(tidyverse)
library(here)
library(beyonce)
library(devtools)
library(ggthemes)


### Load data ##################################################
# The data is part of the package and is called penguins
glimpse(penguins) 


### Data Visualization #########################################
ggplot(data = penguins, 
       aes(x = bill_depth_mm,
          y = bill_length_mm)) +
  geom_point() +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") 


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm)) +
  geom_point() +
  geom_smooth() + # add best fit line
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") 


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm") + # make the best fit line specific to a linear model
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") 


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") 


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species)) + # grouping by species so there are three different linear regressions, one for each species
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)")


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) + #tell R to color species differently
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_viridis_d() #add viridis scale for colorblind-friendliness


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(0,20)) #set x limits from 0 to 20


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(0,20)) +
  scale_y_continuous(limits = c(0,50)) #set y limits to 0-50


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(14, 17, 21)) #set custom x axis breaks


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(14, 17, 21),
                     labels = c("low", "medium", "high")) # assign labels to scale breaks


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = c("orange", "purple", "green")) #replace viridis color scheme with your own


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(2)) # use Beyonce color palette


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(10)) #and do it again with different colors!


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(10)) +
  coord_flip() #flip the coordinates


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(10)) +
  coord_fixed() #fix the axes


# Use exponential data instead
ggplot(diamonds, 
       aes(carat, price)) +
  geom_point() 


ggplot(diamonds, 
       aes(carat, price)) +
  geom_point() +
coord_trans(x = "log10", y = "log10") #transform axes to log scale


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(10)) +
  coord_polar("x") #make it polar!


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(10)) +
  theme_classic() #layer on classic theme


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(10)) +
  theme_bw() #layer on black and white theme (I hate this one and always have)


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(10)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20,
                                  color = "red")) # make title color red- it's hideous :)


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(10)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20,
                                  color = "red"),
        panel.background = element_rect(fill = "linen")) # change background panel color


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(10)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        panel.background = element_rect(fill = "linen")) +
  ggsave(here("Week_3","outputs","penguin.png")) #save plot with ggsave


ggplot(data = penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           group = species,
           color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(10)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        panel.background = element_rect(fill = "linen")) +
  ggsave(here("Week_3","outputs","penguin.png"),
         width = 7, height = 5) #specify the dimensions we want for our saved plot (in inches)


penguinplot <- ggplot(data = penguins, #save plot as object so you can call it later
                      aes(x = bill_depth_mm,
                          y = bill_length_mm,
                          group = species,
                          color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(10)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        panel.background = element_rect(fill = "linen")) 


penguinplot #it's later, call the graph

