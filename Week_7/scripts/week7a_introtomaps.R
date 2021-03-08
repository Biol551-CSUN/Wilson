### This is a script for Spring 2021 Computer Modeling covering making maps in R
### Created by: Emily Wilson
### Created on: 03/08/21
############################################################################################
  
### Load libraries #########################################################################

library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)


### Read in data ###########################################################################

pop <- read.csv(here("Week_7", "data", "CAPopdata.csv")) # population by county for California
star <- read.csv(here("Week_7", "data", "stars.csv")) # sea star abundances at field sites


### Start mapping! #########################################################################

world <- map_data("world") # data for the world

usa <- map_data("usa") # data for US

italy <- map_data("italy") # data for Italy

states <- map_data("state") # data for states

counties <- map_data("county") # data for counties


# Make map of the world
ggplot()+
  geom_polygon(data = world, 
               aes(x = long,# longtitude
                   y = lat, #latitude
                   group = group, # controls connections between groups of points; "don't connect points between different groups"
                   fill = region), # fill by region
               color = "black") + # assign line color
  guides(fill = FALSE) + # don't give me a legend for over 200 countries
  theme_void() + # can still change themes
  theme(panel.background = element_rect(fill = "lightblue")) + # add ocean color to background
  coord_map(projection = "sinusoidal", # change projection (default is Richard's least favorite, mercator)
            xlim = c(-180,180)) # set x limits


# Make map of just California

ca_data <- states %>%
  filter(region == "california") # just california data

ggplot()+
  geom_polygon(data = ca_data, # california data
               aes(x = long, # longtitude
                   y = lat, # latitude
                   group = group), # keep it form being an abstract art piece
               color = "black") + # set outline color
  theme_void() + # remove basically anything but the map
  coord_map() # will give a mercator projection by default


# Make map of California including counties, with sea star abundance data

ca_county <- pop %>%
  select("subregion" = County, Population)  %>% # rename county column and keep population column
  inner_join(counties) %>% # inner join with county data set
  filter(region == "california") # some different states have the same county names

ggplot() +
  geom_polygon(data = ca_county, 
               aes(x = long, 
                   y = lat, 
                   group = group, # avoid abstract map art
                   fill = Population), # fill by county population
               color = "black") + # outline color
  geom_point(data = star, # add a point for each sea star site
             aes(x = long,
                 y = lat,
                 size = star_no),  # change point size by sea star abundance
             alpha = 0.75) + # change transparency so I can see things
  coord_map() +
  theme_void() +
  scale_fill_gradient(trans = "log10") + # make the scale fill logarithmic for easier interpretation
  labs(size = 'Stars /'~m^2) # change legend title















