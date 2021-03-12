### This is a script for Spring 2021 Computer Modeling covering making maps in R using ggmaps
### Created by: Emily Wilson
### Created on: 03/10/21
############################################################################################

### Load libraries #########################################################################

library(ggmap)
library(tidyverse)
library(here)
library(ggsn)


### Read in data ###########################################################################

chemdata<-read_csv(here("Week_7","data","chemicaldata_maunalua.csv"))


### Start mapping! #########################################################################

oahu <- get_map("Oahu") # get map for oahu

ggmap(oahu) # plot base layer for oahu map using ggmap

wp <- data.frame(lon = -157.7621, lat = 21.27427) # put in coordinates for Wailupe by making df of lat and long coordinates

wpmap <- get_map(wp) # get a base layer for the map

wpmap <- get_map(wp, zoom = 17) # zoom in on map (3-20, 3 being continent level and 20 being building level)

wpmap <- get_map(wp, zoom = 17, maptype = "satellite") # change map type to satmap

wpmap <- get_map(wp, zoom = 17, maptype = "watercolor") # change map type to watercolor

ggmap(wpmap)


# Plots using ggmao

ggmap(wpmap) + # can use ggmap base for any ggplot
  geom_point(data = chemdata, 
             aes(x = Long, 
                 y = Lat,
                 color = Salinity), 
             size = 2) + # point size
  scale_color_viridis_c() + # viridis palette
  scalebar(x.min = -157.766, x.max = -157.758, # add scale bar
            y.min = 21.2715, y.max = 21.2785,
            dist = 250, dist_unit = "m", model = "WGS84", 
            transform = TRUE, st.color = "white",
            box.fill = c("yellow", "white")) 


# Getting exact locations for maps using geocode()

geocode("the white house")

geocode("California State University, Northridge")

