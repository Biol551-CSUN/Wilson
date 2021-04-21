### This is a script for Spring 2021 Computer Modeling covering working wth factors in R: lab
### Created by: Emily Wilson
### Created on: 04/21/21
############################################################################################

# Load libraries ###########################################################################
library(tidyverse)
library(here)

# Load data ################################################################################
intertidal <- read_csv(here("Week_12", "data", "intertidaldata.csv"))
int_lat <- read_csv(here("Week_12", "data", "intertidaldata_latitude.csv"))

# Get graphing ##############################################################################


intertidal %>% 
  inner_join(int_lat) %>% 
  mutate(Quadrat = factor(Quadrat)) %>% # make quadrat a factor
  mutate(Site = factor(Site)) %>% # make site a factor, just for the fun of it
  mutate(Quadrat = fct_recode(Quadrat, "Mid" = "Mid  1")) %>% # get rid of weird labels
  mutate(Quadrat = fct_recode(Quadrat, "Low" = "Low  .")) %>% # get rid of weird labels
  mutate(Quadrat = fct_relevel(Quadrat, levels = c("Low", "Mid", "High"))) %>% # relevel from low to high
  droplevels() %>% # drop any weird extra leftover levels
  select(Site, Transect, Latitude, Quadrat, Algae) %>% 
  group_by(Site, Latitude, Quadrat) %>%
  summarise(avg_alg = mean(Algae)) %>% 
  ggplot(aes(x = Quadrat, # plot quadrat on x axis
           y = fct_reorder(Site, Latitude), # and site on y axis; reorder by latitude
           fill = avg_alg)) + # fill with algal abundances
  geom_tile() + # make it a tile chart
  coord_equal() + # make the tiles perfect boxes
  scale_fill_viridis_c(option = "mako") + # fill with one of the new viridis colors!!!
  theme_minimal() + # nice minimal theme
  labs(title = "Intertidal Algal Abundances", # add plot title
       subtitle = "Average algal abundances across tidal heights", # add subtitle
       y = "Site",
       fill = "Algae") + # change legend
  theme(plot.title = element_text(hjust = 0.6), # center plot title
        plot.subtitle = element_text(hjust = 0.6)) + 
  ggsave(here("Week_12", "outputs", "intertidalalgae.png"), width = 4, height = 5.5, unit = "in") # save to outputs folder for this week
 

