### This is a script for Spring 2021 Computer Modeling covering data wrangling for times and dates using lubridate
### Created by: Emily Wilson
### Created on: 02/24/2021
######################################################################################################################

### Load libraries ##################################################################################################

library(tidyverse)
library(lubridate)
library(here)

### Read in data ####################################################################################################

cond <- read.csv(here("Week_5", "data", "conddata.csv"))
depth <- read.csv(here("Week_5", "data", "depthdata.csv"))


### Start wrangling! ################################################################################################

# date and time are already in the correct formats

depth <- depth %>% 
  mutate(date = ymd_hms(date)) # convert date column

combo <- cond %>% 
  mutate(date = ymd_hms(date), # convert date column
         date = round_date(date, "10s")) %>%  #%>% # round to nearest ten seconds
  inner_join(depth) %>%  # join (does it by date automatically)
  mutate(time = paste(hour(date), minute(date), sep = ":")) # put hour and minute into new column

summary <- combo %>% 
  group_by(time) %>% # group by minute
  summarise(date_avg = mean(date), # get average date
            depth_avg = mean(Depth), # get average depth
            temp_avg = mean(TempInSitu), # get average temp
            sal_avg = mean(SalinityInSitu_1pCal)) # get average salinity

ggplot(summary,
       aes(x = depth_avg, # assign aesthetics
           y = temp_avg)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) + # set 2d density plot
  scale_fill_viridis_c(option = "A", "Frequency", breaks = c(3, 6, 9, 12)) + # fill with viridis
  labs(title = "Frequency of Temperatures by Depth", # add title
       x = "Average Depth (m)", # change x acis label
       y = "Average Temp (°C)") + # change y axis label
  scale_x_continuous(expand = c(0,0)) + # get rid of weird empty margins
  scale_y_continuous(expand = c(0,0)) + # get rid of weird empty margins
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + # give wider plot margin
  ggsave(here("Week_5", "outputs", "lubridate_labplot.png"), width = 5, height = 3, unit = "in") # save to outputs folder for this week













