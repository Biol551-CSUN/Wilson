### This is a script for Spring 2021 Computer Modeling covering the iterative coding in R lab
### Created by: Emily Wilson
### Created on: 04/26/21
############################################################################################

# Load libraries ###########################################################################
library(here)
library(tidyverse)


# Start iterating! ##########################################################################

# Using a for loop:

homework_path <- here("Week_13", "data", "homework") # set path
files <- dir(path = homework_path, pattern = ".csv") # use regex to specifically select file names ending in .csv
homework_data <- data.frame(matrix(nrow = length(files), ncol = 5)) # make an empty dataframe that has one row for each file and 3 columns
colnames(homework_data) <- c("filename","mean_temp", "sd_temp", "mean_light", "sd_light")

raw_data <- read_csv(paste0(homework_path,"/", files[4])) # read in first file to see if it works; it does!
mean_temp <- mean(raw_data$Temp.C, na.rm=TRUE) # calculate a mean; works!
mean_temp


# Turn it into a for loop
for (i in 1:length(files)){ # loop over all the files
  raw_data <- read_csv(paste0(homework_path,"/",files[i])) # add in the loop over the raw data
  homework_data$filename[i] <- files[i] # add in filename for each row
  homework_data$mean_temp[i] <- mean(raw_data$Temp.C, na.rm=TRUE) # calc mean for temp
  homework_data$sd_temp[i] <- sd(raw_data$Temp.C, na.rm=TRUE) # calc sd for temp
  homework_data$mean_light[i] <- mean(raw_data$Intensity.lux, na.rm=TRUE) # calc mean for light
  homework_data$sd_light[i] <- sd(raw_data$Intensity.lux, na.rm=TRUE) # calc sd for light
} 

homework_data # check if it works

# Using {purr}

homework_path2 <- here("Week_13", "data", "homework") # point to file path location
files2 <- dir(path = homework_path2, pattern = ".csv", full.names=TRUE) # select correct files to use
files2 # check if it worked; it did


homework_data2 <- files2 %>%
  set_names("TP1.csv", "TP2.csv", "TP3.csv", "TP4.csv") %>% # set the id of each list to the file name in a nicer-looking way
  map_df(read_csv,.id = "filename") %>% # map everything to a dataframe and put the id in a column called filename
  group_by(filename) %>% # group by file name
  
  summarise(mean_temp = mean(Temp.C, na.rm=TRUE), # calc mean for temperature
            sd_temp = sd(Temp.C, na.rm=TRUE), # calc SD for temperature
            mean_light = mean(Intensity.lux, na.rm=TRUE), # calc mean light
            sd_light = sd(Intensity.lux, na.rm=TRUE)) # calc sd light

homework_data2