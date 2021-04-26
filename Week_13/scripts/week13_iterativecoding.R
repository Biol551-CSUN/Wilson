### This is a script for Spring 2021 Computer Modeling covering iterative coding in R
### Created by: Emily Wilson
### Created on: 04/26/21
############################################################################################

# Load libraries ###########################################################################
library(here)
library(tidyverse)


# Start iterating! ##########################################################################

print(paste("The year is", 2000)) # start with basic piece of code w/out iteration

years<-c(2015:2021) # set up iteration

for (i in years){ # set up the for loop where i is the index
  print(paste("The year is", i)) # loop over i
}

# Printed but didn't save anywhere

year_data<-data.frame(matrix(ncol = 2, nrow = length(years))) # pre-allocate space for the for loop; length(years) lets it be dynamic instead of hard-coding it in

colnames(year_data)<-c("year", "year_name") # add column names

for (i in 1:length(years)){ # set up the for loop where i is the index
  year_data$year_name[i]<-paste("The year is", years[i]) # loop over year name; saves to year_name column
  year_data$year[i]<-years[i] # loop over year; saves to year column
}

year_data # look at filled matrix

# Reading in multiple csvs

CondPath <- here("Week_13", "data", "cond_data") # point to the location on the computer of the folder
# list all the files in that path with a specific pattern
# In this case we are looking for everything that has a .csv in the filename

files <- dir(path = CondPath,pattern = ".csv") # you can use regex to be more specific if you are looking for certain patterns in filenames

files


# pre-allocate space
cond_data<-data.frame(matrix(nrow = length(files), ncol = 3)) # make an empty dataframe that has one row for each file and 3 columns

colnames(cond_data)<-c("filename","mean_temp", "mean_sal") # give the dataframe column names

cond_data


# Write basic code to calculate a mean and build out
raw_data <- read.csv(paste0(CondPath,"/",files[1])) # test by reading in the first file and see if it works
head(raw_data)
mean_temp <- mean(raw_data$Temperature, na.rm = TRUE) # calculate a mean
mean_temp


# Turn it into a for loop
for (i in 1:length(files)){ # loop over 1:3 the number of files
  raw_data <- read.csv(paste0(CondPath,"/",files[i])) # add in the loop over the raw data
  cond_data$filename[i] <- files[i] # add in filename for each row
  cond_data$mean_temp[i] <- mean(raw_data$Temperature, na.rm =TRUE) # calc mean
  cond_data$mean_sal[i] <- mean(raw_data$Salinity, na.rm =TRUE) # calc salinity
} 

cond_data


# {purr}

1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15)  %>% # calculate 15 random numbers based on a normal distribution in a list 
  map_dbl(mean) # calculate the mean. It is now a vector which is type "double"

1:10 %>% # list 1:10
  map(function(x) rnorm(15, x)) %>% # make your own function
  map_dbl(mean)

1:10 %>% # Use a formula when you want to change the arguments within the function
  map(~ rnorm(15, .x)) %>% # changes the arguments inside the function
  map_dbl(mean)

# point to the location on the computer of the folder
CondPath<-here("Week_13", "data", "cond_data")
files <- dir(path = CondPath,pattern = ".csv", full.names = TRUE) # get the full file names in one less step
#save the entire path name
files

data <- files %>% # read in the files using map instead of a for loop while retaining the filename as a column
  set_names() %>% # sets the id of each list to the file name
  map_df(read_csv,.id = "filename") # map everything to a dataframe and put the id in a column called filename

data


data <- files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") %>% # map everything to a dataframe and put the id in a column called filename
  group_by(filename) %>% # group by file name
  summarise(mean_temp = mean(Temperature, na.rm = TRUE), # calc mean
            mean_sal = mean(Salinity,na.rm = TRUE)) # calc sal

data










