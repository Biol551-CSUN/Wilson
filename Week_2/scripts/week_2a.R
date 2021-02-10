### This is the first script of Spring 2021 Computer Modeling
### Created by: Emily Wilson
### Created on: 02/03/2021
################################################################

### Load libraries #############################################
library(tidyverse)
library(here)


### Read in data ###############################################
weightdata <- read.csv(here("Week_2", "data", "weightdata.csv"))


### Data Analysis ##############################################
head(weightdata)
tail(weightdata)
view(weightdata)
