### This is a script for Spring 2021 Computer Modeling covering getting help for R
### Created by: Emily Wilson
### Created on: 04/05/21
############################################################################################

# Load libraries ###########################################################################
library(tidyverse)

# Making reprexes ###########################################################################

# Highlight and use reprex add-in to get reprex
mpg %>%
  ggplot(aes(x = displ, y = hwy)) %>%
  geom_point(aes(color = class))

# from stars data frame; not going to work, b/c it's a train wreck
lat	long	star_no 
33.548	-117.805	10
35.534	-121.083	1
39.503	-123.743	25
32.863	-117.24	22
33.46	-117.671	8
33.548	-117.805	3

# Use datapasta to copy and paste code snippets as a tribble
data <- tibble::tribble(
  ~lat,    ~long, ~star_no,
  33.548, -117.805,      10L,
  35.534, -121.083,       1L,
  39.503, -123.743,      25L,
  32.863,  -117.24,      22L,
  33.46, -117.671,       8L,
  33.548, -117.805,       3L
)










