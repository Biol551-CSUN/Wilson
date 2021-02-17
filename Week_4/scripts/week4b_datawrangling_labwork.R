### This is a script for Spring 2021 Computer Modeling containing lab work for wrangling data with tidyr
### Created by: Emily Wilson
### Created on: 02/17/2021
##########################################################################################################

### Load libraries #########################################################################
library(tidyverse)
library(here)

### Read in data ###########################################################################

chem <- read.csv(here("Week_4", "data", "chemicaldata_maunalua.csv"))
view(chem)

### Start wrangling! #######################################################################

# For calculating summary stats:
chem_tidy <- chem %>% # assign to new df
  filter(complete.cases(.)) %>% # remove incomplete cases
  separate(Tide_time, # choose column I want to separate
           c("tide", "time")) %>%  # tell R what to separate that column into
  select(Zone, tide, Salinity, percent_sgd) %>% #separate out zones, tides, and freshwater-related parameters
  pivot_longer(cols = Salinity:percent_sgd, # pivot longer; give cols to pivot
               names_to = "parameters", # tell R what to name the new column
               values_to = "measurements") %>% # tell R what to name the column where it'll put the values
  group_by(Zone, tide, parameters) %>% # group by pretty much everything
  summarise(param_mean = mean(measurements), # calculate mean
            param_sd = sd(measurements), # calculate SD
            param_var = var(measurements)) %>% # calculate variance
  write_csv(here("Week_4","outputs","lab_summary.csv")) %>% # save summary table as csv
  filter(parameters == "percent_sgd") # filter out so all I have is salinity data for graphing

view(chem_tidy)
 

# For jitter:
chem_jitter <- chem %>% # assign to new df
  separate(Tide_time, # choose column I want to separate
           c("tide", "time")) %>%  # tell R what to separate that column into
  filter(complete.cases(.)) %>% # remove incomplete cases
  select(Zone, tide, percent_sgd) # select only things to use in plot

view(chem_jitter)


### Graphing: #######################################################################
  
 ggplot(data = chem_tidy, #use summary data for bars
        aes(x = Zone, y = param_mean, # give x and y values
            fill = tide)) + # fill by tide
  geom_col(position = "dodge") + # get clustered rather than stacked bars
   geom_jitter(data = chem_jitter, # use overall data for jitter
               aes(x = Zone, y = percent_sgd), # assign jitter x and y
               alpha = 0.5, # make points slightly transparent for clarity
               position = position_jitterdodge(dodge = 1, jitter.width = 0.2)) + # mess with jitter position
  theme_classic() + # classic theme to get rid of gross-looking elements
  scale_y_continuous(limits = c(0, 27), expand = c(0, 0)) + #remove blank space
  labs(x = "Discharge Zone", # alter x axis label
       y = "Average Groundwater Discharge", # alter y axis label
       title = "Groundwater Discharge by Zone in Mauna Lua", # add title
       caption = "Source: CSUN Silbiger Lab", # add caption
       fill = "Tide") + # capitalize legend title
  scale_fill_manual(values = c("lightblue4", "lightcyan2")) + # change colors
  ggsave(here("Week_4", "outputs", "maunaluaplot.png"), width = 7, height = 4)
      




