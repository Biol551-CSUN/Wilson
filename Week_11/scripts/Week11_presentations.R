### This is a script for Spring 2021 Computer Modeling for group presentation exercises
### Created by: Emily Wilson
### Created on: 03/08/21
############################################################################################

# wHeatmap

# Install libraries #######################################################################
library(devtools)
library(wheatmap)
library(here)


# Load data ###############################################################################

mtcars_matrix <- as.matrix(mtcars)
mtcars_scale <- scale(mtcars_matrix)
head(mtcars_scale)

heatmap <- both.cluster(mtcars_scale) #cluster data
heatmap$mat[,1:4]

# Heat maps
BaseHeatmap <- WHeatmap(heatmap$mat, name ='h1',  #create the heatmap and name it h1
                        yticklabels = TRUE, yticklabel.side = 'b', yticklabel.fontsize=11, #add axis labels
                        xticklabels = TRUE, xticklabel.side = 'b', xticklabel.fontsize = 20, #add axis labels
                        cmp = CMPar(brewer.name = 'BuPu')) #change color palette
BaseHeatmap

EmilyHeatmap <- WHeatmap(heatmap$mat, name = "Emily's Plot",  # create the heatmap and rename it
                        yticklabels = TRUE, yticklabel.side = 'b', yticklabel.fontsize=11, # add axis labels
                        xticklabels = TRUE, xticklabel.side = 'a', xticklabel.fontsize = 20, # move x axis labels to top
                        cmp = CMPar(brewer.name = 'Spectral')) #change color palette to Spectral
EmilyHeatmap

# Add dendrogram to heat map
Dendrogram <- BaseHeatmap + # add axis labels
  WDendrogram(heatmap$row.clust, 
              LeftOf('h1'), # placement of dendrogram
              facing='right') 
Dendrogram

EmilyHeatmap2 <- BaseHeatmap + # add axis labels
  WDendrogram(heatmap$column.clust, 
              TopOf('h1'), # placement of dendrogram
              facing='bottom')
EmilyHeatmap2

# Add legend
Legend <- Dendrogram + #add row dendrogram to right of heatmap
  WLegendV('h1', BottomRightOf('h1', h.pad=.4), 'l1') #add legend

Legend

Legend_left <- Dendrogram + #add row dendrogram to right of heatmap
  WLegendV('h1', BottomRightOf('h1', h.pad = -0.3), 'l1') #add legend

Legend_left

# Highlight
Highlight <- Legend +
  WRect('h1', c(2,5), c(2,3), col='yellow') #highlight certain cells

Highlight


# rinat ################################################################################################

library(rinat)
library(tidyverse)

get_inat_obs(query = "Parrotfishes", # any entry that mentions Parrotfishes 
             quality = "research") # only research grade observations

get_inat_obs(query = "kelp forest")

get_inat_obs(taxon_name = "Isocoma menziesii", # search for Coastal Goldenbush using species name
             quality = "research") # only research grade observations

get_inat_obs(taxon_name = "Egregia menziesii", # search for feather boa kelp using species name
             quality = "research") # only research grade observations

get_inat_obs(taxon_id = 53353, # id number from iNaturalist
             quality = "research") # only research grade observations

get_inat_obs(place_id = 123751)

Mule_deer<- get_inat_obs(query = "Mule Deer", # Search for Mule deer 
                         bounds = c(38.44047, -125, 40.86652, -121.837)) # Creating a bounding box of Northern California


Mule_deer <- get_inat_obs(query = "Mule Deer", # Search for Mule deer 
                          bounds = c(38.44047, -125, 40.86652, -121.837), # In Northern California
                          quality = "research", # research grade observations
                          year = 2019) 

plot(Mule_deer$longitude, Mule_deer$latitude)


# Janitor

library(janitor)
library(tidyverse)
library(here)
library(readxl)
library(kableExtra)

coralgrowth <- readr::read_csv('https://raw.githubusercontent.com/Biol551-CSUN/Janitor_Package/main/project/Data/CoralGrowth.csv')
corals_messy <- readr::read_csv('https://raw.githubusercontent.com/Biol551-CSUN/Janitor_Package/main/project/Data/coraldata.csv')

corals_messy%>%
  kbl() %>% # make a table in RMarkdown
  kable_classic()%>% #  Theme of the table
  kable_styling() %>% 
  scroll_box(width = "700px", height = "300px")# Table dimensions

corals<-clean_names(corals_messy)

corals %>%
  kbl() %>% # make a table in RMarkdown
  kable_classic()%>% # the theme of the table
  kable_styling() %>%
  scroll_box(width = "700px", height = "300px") # table dimensions 

tabyl(corals, change_mg_cm2)%>% #we can put the object into a frequency table
  kbl() %>% # make a table in RMarkdown
  kable_classic()%>% # theme of the table
  kable_styling() %>% 
  scroll_box(width = "300px", height = "300px")# table dimensions

corals %>%
  tabyl(change_mg_cm2)%>%  ### The pipe version ###
  kbl() %>% # make a table in RMarkdown
  kable_classic()%>% # theme of table
  kable_styling() %>%
  scroll_box(width = "300px", height = "300px")# table dimensions

#first make a simple data frame 
a <- data.frame(v1 = c(7, 6, 4, 5),
                v2 = c(NA, NA, NA, NA),
                v3 = c("a", "b", "c", "d"), 
                v4 = c(6, 5, 8, 10))
a %>% #data frame name
  kbl()%>% #make a table in RMarkdown
  kable_classic()%>% #classic theme
  kable_styling(full_width = FALSE, position = "left")

a_clean<-a %>% #rename data frame
  remove_empty(c("rows", "cols")) #checks for empty columns and rows, removes them!
a_clean%>%  #data frame name 
  kbl()%>% #make a table in RMarkdown
  kable_classic()%>% #classic theme
  kable_styling(full_width = FALSE, position = "left")

coralgrowth %>% # data frame name
  kbl() %>% # make a table in RMarkdown
  kable_classic()%>%
  kable_styling() %>%
  scroll_box(width = "700px", height = "300px") # classic theme


# Plotly

library(tidytuesdayR)
library(tidyverse)
library(here)

# load Tidy Tuesday data
tuesdata <- tidytuesdayR::tt_load('2020-02-18')

food.data<-tuesdata$food_consumption
glimpse(food.data)


library(plotly)

fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption)
fig


fig2 <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category) # group by color
fig2

fig3 <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category,
               colors = "viridis") # change color palette
fig3

fig4 <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~co2_emmission, # can also group by continuous variable
               colors = "viridis")
fig4

fig5 <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category,
               colors = "viridis",
               marker = list(size = 10), #change the marker size
               text = ~paste("CO2 emission:", co2_emmission, #change the hover data labels
                             "<br>Consumption:", consumption, #<br> moves the label to a new line
                             "<br>Food category:", food_category)) %>% #must PIPE to the layout 
  layout(title = 'CO2 Emissions and Food Consumption by Food Type', #add a title
         xaxis = list(title = "CO2 Emission"), #change the axes titles
         yaxis = list(title = "Consumption"),
         legend = list(title = list(text = 'Food Category'))) #add a legend title
fig5

fig6 <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category,
               colors = "viridis",
               mode = 'lines', #change to line graph
               text = ~paste("CO2 emission:", co2_emmission, 
                             "<br>Consumption:", consumption, 
                             "<br>Food category:", food_category)) %>% 
  layout(title = 'CO2 Emissions and Food Consumption by Food Type', 
         xaxis = list(title = "CO2 Emission"), 
         yaxis = list(title = "Consumption"),
         legend = list(title = list(text = 'Food Category'))) 

fig6

bar <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China", #select countries
         food_category == "Beef") #select 1 food category

bar <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China",
         food_category == "Beef") %>%
  plot_ly(x = ~country, #create bar chart showing beef consumption by country
          y = ~consumption,
          type = "bar")

bar

bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") #filter out 3 countries of interest

bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") %>% 
  group_by(country) %>% 
  mutate(total_consumption = (sum(consumption)),
         percent_consumption = ((consumption/total_consumption)*100)) #calculate amount of each food consumed as percent of total consumption

bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") %>% 
  group_by(country) %>% 
  mutate(total_consumption = (sum(consumption)),
         percent_consumption = ((consumption/total_consumption)*100)) %>%
  plot_ly(x = ~country, #countries on x-axis
          y = ~percent_consumption, #percent consumption on y-axis
          color = ~food_category, #color by type of food
          type = "bar") #make it a bar chart

bar_stacked

bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") %>% 
  group_by(country) %>% 
  mutate(total_consumption = (sum(consumption)),
         percent_consumption = ((consumption/total_consumption)*100)) %>%
  plot_ly(x = ~country,
          y = ~percent_consumption,
          color = ~food_category,
          text = ~paste("Total consumption:", total_consumption, "kg/person/year"), #customize hover label text
          type = "bar")

bar_stacked

bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") %>% 
  group_by(country) %>% 
  mutate(total_consumption = (sum(consumption)),
         percent_consumption = ((consumption/total_consumption)*100)) %>%
  plot_ly(x = ~country,
          y = ~percent_consumption,
          color = ~food_category,
          text = ~paste("Total consumption:", total_consumption, "kg/person/year"),
          type = "bar") %>% 
  layout(barmode = "stack", #stack bars
         title = "Percent of food comsumed by country", #change plot title
         xaxis = list(title = "Country"), #change x-axis title
         yaxis = list(title = "Percent of total consumption")) #change y-axis title

bar_stacked

dropdown <- food.data %>% # work up to dropdown menu
  filter(country == "USA") %>% #filter out USA
  plot_ly(x = ~food_category) #plot with food category on x-axis

dropdown <- food.data %>%
  filter(country == "USA") %>%
  plot_ly(x = ~food_category) %>%
  add_bars(y = ~consumption, name = "Food Consumption") #create bar graph

dropdown <- food.data %>%
  filter(country == "USA") %>%
  plot_ly(x = ~food_category) %>%
  add_bars(y = ~consumption, name = "Food Consumption") %>%
  add_bars(y = ~co2_emmission, name = "CO2 Emissions", visible = FALSE) #create second bar graph, make invisible

dropdown <- food.data %>%
  filter(country == "USA") %>%
  plot_ly(x = ~food_category) %>%
  add_bars(y = ~consumption, name = "Food Consumption") %>%
  add_bars(y = ~co2_emmission, name = "CO2 Emissions", visible = FALSE) %>%
  layout(updatemenus = list(list(y = 0.6, #set vertical position of menu
                                 x = -0.2, #set horizontal position of menu
                                 buttons = list(list(method = "restyle", #use "buttons" to add the 2 different menu options
                                                     args = list("visible", list(TRUE, FALSE)), #show first plot, hide second plot
                                                     label = "Consumption"),
                                                list(method = "restyle",
                                                     args = list("visible", list(FALSE, TRUE)), #hide first plot, show second plot
                                                     label = "CO2 Emissions")))))

dropdown # finally, a dropdown!

# Chloropleth
food.data<-tuesdata$food_consumption

# Load plotly map code dataframe
map.codes <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
glimpse(map.codes)

food.data <- map.codes %>% 
  rename(country = COUNTRY) %>% # rename column heading for easier join
  right_join(food.data) %>%  # join to food.data dataframe by 'country'
  select(-GDP..BILLIONS.)

# check if any countries were unsuccessfully joined
food.data %>% 
  mutate(na_col = is.na(food.data$CODE)) %>% 
  filter(na_col == TRUE) %>% 
  distinct(country)

US <- food.data %>% 
  filter(country == "USA") %>% 
  mutate(CODE = 'USA',
         country = 'United States')

# Add recoded USA to dataframe and summarize mean CO2 emmissions
food.data <- food.data %>% 
  full_join(US) %>% 
  drop_na()

# Get mean CO2 Emission values for each country
co2.data <- food.data %>% 
  group_by(country, CODE) %>% 
  summarise(mean_co2 = mean(co2_emmission)) %>% 
  ungroup()

full.data <- food.data %>% 
  select(-consumption) %>% 
  left_join(co2.data)

wide.data <- full.data %>%
  pivot_wider(names_from = food_category, values_from = co2_emmission) %>%
  drop_na()

plot_geo(wide.data) %>% # create the world map using our dataframe
  add_trace(locations = ~CODE, # Spatial coordinates; distributes data points by location
            color = ~mean_co2, # Colors data points by the values given
            z = ~mean_co2, # Distributes color within each location boundary
            text = ~paste(country,
                          "<br>Mean CO2 emission:", mean_co2, #change the hover data labels
                          "<br>Pork:", Pork, #<br> moves the label to a new line
                          "<br>Poultry:", Poultry,
                          "<br>Beef:", Beef,
                          "<br>Fish:", Fish,
                          "<br>Eggs:", Eggs,
                          "<br>Soybeans:", Soybeans), 
            hovertemplate = paste('%{text}'), # remove the top line showing value from z
marker = list(line = list(color = toRGB("grey63"), width = 0.5))) # Sets location boundary color and thickness

plot_geo(wide.data) %>% # create the world map using our dataframe
  add_trace(locations = ~CODE, # Spatial coordinates; distributes data points by location
            color = ~mean_co2, # Colors data points by the values given
            z = ~mean_co2, # Distributes color within each location boundary
            text = ~paste(country,
                          "<br>Mean CO2 emission:", mean_co2, #change the hover data labels
                          "<br>Pork:", Pork, #<br> moves the label to a new line
                          "<br>Poultry:", Poultry,
                          "<br>Beef:", Beef,
                          "<br>Fish:", Fish,
                          "<br>Eggs:", Eggs,
                          "<br>Soybeans:", Soybeans), 
            hovertemplate = paste('%{text}'),
            marker = list(line = list(color = toRGB("black"), width = 1))) %>% # Sets location boundary color and thickness
  layout(title = 'Global Annual Carbon Emmissions', # add title
         geo = list(showframe = F, # remove box border around map
                    showcoastlines = T, # If F, removes all other outlines not included in our location list
                    showocean = T, oceancolor="LightBlue", # Includes ocean boundaries in map and fills in color
                    projection = list(type = 'mercator')))  # set map projection

plot_geo(wide.data) %>% # create the world map using our dataframe
  add_trace(locations = ~CODE, # Spatial coordinates; distributes data points by location
            color = ~mean_co2, # Colors data points by the values given
            z = ~mean_co2, # Distributes color within each location boundary
            text = ~paste(country,
                          "<br>Mean CO2 emission:", mean_co2, #change the hover data labels
                          "<br>Pork:", Pork, #<br> moves the label to a new line
                          "<br>Poultry:", Poultry,
                          "<br>Beef:", Beef,
                          "<br>Fish:", Fish,
                          "<br>Eggs:", Eggs,
                          "<br>Soybeans:", Soybeans), 
            hovertemplate = paste('%{text}'),
            marker = list(line = list(color = toRGB("black"), width = 1))) %>% # Sets location boundary color and thickness
  layout(title = 'Global Annual Carbon Emmissions', # add title
         geo = list(showframe = F, # remove box border around map
                    showcoastlines = T, # If F, removes all other outlines not included in our location list
                    showocean = T, oceancolor="LightBlue", # Includes ocean boundaries in map and fills in color
                    projection = list(type = 'robinson')))  # set different map projection

plot_geo(wide.data) %>% # create the world map using our dataframe
  add_trace(locations = ~CODE, # Spatial coordinates; distributes data points by location
            color = ~mean_co2, # Colors data points by the values given
            z = ~mean_co2, # Distributes color within each location boundary
            text = ~paste(country,
                          "<br>Mean CO2 emission:", mean_co2, #change the hover data labels
                          "<br>Pork:", Pork, #<br> moves the label to a new line
                          "<br>Poultry:", Poultry,
                          "<br>Beef:", Beef,
                          "<br>Fish:", Fish,
                          "<br>Eggs:", Eggs,
                          "<br>Soybeans:", Soybeans), 
            hovertemplate = paste('%{text}'),
            marker = list(line = list(color = toRGB("black"), width = 1))) %>% # Sets location boundary color and thickness
  layout(title = 'Global Annual Carbon Emmissions', # add title
         geo = list(showframe = F, # remove box border around map
                    showcoastlines = T, # If F, removes all other outlines not included in our location list
                    showocean = T, oceancolor="LightBlue", # Includes ocean boundaries in map and fills in color
                    projection = list(type = 'robinson'))) %>% # set map projection
  colorbar(title = 'CO2 Emmissions<br />(kg CO2/person/year)') # legend title (added break line)










