# Week 10b Lab: Shiny App

library(shiny)
library(here)
library(ggrepel)
library(tidyverse)
library(lubridate)
library(shinythemes)

baby <- read.csv(here("week_10", "data", "HatchBabyExport.csv")) # read data

# Define UI
ui <- fluidPage(
    theme = shinytheme("sandstone"), # change shiny theme
    
    # Application title
    titlePanel("Hatch Baby Data"), 
    
    # Sidebar with radio buttons for the two babies
    sidebarLayout(
        sidebarPanel(
            radioButtons( # use radio buttons for sidebar choices
                inputId = "Baby.Name", # which ID you want to use for the input
                label = "Baby:", # what you want the label for the sidebar choices to be
                choices = unique(baby$Baby.Name), # give the option of both baby names as choices
                selected = "Blakely" # make Blakely the default selection
            ),
        ),
        
        # Show plots of baby metrics in the main panel
        mainPanel(
            column(12, # make the column wide enough that each plot is just on its own row and is big enough to see
                   plotOutput("weightplot")), # make the first plot to show up baby weight
            p(), # a line break
            p(), # a line break
            column(12, # make the column wide enough that each plot is just on its own row and is big enough to see
                   plotOutput("consumpplot")), # make second plot the amount of food consumed
            p(), # a line break
            p(), # a line break
            column(12, # make the column wide enough that each plot is just on its own row and is big enough to see
                   plotOutput("diaperplot")) # make the third plot the diaper info
        ) # /mainPanel
    )
) # /fluidPage

# Server logic
server <- function(input, output) {
    output$weightplot <- renderPlot({ # define what the output will be for the weight plot we called in the UI section
        baby %>%
            mutate( # get the date into something workable with my plotting plan
                date = mdy_hm(Start.Time),
                day = paste(year(date), month(date), day(date), sep = "-"),
                day = ymd(day)
            ) %>%
            select(Baby.Name, day, Activity, Amount) %>% # only keep the columns I want to use
            filter(Activity == "Weight") %>% # only keep weight data for this plot
            filter(Baby.Name == input$Baby.Name) %>% # filter baby name by which baby you choose in the sidebar
            mutate(Amount = (as.numeric(Amount))) %>% # make the weight numeric (it was a character before)
            mutate_if(is.numeric, round, digits = 2) %>% # round the weight to a reasonable number of decimal places
            ggplot(aes(x = day, y = Amount)) + # plot the weight of the baby by the day
            geom_point(size = 3, color = "slateblue") + # get scatterplot
            geom_label_repel(aes(label = Amount), point.padding = 0, box.padding = 0.3) + # add weight labels
            theme_classic() + # add theme
            labs(title = "Weight", # title graph
                 x = "Date", # capitalize x axis title
                 y = "Weight (lbs)") + # modify y axis title
            scale_x_date(date_breaks = "5 days", date_labels = "%m/%d") + # fix the x scale and display
            theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), # give the plots a bit more space
                  plot.title = element_text(size = 16, face = "bold"), # make title larger
                  axis.title.x = element_text(size = 14), # make x axis title larger
                  axis.title.y = element_text(size = 14), # make y axis title larger
                  axis.text.x = element_text(size = 11), # make axis text larger
                  axis.text.y = element_text(size = 11)) # make axis text larger
    })
    
    output$consumpplot <- renderPlot({ # define what the output will be for the food consumption plot we called in the UI section
        baby %>%
            mutate( # get the date into something workable with my plotting plan
                date = mdy_hm(Start.Time),
                day = paste(year(date), month(date), day(date), sep = "-"),
                day = ymd(day)
            ) %>%
            select(Baby.Name, day, Activity, Amount) %>% # only keep the columns I want to use
            group_by(day) %>% # group by day so there will be one boxplot of feeding data per day in the final output
            filter(Activity == "Feeding") %>% # only keep food consumption data for this plot
            filter(Baby.Name == input$Baby.Name) %>% # filter baby name by which baby you choose in the sidebar
            filter(Amount > "0") %>% # only keep feedings where they at least ate something, otherwise the boxplots are a mess
            mutate(Amount = (as.numeric(Amount))) %>% # make the weight numeric (it was a character before)
            mutate_if(is.numeric, round, digits = 2) %>% # round the amount consumed to a reasonable number of decimal places
            ggplot(aes( # plot the food consumption of the baby by the day
                x = day,
                y = Amount,
                group = day
            )) +
            geom_boxplot(color = "royalblue4", fill = "azure2") + # set graph type as a boxplot
            scale_x_date(date_breaks = "5 days", date_labels = "%m/%d") + # fix the x scale and display
            theme_classic() + # set theme
            labs(title = "Feeding", # title graph
                 x = "Date", # capitalize x axis title
                 y = "Amount Consumed (oz)") + # modify y axis title
            theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), # give the plots a bit more space
                  plot.title = element_text(size = 16, face = "bold"), # make title larger
                  axis.title.x = element_text(size = 14), # make x axis title larger
                  axis.title.y = element_text(size = 14), # make y axis title larger
                  axis.text.x = element_text(size = 11), # make axis text larger
                  axis.text.y = element_text(size = 11)) # make axis text larger
    })
    
    output$diaperplot <- renderPlot({ # define what the output will be for the diaper status plot we called in the UI section
        baby %>%
            mutate( # get the date into something workable with my plotting plan
                date = mdy_hm(Start.Time),
                day = paste(year(date), month(date), day(date), sep = "-"),
                day = ymd(day)
            ) %>%
            filter(Activity == "Diaper") %>% # only keep diaper data for this plot
            filter(Baby.Name == input$Baby.Name) %>% # filter baby name by which baby you choose in the sidebar
            ggplot(aes( # plot the occurrence of each diaper state by day
                x = day,
                y = Amount
            )) + 
            geom_violin(fill = "darkolivegreen", alpha = 0.3) + # make a viiolin plot
            geom_jitter(aes(x = day, y = Amount), # overlay jittered points so they can visualize the sheer number of diapers they've dealt with
                        alpha = 0.75, # make points slightly transparent for clarity
                        color = "darkolivegreen4",
                        position = position_jitter(height = 0.1)) + # mess with jitter position
            scale_x_date(date_breaks = "5 days", date_labels = "%m/%d") + # fix the x scale and display
            theme_classic() + # set theme
            labs(title = "Diapers", # title graph
                 x = "Date", # capitalize x axis title
                 y = "Type of Dirty Diaper") + # modify y axis title
            theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), # give the plots a bit more space
                  plot.title = element_text(size = 16, face = "bold"), # make title larger
                  axis.title.x = element_text(size = 14), # make x axis title larger
                  axis.title.y = element_text(size = 14), # make y axis title larger
                  axis.text.x = element_text(size = 11), # make axis text larger
                  axis.text.y = element_text(size = 11)) # make axis text larger
    })
}

# Run the application
shinyApp(ui = ui, server = server)
