### This is a script for Spring 2021 Computer Modeling covering functional programming in R
### Created by: Emily Wilson
### Created on: 03/24/21
############################################################################################

# Load libraries ###########################################################################

library(tidyverse)
library(palmerpenguins)
library(PNWColors) # for the PNW color palette 


# Start Writing ##########################################################################

df <- tibble::tibble( # make a data frame of random numbers
  a = rnorm(10), # draws 10 random values from a normal distribution
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10))


df <- df %>% # rescale every column individually
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)), 
         b = (b-min(b, na.rm = TRUE))/(max(b, na.rm = TRUE)-min(b, na.rm = TRUE)),
         c = (c-min(c, na.rm = TRUE))/(max(c, na.rm = TRUE)-min(c, na.rm = TRUE)),
         d = (d-min(d, na.rm = TRUE))/(max(d, na.rm = TRUE)-min(d, na.rm = TRUE)))

# could just copy and paste that for a-d, but might make a mistake; can write a function to rescale instead

rescale01 <-   function(x) { # pick a name for the new function, then list inputs/arguments of the function inside function
  value <- (x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE)) # place developed code in the body of the function
  return(value) # tell it what values we want returned when you run the function
} 


df %>% # try it out
  mutate(a = rescale01(a),
         b = rescale01(b),
         c = rescale01(c),
         d = rescale01(d))


# Make a function to convert degrees Fahrenheit to Celsius


temp_c <- (temp_f - 32) * 5 / 9 # equation for conversion


fahrenheit_to_celsius <- function(temp_f) { # name function, then list input
  temp_c <- (temp_f - 32) * 5 / 9 # put in the equation
  return(temp_c) # tell it what to return
}


fahrenheit_to_celsius(32) # try it out

fahrenheit_to_celsius(212)


# Make a function to covert degrees Celsius to Kelvin

temp_k <- (temp_c + 273.15) # equation for conversion

celsius_to_kelvin <- function(temp_c) { # name the function and give it the input
  temp_k <- (temp_c + 273.15) # put in the equation you want it to use
  return(temp_k) # tell it what you want it to return
}

celsius_to_kelvin(0) # test it out


# Making plots into functions

myplot <- function(data, x, y){ # name function, give three inputs- data, x, and y
  pal <- pnw_palette("Lake", 3, type = "discrete") # assign color palette
  
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island)) + # put in our plot code; use "curly curlies to assign variables that are column names in data frames
    geom_point() +
    geom_smooth(method = "lm") + # add linear model
    scale_color_manual("Island", values=pal) + # assign colors and change legend title
    theme_bw()
} 

myplot(data = penguins, x = body_mass_g, y = bill_length_mm) # test it out

myplot(data = penguins, x = body_mass_g, y = flipper_length_mm) # test again with different variables


myplot <- function(data = penguins, x, y){ # default to using penguins data
  pal <- pnw_palette("Lake", 3, type = "discrete") 
  
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island)) +
    geom_point() +
    geom_smooth(method = "lm") + 
    scale_color_manual("Island", values=pal) + 
    theme_bw()
} 


myplot(x = body_mass_g, y = flipper_length_mm) # defaulting to penguins data just lets you write x and y in


myplot(x = body_mass_g, y = flipper_length_mm)+
  labs(x = "Body mass (g)",
       y = "Flipper length (mm)") # can layer things on these plots just like using normal ggplot


# If-Else Statements

a <- 4
b <- 5


if (a > b) { # my question; is a > b?
  f <- 20 # if it is true give me answer 1
} else { # else give me this other answer
  f <- 10
}

f # try it out; this isn't true, so it gives me 10


myplot <- function(data = penguins, x, y, lines = TRUE){ # add new argument called lines; make adding lm the default
  pal <- pnw_palette("Lake", 3, type = "discrete") 
  
  if(lines == TRUE) {ggplot(data, aes(x = {{x}}, y = {{y}} , color = island)) + # give it two options; if lines is exactly equal to TRUE, give this first one with lm
    geom_point() +
    geom_smooth(method = "lm") + 
    scale_color_manual("Island", values=pal) + 
    theme_bw()
  } 
  
else{ # otherwise, give this one without lm
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island)) +
    geom_point() +
    scale_color_manual("Island", values=pal) + 
    theme_bw()
}
}

myplot(x = body_mass_g, y = flipper_length_mm) # test it with lines

myplot(x = body_mass_g, y = flipper_length_mm, lines = FALSE) # test it without lines

