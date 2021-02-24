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


### Start wrangling! ################################################################################################

now() # time now
now(tzone = "EST") # what time is it on the east coast
now(tzone = "GMT") # what time in GMT
today() # only the date
today(tzone = "GMT") # the date in a certain time zone
am(now()) # is it morning?
pm(now()) # is it night?
leap_year(now()) # is it a leap year?


# different lubridate day/month/time formats- all give same result
ymd("2021-02-24")
mdy("02/24/2021")
mdy("February 24 2021")
dmy("24/02/2021")


# different date and time options for lubridate
ymd_hms("2021-02-24 10:22:20 PM")
mdy_hms("02/24/2021 22:22:20")
mdy_hm("February 24 2021 10:22 PM")


datetimes <- c("02/24/2021 22:22:20", # making a character string of dates and times
             "02/25/2021 11:21:10",
             "02/26/2021 8:01:52")

datetimes <- mdy_hms(datetimes) # convert to date/time in a certain format
month(datetimes) # extract months from the character string
month(datetimes, label = TRUE) # label the months (not just numbers)
month(datetimes, label = TRUE, abbr = FALSE) # ditto, but don't abbreviate
day(datetimes) # extract day 
wday(datetimes, label = TRUE) # extract day of week 
hour(datetimes) # ditto with hours
minute(datetimes) # and minutes
second(datetimes) # and also seconds 

datetimes + hours(4) # adding four hours (like to adjust for different time zones if you've messed up)
# *hour()* extracts the hour component from a time and *hours()* is used to add hours to a datetime
datetimes + days(2) # can also add days (and prob anything else you'd need to add, honestly)

round_date(datetimes, "minute") # round to the nearest minute
round_date(datetimes, "5 mins") # and to the nearest 5 minute

cond_date <- cond %>% 
  mutate(date_time = ymd_hms(date)) # make a new column with date/time
 












