#config
library(tidyverse)

# create a random sample of dates
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# we can use the libudate package functions to process dates
data.frame(date = days(dates),
           month = month(dates),
           day = day(dates),
           year = year(dates))

# we can also use the parsers to convert strings to dates
# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# the powerful ways to process time
now("utc")
OlsonNames()
Sys.time()
