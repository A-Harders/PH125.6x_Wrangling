#config
library(rvest)
library(stringr)
library(tidyverse)


# scraping the web for the case study
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)

murders_raw <- h %>% html_nodes("table")
murders_raw <- murders_raw[[2]]
murders_raw <- murders_raw %>% html_table
murders_raw <- murders_raw %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", 
                          "total_rate", "murder_rate", "gun_murder_rate"))
head(murders_raw)

# we need to find the columns with commas by using str_detect()
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))

# we then use the str_replace_all() to remove them
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

# we then use the mutate_all() to apply this operation to each column

# alternatively parse_number() included in readr, is used to remove non-numeric characters before parsing
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head
