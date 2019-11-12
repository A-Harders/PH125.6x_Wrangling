#config
library(rvest)

# first step of web scraping is importing the webpage
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)

# second step is to extract all nodes
tab <- h %>% html_nodes("table")
tab <- tab[[2]]
class(tab)

# third step is converting HTML table to a data frame
tab <- tab %>% html_table
class(tab)

# fourth step is general formatting of column names
tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", 
                          "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)
