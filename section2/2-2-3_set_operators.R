# config
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)

data(polls_us_election_2016)

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# with dplyr loaded set operators work on data frames
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]

intersect(tab1, tab2)
union(tab1, tab2)
setdiff(tab1, tab2)
setequal(tab1, tab2)