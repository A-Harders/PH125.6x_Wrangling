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

# to illustrate the challenge we create two tables with differing state names
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab2 <- slice(results_us_election_2016, c(1:3,5,7:8)) %>%
        select(state, electoral_votes)

# now we try the different join functions
left_join(tab1, tab2)

# NOTE: all join functions can receive the first argument through a pipe (%>%)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
tab1 %>% inner_join(tab2)
tab1 %>% full_join(tab2)
tab1 %>% semi_join(tab2)
tab1 %>% anti_join(tab2)
