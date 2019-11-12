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

# bind_cols() can be used to quickly make dataframes
bind_cols(a = 1:3, b= 4:6)

# bind_cols() can be used to join data frames
tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1,tab2,tab3)
head(new_tab)

# bind_rows() is similar, but binds rows instead of cols
tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1 ,tab2)