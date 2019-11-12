# config
library(tidyverse)
library(ggrepel)
library(dslabs)

# Assessment q5-7: Lahman dataset
install.packages("Lahman")
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()
Salaries %>% as_tibble()

# Assessment Q5: create a top 10 HR hitter table
top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

top_players <- top_names %>% select(playerID)

# Assessment Q6: create a top 10 salary table for 2016
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(playerID, nameFirst, nameLast, HR, salary)

top_salary

# Assesment Q7: compare AwardPlayers and top_salary
AwardsPlayers %>% filter(yearID == 2016) %>%
  select(playerID) %>%
  intersect(top_players)
