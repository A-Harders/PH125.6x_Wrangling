#config
library(dslabs)
library(lubridate)
options(digits = 3)

# Q3a - how many brexit polls has startdate in April (4)
data(brexit_polls)

brexit_polls %>%
  mutate(months = month(startdate)) %>%
  count(months)

# Q3b - how many brexit polls ended in week 2016-06-12
brexit_polls %>%
  mutate(week = round_date(enddate, unit="week", week_start = "2016-06-12")) %>%
  count(week) %>%
  filter(week == "2016-06-12")

# Q4 - largest proportion of weekday polls
brexit_polls %>%
  mutate(day = weekdays(enddate)) %>%
  count(day) %>%
  arrange(desc(n))

# Q5 - which year has the most movie reviews
data(movielens)

movielens %>%
  mutate(date = as_datetime(timestamp),
         year = year(date)) %>%
  count(year) %>%
  arrange(desc(n))
  
movielens %>%
  mutate(date = as_datetime(timestamp),
         hour = hour(date)) %>%
  count(hour) %>%
  arrange(desc(n))

# part 2 config
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata

# Q6 - str_detect to find unique ID numbers
gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice") == TRUE) %>%
  select(title)

# Q7 - what is the correct ID number for Pride and Prejudice
gutenberg_works() %>%
  filter(title == "Pride and Prejudice")

# Q8 - download Pride and Prejudice and count the words
gutenberg_download(1342) %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  mutate(total = sum(n))

# Q9 - remove stop_words and count the words
gutenberg_download(1342) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  count(word) %>%
  mutate(total = sum(n))

# Q10 - remove tokens that contain digits and count the words
filtered_pride <- gutenberg_download(1342) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word & 
           !str_detect(word, "\\d")) %>%
  count(word) %>%
  mutate(total = sum(n))

# Q11a - how many words appear more than 100 times
filtered_pride %>%
  filter(n >100)

# Q11b - what is the most common word
filtered_pride %>%
  arrange(desc(n))

# Q12a - how many words appear in the afinn lexicon
afinn <- get_sentiments("afinn")

filtered_pride %>%
  inner_join(afinn) %>%
  mutate(total = sum(n))

# Q12b - what proportion of words have a positive value?
filtered_pride %>%
  inner_join(afinn) %>%
  mutate(total = sum(n),
         positive = (value > 0)*n,
         proportion = sum(positive)/total)

# Q12c - how many elements have a value of 4
filtered_pride %>%
  inner_join(afinn) %>%
  filter(value == 4) %>%
  mutate(total = sum(n))
