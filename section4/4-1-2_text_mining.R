#config
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(tidytext)
library(textdata)
set.seed(1)

# we can mine trumps tweets pre-compiled
url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

names(trump_tweets)

# we use extract() to remove "twitter for(.*)" part and filter out retweets
trump_tweets %>%
  extract(source, "source", "Twitter for (.*)") %>%
  count(source)

trump_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

# we then focus on the tweets around his campaign: from his announcement to election day
  # we extract hour, then compute the proportion of tweets tweeted at each hour for each device
campaign_tweets <- trump_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  extract(source, "source", "Twitter for(.*)") %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "houu of day (EST)",
       y = "% of tweets",
       color = "")
campaign_tweets

  # we notice a big peak for android in the early morning that is different to the other patterns
  # we therefore assum that two different entities are using these devices, and will study the tweet difference

# we use the tidytext package to convert free text into tidy tables, the main function being unnest_tokens()
# here is an example using tweet 3008
i <- 3008
trump_tweets$text[i]
trump_tweets[i,] %>%
  unnest_tokens(word, text) %>%
  select(word)

# we have to create a pattern that captures twitter tokens also
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# we can now use the unnest_tokens() function with regex to extract hastags also
trump_tweets[i,] %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

# now we just need to remove links from any tweets
trump_tweets[i,] %>%
  mutate(text = str_replace_all(text, "http://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)
           
# now we can extract the words for all tweets and analyse the data
tweet_words <- trump_tweets %>%
  mutate(text = str_replace_all(text, "http://t.co/[A-za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern)

tweet_words %>%
  count(word) %>%
  arrange(desc(n))

# the results are not surprising, the to a and is of in, are the top words
# tidytext allows you to filter out these stop_words
tweet_words <- trump_tweets %>%
  mutate(text = str_replace_all(text, "http[s]?://t.co/[A-za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word)

tweet_words %>%
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word,n)) %>%
  arrange(desc(n))  

# inspecting the words shows characteristics we dont want, just numbers and the capturing of quote marks
tweet_words <- trump_tweets %>%
  mutate(text = str_replace_all(text, "http[s]?://t.co/[A-za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
          !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

# now we can see what proportion of words are sent from varying devices
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n , fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) /
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone +0.5)))

android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

# given some of these words are low frequency we can filter on frequency also
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

# the tidytext package contains a lexicon of sentiments
sentiments

# we can force the tidytext package to use different lexicons
get_sentiments("bing") %>%
  count(sentiment)

get_sentiments("afinn") %>%
  count(value)

get_sentiments("loughran") %>%
  count(sentiment)

get_sentiments("nrc") %>%
  count(sentiment)

# we will be using nrc for our sentiment analysis
# we combine the words and sentiment using inner_join, which only keeps associated words witha sentiment
nrc <- get_sentiments("nrc")
tweet_words %>% inner_join(nrc, by = "word") %>%
  select(source, word, sentiment) %>% sample_n(10)

# now we will count and compare the frequencies of sentiment for each device
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

# as there are more tweets via Android we need to compare proportions not just frequency
sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android),
iPhone = iPhone / (sum(iPhone) - iPhone),
or = Android/iPhone) %>%
  arrange(desc(or))

# are the sentiments statistically significant?
# we need to compute the odds ratio and confidence interval
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log((Android/(sum(Android)-Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone +1/(sum(iPhone)-iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

# a graphical representation shows sentiments are overrepresented
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  coord_flip()

# we can explore specific words also
android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone >10) %>%
  arrange(desc(or))

# and we can display this graphically also
android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0 )) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) +
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust =1))
