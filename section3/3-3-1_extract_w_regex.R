#config
library(tidyverse)

# we construct a simple data-farme for our example
s <- c("5'10", "6'1")
tab <- data.frame(x=s)

# we can use separate() to separate the feet and inches
tab %>% separate(x, c("feet", "inches"), sep = "'")

# we can also use the extract() and regex to extract this data
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# regex is far more potent
s <- c("5'10", "6'1", "5'8inches")
tab <- data.frame(x=s)
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")
