#config
library(dslabs)
library(tidyverse)
data(reported_heights)

# due to several non-numeric entires the heights column is a character
class(reported_heights$height)

# when converting to numeric we get many NAs
x <- as.numeric(reported_heights$height)
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
    filter(is.na(new_height)) %>%
    head(n=10)

# we need to survey the problematic entries
# we write a function to remove the correct entries and we can suppressWarnings() while doing this
not_inches <- function(x, smallest = 50, tallest = 84){
    inches <- suppressWarnings(as.numeric(x))
    ind <- is.na(inches) | inches < smallest | inches > tallest
    ind
}

problems <- reported_heights %>%
  filter(not_inches(height)) %>%
  .$height
length(problems)

# observing the result shows 3 large groups of errors:
print(problems)

  # 1. x'y OR x' y" or x' y\"
  pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
  str_subset(problems, pattern) %>% head(n=10) %>% cat

  # 2. x.y or x,y
  pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
  str_subset(problems, pattern) %>% head(n=10) %>% cat

  # 3. entries reported in cm instead of inches
  ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81))
  ind <- ind[!is.na(ind)]    
  problems[ind] %>% head(n=10) %>% cat  
  
# we can use the str_subset() to search for simple regex arguments
str_subset(reported_heights$height, "cm")

# we can also define the strings that satisfy our pattern and define the ones that dont
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
  # we could call str_detect twice to find which satify our criteria
  str_detect(s, "cm") | str_detect(s, "inches")

# we can replace the different ways the same information is represented with a uniform symbol to increase matching
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot", "'") %>%
  str_replace("inches|in|''|\"", "") %>%
  str_detect(pattern) %>%
  sum

# our second largest problematic group is the x.y x,y and x y form
# we cant just search and replace x,y as that would change 70,5 into 70'5
pattern_without_groups <- "^[4-7]\\s*[,\\.\\s+]\\s*\\d*$"
pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

str_subset(problems, pattern_with_groups) %>%
    str_replace(pattern_with_groups, "\\1'\\2") %>% head

# now we have a powerful string processing technique, we need to improve it
# we need to create a function that shows all of the problem heights
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
      ((inches >= smallest & inches <= tallest) |
          (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

problems <- reported_heights %>%
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

# we can use the pipe to concatenate our replacements and find how many we match
converted <- problems %>%
  str_replace("feet|ft|foot", "'") %>% # convert feet to '
  str_replace("inches|in|''|\"", "") %>% # remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") # change format

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)