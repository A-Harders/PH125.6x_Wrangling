#config
library(dslabs)
library(tidyverse)
data(reported_heights)

# using the reported heights we have processed from 3-2-1
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

converted <- problems %>%
  str_replace("feet|ft|foot", "'") %>% # convert feet to '
  str_replace("inches|in|''|\"", "") %>% # remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") # change format

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

# case 1 solution -  adding "'0" then the pattern will match
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c (yes, no)
str_replace(s, "^([4-7])$", "\\1'0")

# case 2 & 4 solution - we adapt the method above of adding "'0" to solve cases 2 & 4
# including the '? quantifier means that the replace is looking for either none or one apostrophe
str_replace(s, "^([56])'?$", "\\1'0")

# case 3 solution - we adapt our pattern to include decimals points for the inches
  # NOTE: we have to escape the . in our pattern as it is a regex special character
old_pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
new_pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"
index <- str_detect(converted, new_pattern)
mean(index)

# case 5 solutions - for the metres with commas we use the same process as x.y to x'y, but require a 1 or 2
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

# trimming removes leading and following spaces, the blank spaces can lead to patterns not matching
s <- " Hi "
str_trim(s)

# standardising capitalisation can save on lines of code
s <- c("Five feet eight inches")
str_to_lower(s)

# now we place all of our str_replace() and formatting functions into a function
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

# we write a function that converts words to numbers
words_to_numbers <- function(s){
  str_to_lower(s) %>%
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

# now we can process the problems and see what problems remain
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
mean(index)
