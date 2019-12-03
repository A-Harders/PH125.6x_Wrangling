# config
library(tidyverse)
library(dslabs)
library(pdftools)
options(digits = 3)

# Q1 - open the file and inspect the pdf report (broken in windows)
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))

# Q2 - process the pdf to text and inspect
txt <- pdf_text(fn)
txt

# Q3 - extract page 9 and use the \n to str_split
x <- txt[9] %>%
  str_split("\n")
length(x)
class(x)

# Q4 - find the first entry of the x object
s <- x[[1]]
length(s)
class(s)

# Q5 - str_trim the results of s to tidy the data, what is the last character of the element?
s <- str_trim(s)
s

# Q6 - use str_which to create the header, whats the value of the ehader
header_index <- s %>%
  str_which("^[A-Z]{3}\\s*")

# Q7 - extract the month and header to store as column names
header <- s[header_index] %>%
  str_split("\\s+")

# Q8 - create the tail index using the total row
tail_index <- s %>%
  str_which("^[A-Za-z]{5}")

# Q9 - use str_count to find how many rows have a single number in them
n <- s %>%
  str_count("\\d+")

# Q10 - remove all entries before header_index, after tail index, and all where n = 1
out <- c(1:header_index,which(n<=1),tail_index:length(s))
s <- s[-out]
s

# Q11 - remove all text that isnt a digit or a space
s <- str_remove_all(s, "[^\\d\\s]")
s

# Q12a - convert into a matrix, calculate the mean for 2015
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
colnames(s) <- c("DAY","2015","2016","2017","2018")

class(s) <- "numeric"

mean(s)
