#config
library(tidyverse)
library(dslabs)

# the table we used in the past for research rates was extracted from a PDF
data("research_funding_rates")
research_funding_rates

# downloading the PDF
install.packages("pdftools")
library(pdftools)
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

# we save the raw table data as a variable
raw_data_research_funding <- txt[2]

# inspecting the raw data table
raw_data_research_funding %>% head

# each new line is indicated by \n, so we use str_split to process this
tab <- str_split(raw_data_research_funding, "\n")
tab <- tab[[1]]
tab %>% head

# by inspecting the data we see the information for the column names by entry number
the_names_1 <- tab[3]
the_names_2 <- tab[4]

# we need to extract the names from the string so we use the regex we learned
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.","") %>% # removes space following the comma
  str_split("\\s{2,}", simplify = TRUE) # splits the entries, whe there are 2 or more spaces (to avoid splitting 'success rate')
the_names_1

the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

# now we can join the names so we have a single name per column
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

# with the column names sorted we can apply the data to our data.frame
view(tab[6:14])
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()
