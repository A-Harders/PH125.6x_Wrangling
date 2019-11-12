#config
library(rvest)

# assessment q1-3: loading in and scraping website for q1-3
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
tab <- html_table(nodes[[8]])

tab

# assessment q1: load the first four nodes and inspect, which are the payroll?
html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])

# assessment q2: for the last 3 components which is true?
length(nodes)
html_table(nodes[[19]])
html_table(nodes[[20]])
html_table(nodes[[21]])

# assessment q3: tables for nodes 10 and 19, select columns (team, payroll, average), full_join(Team), count rows
html_table(nodes[[19]])
tab1 <- html_table(nodes[[10]]) %>% select("X2", "X3", "X4") %>%
        setNames(c("Team", "Payroll", "Average"))
tab1 <- tab1[-1,]  
  
tab2 <- html_table(nodes[[19]]) %>% setNames(c("Team", "Payroll", "Average"))
tab2 <- tab2[-1,]

nrow(full_join(tab1, tab2, by = "Team"))

# assessment q4-5: loading in and scraping website for q4-5
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
nodes <- html_nodes(h, "table")

# assessment q4: how many tables are in this site
length(nodes)

# assessment q5: which table has 9 columns and the first column is named "Date(s) conducted)?
head(html_table(nodes[[5]], fill=TRUE))
