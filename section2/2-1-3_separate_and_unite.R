#config
library(tidyverse)

# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# first attempt at separate()
dat %>% separate(key, c("year", "variable_name"), "_")

# because the separator is an underscore we can simplify the code
# we also add merge to tell the code what to do with the _expectancy
dat %>% separate(key, c("year", "variable_name"), extra = "merge")

# we can now spread the resulting partially tidied table so that we have a column for each variable
dat %>% separate(key, c("year", "variable_name"), extra = "merge") %>%
    spread(variable_name, value)

data(co2)
co2

# assessment Q10 - tidy the wide data
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_wide

co2_tidy<- gather(co2_wide, month, co2, -year)
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

# assessment Q12 -  tidying admissions dataset
library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
head(dat)

dat_tidy <- spread(dat, gender, admitted)

# assessment Q13 - combining columns
tmp <- gather(admissions, key, value, admitted:applicants)

tmp2 <- unite(tmp, column_name, c(key, gender))
spread(tmp2, column_name, value)
