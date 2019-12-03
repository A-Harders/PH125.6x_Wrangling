#config
library(tidyverse)
library(dslabs)
data("gapminder")

# we want to show life expectancy for the carribean
gapminder %>%
  filter(region =="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color= country)) +
  geom_line()

# we notice that lots of space in the plot area is wasted with longer country names
# we use recode() to change column variable names consistently
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country,
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()