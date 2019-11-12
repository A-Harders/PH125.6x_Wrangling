#config
library(rvest)

# first step is to import the webpage
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")

# second step is to extract the relevant selectors
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

# with these we can create a list
guacamole <- list(recipe, prep_time, ingredients)
guacamole

# because the recipes on this site follow the same general layout,
# we can create a function that works for any recipe from this site
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
}

get_recipe("https://www.foodnetwork.com/recipes/tyler-florence/spaghetti-alla-carbonara-recipe-1914140")
