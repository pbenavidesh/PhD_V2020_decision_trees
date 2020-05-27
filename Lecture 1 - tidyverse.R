library(tidyverse)
library(gapminder)

gapminder

gapminder %>% 
  filter(year == 1957)

gapminder %>% 
  filter(country == "Austria")

gapminder %>% 
  filter(country == "China")

gapminder %>% 
  arrange(desc(pop))

gapminder %>% 
  filter(year == 1957) %>% 
  arrange(desc(pop))

