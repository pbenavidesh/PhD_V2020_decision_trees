# Lecture 1 - Intro to tidyverse
# Analytics based on decision trees for
# classification and regression
# Pablo Benavides Herrera
# 2020-05-26

# pkgs ####
library(tidyverse)
library(gapminder)
library(plotly)
library(tidymodels)

# A nice figure ####
seq(-3,3,by=.01) %>%
  
  expand.grid(x=., y=.) %>%
  
  ggplot(aes(x=(1-x-sin(y^2)), y=(1+y-cos(x^2)))) +
  
  geom_point(alpha=.05, shape=20, size=0)+
  
  theme_void()+
  
  coord_polar()

# dplyr + ggplot2 ####
# the gapminder data
gapminder

# filtering by year
gapminder %>% 
  filter(year == 1957)

# filter by country
gapminder %>% 
  filter(country == "Austria")

gapminder %>% 
  filter(country == "China")

# sorting the tibble by population (descending) 
gapminder %>% 
  arrange(desc(pop))

# filter + arrange
gapminder %>% 
  filter(year == 1957) %>% 
  arrange(desc(pop))

# top 10 of countries with max lifeExp (in months)
gapminder %>% 
  mutate(lifeExpMonths = lifeExp * 12) %>% 
  filter(year == 2007) %>% 
  top_n(10, lifeExpMonths)

# ggplot + plotly
plot <- gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  scale_x_log10()

ggplotly(plot)

# animate a plot across years
plot <- gapminder %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(frame = year) +
  scale_x_log10()

ggplotly(plot)


gapminder %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp,
             color = continent, size = pop)) +
  geom_point() +
  facet_wrap(~ year)


gapminder %>% 
  filter(year == 2007) %>% 
  summarise(meanLifeExp = mean(lifeExp),
            totalpop = sum(as.numeric(pop)))

gapminder %>% 
  summarise(medianLifeExp = median(lifeExp),
            maxGdpPerCap = max(gdpPercap))


gapminder %>% 
  group_by(year) %>% 
  summarise(medianLifeExp = median(lifeExp),
            maxGdp = max(gdpPercap))

gapminder %>% 
  filter(year == 1957) %>% 
  group_by(continent) %>% 
  summarise(medianLifeExp = median(lifeExp),
            maxGdp = max(gdpPercap))


  