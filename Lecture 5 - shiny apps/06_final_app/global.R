# Global configuration

# pkgs --------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(tidyverse)
library(moderndive)
library(nycflights13)
library(colourpicker)
library(patchwork)
library(plotly)
library(gapminder)
library(tsibble)
library(feasts)
library(fable)
library(tsibbledata)
library(fpp3)
library(DT)

# First tab - moderndive --------------------------------------------------

house_prices <- house_prices %>% mutate(
  log10_price = log10(price), log10_size = log10(sqft_living) )

themes <- list("grey" = theme_grey(), "bw" = theme_bw(), "Light" = theme_light(),
               "Minimal" = theme_minimal(),
               "dark" = theme_dark())


# Second tab - mpg --------------------------------------------------------

autos <- mpg %>% 
  mutate_if(is.character, as_factor)

fuel <- list("City" = "cty",
             "Highway" = "hwy")

plot_types <- list("Boxplot" = geom_boxplot(),
                   "Violin" = geom_violin())


# Third tab - gapminder ---------------------------------------------------




# Fourth tab - transformations --------------------------------------------

glob_econ <- global_economy %>%
  mutate(`GDP per capita` = GDP / Population)


us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)


# Fifth tab - TS decomposition --------------------------------------------

series <- us_employment %>% 
  distinct(Title) %>% filter(!str_detect(Title,":")) %>% pull(Title)

empleo <- us_employment %>% 
  filter(year(Month) >= 1990,
         Title %in% series) %>% 
  mutate(Title = factor(Title))

dcmp <- list("Classic" = classical_decomposition(Employed),
             # "X11"= feasts:::X11(Employed),
             # "SEATS" = feasts:::SEATS(Employed),
             "STL" = STL(Employed))


