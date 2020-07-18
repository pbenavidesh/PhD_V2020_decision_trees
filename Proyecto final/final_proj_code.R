# Classifying asteroids as hazardous or non-hazardous
# Pablo Benavides-Herrera
# 
# 2020-07-17
# Analytics based on classification and regression trees
# ITESO
# 
# Data obtained from 
# https://www.kaggle.com/shrutimehta/nasa-asteroids-classification
# https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009


# Session info ------------------------------------------------------------

# sessionInfo()
# 
# R version 4.0.2 (2020-06-22)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 18362)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=Spanish_Mexico.1252  LC_CTYPE=Spanish_Mexico.1252   
# [3] LC_MONETARY=Spanish_Mexico.1252 LC_NUMERIC=C                   
# [5] LC_TIME=Spanish_Mexico.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] JOUSBoost_2.1.0     randomForest_4.6-14 rpart.plot_3.0.8    rpart_4.1-15       
# [5] yardstick_0.0.6     workflows_0.1.2     tune_0.1.1          rsample_0.0.7      
# [9] recipes_0.1.13      parsnip_0.1.2       infer_0.5.2         dials_0.0.8        
# [13] scales_1.1.1        broom_0.5.6         tidymodels_0.1.0    forcats_0.5.0      
# [17] stringr_1.4.0       dplyr_1.0.0         purrr_0.3.4         readr_1.3.1        
# [21] tidyr_1.1.0         tibble_3.0.2        ggplot2_3.3.2       tidyverse_1.3.0    
# 
# loaded via a namespace (and not attached):
#   [1] readxl_1.3.1         backports_1.1.8      tidytext_0.2.4      
# [4] plyr_1.8.6           igraph_1.2.5         splines_4.0.2       
# [7] crosstalk_1.1.0.1    listenv_0.8.0        SnowballC_0.7.0     
# [10] rstantools_2.1.1     inline_0.3.15        digest_0.6.25       
# [13] foreach_1.5.0        htmltools_0.5.0      rsconnect_0.8.16    
# [16] fansi_0.4.1          magrittr_1.5         globals_0.12.5      
# [19] modelr_0.1.8         gower_0.2.2          RcppParallel_5.0.2  
# [22] matrixStats_0.56.0   xts_0.12-0           prettyunits_1.1.1   
# [25] colorspace_1.4-1     blob_1.2.1           rvest_0.3.5         
# [28] haven_2.3.1          xfun_0.15            callr_3.4.3         
# [31] crayon_1.3.4         jsonlite_1.7.0       lme4_1.1-23         
# [34] iterators_1.0.12     survival_3.1-12      zoo_1.8-8           
# [37] glue_1.4.1           gtable_0.3.0         ipred_0.9-9         
# [40] V8_3.2.0             pkgbuild_1.0.8       rstan_2.21.1        
# [43] DBI_1.1.0            miniUI_0.1.1.1       Rcpp_1.0.5          
# [46] xtable_1.8-4         GPfit_1.0-8          stats4_4.0.2        
# [49] lava_1.6.7           StanHeaders_2.21.0-5 prodlim_2019.11.13  
# [52] DT_0.14              htmlwidgets_1.5.1    httr_1.4.1          
# [55] threejs_0.3.3        ellipsis_0.3.1       pkgconfig_2.0.3     
# [58] loo_2.3.0            nnet_7.3-14          dbplyr_1.4.4        
# [61] utf8_1.1.4           tidyselect_1.1.0     rlang_0.4.6         
# [64] DiceDesign_1.8-1     reshape2_1.4.4       later_1.1.0.1       
# [67] munsell_0.5.0        cellranger_1.1.0     tools_4.0.2         
# [70] cli_2.0.2            generics_0.0.2       ggridges_0.5.2      
# [73] evaluate_0.14        fastmap_1.0.1        yaml_2.2.1          
# [76] processx_3.4.3       knitr_1.29           fs_1.4.2            
# [79] packrat_0.5.0        future_1.17.0        nlme_3.1-148        
# [82] mime_0.9             rstanarm_2.19.3      xml2_1.3.2          
# [85] tokenizers_0.2.1     compiler_4.0.2       bayesplot_1.7.2     
# [88] shinythemes_1.1.2    rstudioapi_0.11      curl_4.3            
# [91] reprex_0.3.0         tidyposterior_0.0.3  lhs_1.0.2           
# [94] statmod_1.4.34       stringi_1.4.6        ps_1.3.3            
# [97] lattice_0.20-41      Matrix_1.2-18        nloptr_1.2.2.2      
# [100] markdown_1.1         shinyjs_1.1          vctrs_0.3.1         
# [103] pillar_1.4.4         lifecycle_0.2.0      furrr_0.1.0         
# [106] httpuv_1.5.4         R6_2.4.1             promises_1.1.1      
# [109] gridExtra_2.3        janeaustenr_0.1.5    codetools_0.2-16    
# [112] boot_1.3-25          colourpicker_1.0     MASS_7.3-51.6       
# [115] gtools_3.8.2         assertthat_0.2.1     withr_2.2.0         
# [118] shinystan_2.5.0      parallel_4.0.2       hms_0.5.3           
# [121] grid_4.0.2           timeDate_3043.102    class_7.3-17        
# [124] minqa_1.2.4          rmarkdown_2.3        pROC_1.16.2         
# [127] tidypredict_0.4.5    shiny_1.5.0          lubridate_1.7.9     
# [130] base64enc_0.1-3      dygraphs_1.1.1.6

# pkgs --------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(rpart)
library(rpart.plot)
library(randomForest)
library(JOUSBoost)

# Data --------------------------------------------------------------------

# nasa <- read_csv("./Proyecto final/nasa.csv")
# nasa <- read_csv("nasa.csv") # to use in shiny app
# nasa
# 
# # wine <- read_csv("./Proyecto final/wine.csv")
# wine <- read_csv("wine.csv") # to use in shiny app
# wine

red <- read_delim("./Proyecto final/winequality-red.csv", delim = ";") %>% 
  mutate(wine = "red")
white <- read_delim("./Proyecto final/winequality-white.csv", delim = ";") %>% 
  mutate(wine = "white")

wine <- bind_rows(red,white) %>% 
  mutate(wine = as_factor(wine))


# Cleaning ----------------------------------------------------------------

    # nasa ####
nasa_tidy <- nasa %>% 
  mutate_if(is.character, as_factor) %>% 
  mutate(Hazardous = as_factor(Hazardous))
nasa_tidy

    # wine ####
wine_tidy <- wine %>% 
  mutate(quality2 = case_when(quality <= 4 ~ "bad",
                             quality <= 6 ~ "regular",
                             TRUE ~ "good") %>% 
           factor(levels = c("bad","regular","good"),
                  ordered = TRUE))



# EDA ---------------------------------------------------------------------

    # nasa ####
nasa_tidy %>% 
  ggplot(aes(x = `Relative Velocity km per sec`, 
             y = , 
             size = `Est Dia in KM(max)`, 
             shape = Hazardous))

nasa_tidy %>% 
  distinct(`Neo Reference ID`) %>% 
  summarise(n = n())

    # wine ####
summary(wine_tidy)

wine_tidy %>% 
  group_by(wine, quality) %>% 
  summarise(n = n(), .groups = "drop")


wine_tidy %>% 
  ggplot(aes(x = `volatile acidity`, y = alcohol, 
             color = quality2)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~wine)

wine_tidy %>% 
  ggplot(aes(x = sulphates, y = alcohol, 
             color = quality2)) +
  geom_point()

wine_tidy %>% 
  ggplot(aes(x = `volatile acidity`, fill = quality2)) +
  geom_histogram() + facet_wrap(~ quality2)

wine_tidy %>% 
  ggplot(aes(x = `volatile acidity`, color = quality2)) +
  geom_freqpoly(size = 1.5) + guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top")

wine_tidy %>% 
  ggplot(aes(x = `volatile acidity`, fill = quality2)) +
  geom_area(stat = "bin") + 
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "top")

wine_tidy %>% 
  ggplot(aes(x = `volatile acidity`, fill = quality2)) +
  geom_density(alpha = 0.5) + 
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "top")

GGally::ggpairs(wine_tidy)

# Decision tree -----------------------------------------------------------

# Tree specification
model_tree <-
  decision_tree(cost_complexity = 0.01) %>%
  set_engine("rpart") %>%
  set_mode("classification") %>% 
  translate()
# model_tree

    # nasa ####
nasa_tree <- model_tree %>% 
  fit(Hazardous ~ ., data = nasa_tidy)
nasa_tree


rpart::plotcp(nasa_tree$fit)

rpart.plot(nasa_tree$fit, type = 4,roundint=FALSE)

    # wine ####

wine_tree <- model_tree %>% 
  fit(quality2 ~ ., data = wine_tidy)
wine_tree


rpart::plotcp(wine_tree$fit)

rpart.plot(wine_tree$fit, type = 2,roundint=FALSE)


# Ensemble methods --------------------------------------------------------


