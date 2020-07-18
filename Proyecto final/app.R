# Red and white wine quality classification and regression
# Pablo Benavides-Herrera
# 2020-07-17

# pkgs --------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(plotly)
library(GGally)
library(tidyverse)
library(tidymodels)
library(rpart)
library(rpart.plot)
library(randomForest)
library(JOUSBoost)

# data --------------------------------------------------------------------

red <- read_delim("winequality-red.csv", delim = ";") %>% 
    mutate(wine = "red")
white <- read_delim("winequality-white.csv", delim = ";") %>% 
    mutate(wine = "white")

wine <- bind_rows(red,white) %>% 
    mutate(wine = as_factor(wine),
           quality = factor(quality, levels = 0:10, ordered = TRUE))


# inputs ------------------------------------------------------------------

dist_plot_types <- list("Histogram" = geom_histogram(aes(fill = quality)),
                        "Freq. polygons" = geom_freqpoly(aes(color = quality), 
                                                         size = 1.5),
                        "Density" = geom_density(aes(fill = quality), alpha = 0.45),
                        "Area" = geom_area(aes(fill = quality),
                                           stat = "bin"))

features <- names(wine)[1:11]

pair_plots <- list("red" = ggpairs(wine %>% filter(wine =="red")),
                   "white" = ggpairs(wine %>% filter(wine == "white")),
                   "both" = ggpairs(wine))

# UI ----------------------------------------------------------------------

ui <- fluidPage(
    theme = shinytheme("slate"),
    
    fluidRow(
        column(6,
               titlePanel("Red and white wine quality")
               ),
        column(3,
               selectInput(inputId = "analysis",
                           label = "",
                           choices = c("classification","regression"))
            
        ),
        column(3,
            uiOutput("quality_levels")
        )
    ),
    
    navbarPage(
        "Final project",
        position = "fixed-bottom",
        # Home ####
        tabPanel(
            title = "",
            icon = icon("wine-glass-alt"),
            
            br(),
            img(src = "wine.jpg", height = "70%", width = "70%"),
            br(),
            br(),
            p("This app has the purpose of showing an analytics problem
              using classification or regression with ", strong("decision trees "), 
              "and ", strong("ensemble methods.")
              ),
            br(),
            p("To show this, we use data downloaded from ", 
              a("Kaggle", href = "https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009"),
              "regarding red and white wine and some of their features such as"),
            markdown("* Fixed acidity
                     * volatile acidity
                     * citric acid
                     * residual sugar
                     * chlorides
                     * free sulfur dioxide
                     * total sulfur dioxide
                     * density
                     * pH
                     * sulphates
                     * alcohol"),
            br(),
            p("The variable to predict is the wine quality. 
              We provide two options, depending on whether a 
              'classification' or 'regression' analysis is desired. 
              The wine quality comes as a numeric variable ranging
              from 0 (poorest quality) to 10 (highest quality).
              For a classification problem, you can choose which scores to use
             to consider the wine to be of a bad, regular or good quality.
              For a regression problem, the variable remains unchanged."),
            br(),
            tags$blockquote(p("P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis."),
                            p("Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009."),
                            ),
            code("by @pbenavides"),
            br(),
            br()
            
        ),
        # EDA ####
        tabPanel(
            "EDA",
            icon = icon("wpexplorer"),
            sidebarLayout(
                sidebarPanel(
                    radioButtons(
                        inputId = "wine_type",
                        label = "Type of wine to analyze",
                        choices = c("red","white","both"),
                        inline = TRUE
                    ),
                    conditionalPanel(
                        condition = "input.eda_tabs == 'Plots'",
                        selectInput(
                            inputId = "x_var",
                            label = "Choose the variable to plot on the x axis",
                            choices = features
                        ),
                        conditionalPanel(
                            condition = "input.eda_plots == 'Scatter plot'",
                            uiOutput("sel_y_var")
                        )
                    ),
                    conditionalPanel(
                        condition = "input.eda_tabs == 'Plots' & input.eda_plots == 'Distribution' ",
                        selectInput(
                            inputId = "dist_type",
                            label = "Choose the plot type",
                            choices = names(dist_plot_types)
                        )
                        
                    )
                ),
                mainPanel(
                    tabsetPanel( id = "eda_tabs",
                        tabPanel(
                            "Summary",
                            icon = icon("charging-station"),
                            verbatimTextOutput("summary"),
                            verbatimTextOutput("summary2")
                        ),
                        tabPanel(
                            "Plots",
                            icon = icon("chart-bar"),
                            tabsetPanel(id = "eda_plots",
                                type = "pills",
                                tabPanel(
                                    "Distribution",
                                    plotlyOutput("dist_plot")
                                ),
                                tabPanel(
                                    "Scatter plot",
                                    plotlyOutput("scatter_plot")
                                ),
                                tabPanel(
                                    "Pairs",
                                    plotOutput("pairs_plot")
                                )
                            )
                            
                        )
                    )
                )
            )
            
        ),
        
        # Decision trees ####
        tabPanel(
            "Decision trees",
            icon = icon("tree"),
            sidebarLayout(
                sidebarPanel(
                    h3("Decision tree specification"),
                    sliderInput("cp",
                                "Cost complexity",
                                min = 0.001,
                                max = .1,
                                value = 0.01,
                                step = 0.001),
                    br(),
                    h3("Tree plot configuration"),
                    selectInput("tree_type",
                                "Type of tree plot",
                                choices = 0:5),
                    checkboxInput("clip_labs",
                                  "Clip right labels?",
                                  value = TRUE),
                    checkboxInput("under",
                                  "Under?",
                                  value = TRUE),
                    sliderInput("branch",
                                "Branch",
                                min = 0.1, max = 1,
                                step = 0.05, value = 0.3
                    )
                        
                    ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Tree specification",
                            h2("Model specification"),
                            br(),
                            verbatimTextOutput("model"),
                            br(),
                            h2("Results"),
                            verbatimTextOutput("results")
                        ),
                        tabPanel(
                            "Decision tree",
                            plotOutput("tree"),
                            br(),
                            br()
                        ),
                        tabPanel(
                            "Cost complexity plot",
                            plotOutput("cp_plot")
                        )
                    )
                )
                )
        ),
        
        # Ensemble ####
        tabPanel(
            "Ensemble methods",
            icon = icon("pastafarianism"),
            sidebarLayout(
                sidebarPanel(
                    sliderInput("ntree",
                                label = "Number of trees to grow",
                                min = 25,
                                max = 3000,
                                step = 5,
                                value = 500),
                    numericInput("predictions",
                                 label = "Number of predictions to make",
                                 min = 1, step = 1, value = 5)
                ),
                mainPanel(
                    tabsetPanel(
                        # Bagging ####
                        tabPanel(
                            "Bagging", 
                            icon = icon("shopping-bag"),
                            
                        ),
                        # Random forest ####
                        tabPanel(
                            "Random forest",
                            icon = icon("random"),
                            verbatimTextOutput("rf_model"),
                            verbatimTextOutput("rf_predict"),
                            plotOutput("rf_varimp")
                            
                        ),
                        # Boosting ####
                        tabPanel(
                            "Boosting", 
                            icon = icon("fighter-jet")
                        )
                        
                    )
                )

            )
            
            
        )
    ), # navbarPage
    
    br(),
    
    code("by @pbenavides"),
    
    br(),
    br()
  
) # fluidPage


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
    # reactive data ####
    
    output$quality_levels <- renderUI({
        conditionalPanel(
            condition = "input.analysis == 'classification'",
            sliderInput(
                inputId = "quality_lvl",
                label = "Choose the range for the 'regular' quality of the wine",
                min = 0, max = 10, step = 1,
                value = c(4,6)
            )
        )
        
    })
    
    df0 <- reactive({
        if (input$analysis == "classification"){
            wine %>%
                mutate(quality = case_when(quality < input$quality_lvl[1] ~ "bad",
                                           quality <= input$quality_lvl[2] ~ "regular",
                                           TRUE ~ "good") %>% 
                                            factor(levels = c("bad","regular","good"),
                                                   ordered = TRUE)
                       )
        } else {
            wine %>% 
                mutate(quality = as.integer(quality))
                           
        }
        
    })
    
    df <- reactive({
        if (input$wine_type == "both") {
            df0() 
                
        } else {
            
            df0() %>% 
                filter(wine == input$wine_type)
            }
        
    })
    
    # EDA - Summary ####
    output$summary <- renderPrint({
        summary(df())
    })
    
    output$summary2 <- renderPrint({
        df() %>% 
            group_by(wine, quality) %>% 
            summarise(n = n(), .groups = "drop")
    })
    
    # EDA - Plots ####
    
    output$dist_plot <- renderPlotly({
        df() %>% 
            ggplot(aes(x = !!sym(input$x_var))) +
            dist_plot_types[[input$dist_type]] +
            facet_wrap(~ wine)
    })
    
    output$sel_y_var <- renderUI({
        selectInput(
            inputId = "y_var",
            label = "Choose the variable to plot on the y axis",
            choices = as_tibble(features) %>% filter(value != input$x_var) %>% pull()
        )
    })
    
    output$scatter_plot <- renderPlotly({
        df() %>% 
            ggplot(aes(x = !!sym(input$x_var), y = !!sym(input$y_var),
                       color = quality)) +
            geom_point() +
            facet_wrap(~ wine)
    })
    
    output$pairs_plot <- renderPlot({
        pair_plots[[input$wine_type]]
    }, height = 800, width = 1000)
  
    # Decision trees ####
    model_tree <- reactive({
        decision_tree(cost_complexity = input$cp) %>% 
            set_engine("rpart") %>% 
            set_mode(input$analysis) %>% 
            translate()
    })
    
    output$model <- renderPrint({
        model_tree()
    }) 
    
    fit <- reactive({
        
        model_tree() %>%
                parsnip::fit(quality ~ ., data = df())
    })
    
    output$results <- renderPrint({
        fit()
    })
    
    output$tree <- renderPlot({
        rpart.plot(fit()$fit, type = as.integer(input$tree_type),
                   clip.right.labs = input$clip_labs,
                   branch = input$branch,
                   under = input$under)
    }, height = 750, width = 900)
    
    output$cp_plot <- renderPlot({
        plotcp(fit()$fit)
    })
    # Ensemble - Random forest ####
    rf <- reactive({
        randomForest(quality ~ ., data = df(),
                     ntree = input$ntree, importance = TRUE)
        
    })
    
    output$rf_model <- renderPrint({
        rf()
    })
    
    output$rf_predict <- renderPrint({
        predict(rf(), head(df(), input$predictions))
    })
    
    output$rf_varimp <- renderPlot({
        varImpPlot(rf())
    })
}

shinyApp(ui, server)