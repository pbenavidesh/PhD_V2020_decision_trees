# Decision trees examples app

# pkgs --------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(tidyverse)
library(tidymodels)
library(rpart)
library(rpart.plot)
library(ISLR)

# data --------------------------------------------------------------------
data("ptitanic")
data("iris")
data("Carseats")

df <- list(Titanic = as_tibble(ptitanic),
           Iris = as_tibble(iris),
           Carseats = as_tibble(Carseats))

tree_types <- list("Classification" = "classification",
                "Regression" = "regression")


# UI ----------------------------------------------------------------------

ui <- fluidPage( theme = shinytheme("united"),

    # Application title
    titlePanel("CART (Classification and regression trees)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            wellPanel(
                h3("Decision tree specification"),
                selectInput("data",
                            "Select the dataset",
                            choices = names(df)),
                radioButtons("type",
                             "Select the type of tree",
                             choices = names(tree_types),
                             inline = TRUE),
                sliderInput("cp",
                            "Cost complexity",
                            min = 0.001,
                            max = .1,
                            value = 0.01,
                            step = 0.001)
            ),
            wellPanel(
                h3("Tree plot configuration"),
                selectInput("tree_type",
                            "Type of tree plot",
                            choices = 1:5),
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
                
            )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Data",
                    tableOutput("table")
                ),
                tabPanel("Decision Tree",
                    h2("Model specification"),
                    br(),
                    verbatimTextOutput("model"),
                    br(),
                    h2("Results"),
                    verbatimTextOutput("results")
                ),
                tabPanel("Plots",
                    h2("Decision tree"),
                    plotOutput("tree"),
                    h2("Cost complexity plot"),
                    plotOutput("cp_plot")
                )
            )
           
        )
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {

    output$table <- renderTable({
        df[[input$data]]
    })
    
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
        
        if (input$data == "Titanic"){
            model_tree() %>%
                parsnip::fit(survived ~ ., data = ptitanic)
        } else if (input$data == "Iris"){
            model_tree() %>%
                parsnip::fit(Species ~ ., data = iris)
        } else {
            model_tree() %>%
                parsnip::fit(Sales ~ ., data = Carseats)
        }
            
    })
    
    output$results <- renderPrint({
        fit()
    })
    
    output$tree <- renderPlot({
        rpart.plot(fit()$fit, type = as.integer(input$tree_type),
                   clip.right.labs = input$clip_labs,
                   branch = input$branch,
                   under = input$under)
    })
    
    output$cp_plot <- renderPlot({
        plotcp(fit()$fit)
    })
}


# Run the app -------------------------------------------------------------

shinyApp(ui = ui, server = server)
