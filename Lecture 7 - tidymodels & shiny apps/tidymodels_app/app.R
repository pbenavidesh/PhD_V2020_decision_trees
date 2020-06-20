# pkgs & scripts ####

library(shiny)
library(shinythemes)
library(tidymodels)

# Data and fixed variables ####
pkgs <- c("rsample", 
          "parsnip", 
          "recipes", 
          "workflows",
          "tune", 
          "yardstick",
          "broom", 
          "dials")

# Define UI for application ####
ui <- fluidPage( theme = shinytheme("cerulean"),
                 title = "tidymodels",
    titlePanel(a(code("tidymodels"), style="color:firebrick", href="https://www.tidymodels.org/")),
    navbarPage(icon("home"),
        # theme = "cerulean",  # <--- To use a theme, uncomment this
        #p("Tidymodels", style="color:firebrick"),
        # T1 - installation ####
        tabPanel("Installation",
                 icon = icon("download"),
                 fluidRow(
                     column(5, img(src = "tidymodels.png", hight=300, width = 300)),
                     column(3,
                            h3("TIDYMODELS"), br(),
                            p("The tidymodels framework is a collection of packages for modeling 
                            and machine learning using", strong("tidyverse"),"principles."), 
                            p("Install tidymodels with:"),
                            br(),br(), code("install.packages(\"tidymodels\")"), br(),br(),
                            p("Run", em("library(tidymodels)"), "to load the core packages and make 
                              them available in your current R session"))
                 )
        ),
        # T2 - pkgs ####
        tabPanel("Packages",
                 icon = icon("box-open"), 
                 h3("CORE TIDYMODELS"),
                 p("The core tidymodels packages work together to enable a wide variety of modeling approaches."),
                 fluidRow(
                     column(3,
                            wellPanel(
                                selectInput("pkg_name", "Choose a tidymodel library:",
                                            list(package = pkgs) # hacer una lista limpia con los paquetes principales
                                ),
                                br(),
                                br(),
                                br(),
                                imageOutput("pkg_logo", width = "90%", height = "90%"),
                                br(),
                                br(),
                                br(),
                                verbatimTextOutput("pkg_descr"),
                                # textOutput("pkg_descr")
                            )
                            ),
                     column(9,
                            htmlOutput("frame")
                            ) 
                 ) # fluidRow
        ), # tabPanel
        # T3 - Learn ####
        tabPanel("Learn",
                 icon = icon("chalkboard-teacher"),
                 tabsetPanel(
                     tabPanel("Perform statistical analysis",
                              tabsetPanel(type = "pills",
                                          tabPanel(
                                              title = "Tidy analysis",
                                              em("Analyze the results of correlation tests
                                                 and simple regression models for many data
                                                 sets at once."),
                                              htmlOutput("tidy_analysis")
                                          ),
                                          tabPanel(
                                              title = "K-means",
                                              em("Summarize clustering characteristics
                                                 and estimate the best number of clusters
                                                 for a data set."),
                                              htmlOutput("k_means")
                                          ),
                                          tabPanel(
                                              title = "Bootstrap resampling",
                                              em("Apply bootstrap resampling to 
                                                 estimate uncertainty in model parameters."),
                                              htmlOutput("bootstrap")
                                          ),
                                          tabPanel(
                                              title = "Hypothesis testing",
                                              em("Perform common hypothesis tests for 
                                                 statistical inference using flexible 
                                                 functions."),
                                              htmlOutput("hypothesis")
                                          ),
                                          tabPanel(
                                              title = "Contingency tables",
                                              em("Use tests of independence and 
                                                 goodness of fit to analyze tables of counts."),
                                              htmlOutput("xtabs")
                                              )
                              
                              
                     ),
                     tabPanel("Tab 2", "This panel is intentionally left blank"),
                     tabPanel("Tab 3", "This panel is intentionally left blank")
                 )
                 
        )
    )
    
)
)

# Define server logic required ####
server <- function(input, output, session) {
    # T2 - pkgs ####
    output$pkg_logo <- renderImage({
        list(src = paste0("./images/",input$pkg_name,".png"),
             width = "60%",
             height = "60%")
    }, deleteFile = FALSE)
    
    output$pkg_descr <- renderPrint({
        cat(packageDescription(input$pkg_name)$Description,sep = "\n")
    }, width = 20)
    
    output$frame <- renderUI({
        my_test <- tags$iframe(src=paste0("https://", input$pkg_name,".tidymodels.org"), height=600, width=1300)
        print(my_test)
    })
    
    # T3 - learn ####
    output$tidy_analysis <- renderUI({
        my_test <- tags$iframe(src="https://www.tidymodels.org/learn/statistics/tidy-analysis/", 
                               height=600, width=1800)
        print(my_test)
    })
    
    output$k_means <- renderUI({
        my_test <- tags$iframe(src="https://www.tidymodels.org/learn/statistics/k-means/", 
                               height=600, width=1800)
        print(my_test)
    })
    
    output$bootstrap <- renderUI({
        my_test <- tags$iframe(src="https://www.tidymodels.org/learn/statistics/bootstrap/", 
                               height=600, width=1800)
        print(my_test)
    })
}

# Run the application 
shinyApp(ui, server)
