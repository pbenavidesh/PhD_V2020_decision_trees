
library(shiny)

# Define UI for application
shinyUI(navbarPage(title = "Lecture 5 - Shiny Apps",
                   theme = shinytheme("united"),
                   # First tab - moderndive ####
                   tabPanel("Seattle House prices",
                            sidebarLayout(
                                sidebarPanel(
                                    tabsetPanel(type = "pills",
                                        tabPanel(
                                            h4("Modify the log10 size"),
                                            sliderInput("t1_bins1",
                                                        "Number of bins:",
                                                        min = 1,
                                                        max = 50,
                                                        value = 20), 
                                            colourInput("t1_color1", "Select line colour", 
                                                        value = "orange", showColour = "background"),
                                            colourInput("t1_colorLine1", "Select fill colour", 
                                                        value = "white", showColour = "background"),
                                            selectInput("theme", label = h4("Select theme for plot"), 
                                                        choices = names(themes)),
                                        ),
                                        tabPanel(
                                            h4("Modify the log10 price"),
                                            sliderInput("t1_bins2",
                                                        "Number of bins:",
                                                        min = 1,
                                                        max = 50,
                                                        value = 20), 
                                            colourInput("t1_color2", "Select line colour", 
                                                        value = "white"),
                                            colourInput("t1_colorLine2", "Select fill colour", 
                                                        value = "orange")
                                        )
                                        
                                    )),
                                    
                                
                                # Show a plot of the generated distribution
                                mainPanel(
                                    plotOutput("t1_distPlot")
                                )
                            )
                   ),
                   # Second tab - mpg ####
                   tabPanel("mpg dataset",
                            # fluidRow()
                            sidebarLayout(
                                sidebarPanel(
                                    radioButtons("t2_plot_type",
                                                 label = "Choose the type of plot",
                                                 choices = names(plot_types)),
                                    radioButtons("t2_fuel",
                                                 label = "City or Highway fuel economy?",
                                                 choices = names(fuel))
                                    
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel(
                                    plotlyOutput("t2_mpg_plot")
                                )
                            )
                   ),
                   # Third tab - gapminder ####
                   tabPanel("gapminder",
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput("t3_continents",
                                                   label = "Choose continents to display",
                                                   choices = levels(gapminder$continent),
                                                   selected = levels(gapminder$continent)),
                                selectInput("t3_facets",
                                            label = "Plot layout",
                                            choices = c("One plot","By facets")),
                                actionButton("t3_go",
                                             label = "Render plot!")
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotlyOutput("t3_gap_plot")
                              )
                            )
                            )
                   
        
) #navbarPage
) #shinyUI
    
