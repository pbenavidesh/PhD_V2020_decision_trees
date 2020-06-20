library(shiny)

# Define UI for application
shinyUI(navbarPage(title = "Data analysis app",
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
                            ),
                   # Fourth tab - transformations ####
                   tabPanel("transformations",
                            navlistPanel(
                              tabPanel("Mathematical transformations",
                                       fluidRow(
                                         column(12,
                                                h1("Box-Cox transformations"))
                                       ),
                                       fluidRow(
                                         column(4,
                                                wellPanel(sliderInput("lambda",
                                                                      "Value of lambda:",
                                                                      min = -1,
                                                                      max = 2,
                                                                      value = 1, step = 0.01))),
                                         column(8,
                                                plotlyOutput("boxcox"))
                                       ),
                                       hr()),
                              tabPanel("Scaling axes",
                                       fluidRow(
                                         column(12,
                                                h1("Log scales"))  
                                       ),
                                       fluidRow(
                                         column(4,
                                                wellPanel(
                                                  selectInput("gdp_var",
                                                              label = "Variable to plot",
                                                              choices = c("GDP", "GDP per capita")),
                                                  checkboxInput("logscale",
                                                                label = "Use log scale",
                                                                value = FALSE),
                                                  checkboxGroupInput("countries",
                                                                     label = "Choose the countries to plot",
                                                                     choices = c("Mexico", "Iceland","Australia",
                                                                                 "Brazil", "Canada", "China",
                                                                                 "Germany", "United States"),
                                                                     selected = c("Mexico", "Iceland",
                                                                                  "Australia"))
                                                )
                                         ),
                                         column(8,
                                                plotlyOutput("gdp"))
                                       ),
                                       hr()
                                       ),
                              tabPanel("Moving averages",
                                       fluidRow(
                                         column(12,
                                                h1("Moving averages"))
                                       ),
                                       fluidRow(
                                         column(4,
                                                wellPanel(
                                                  sliderInput("ma",
                                                              label = "Choose the order for the MA",
                                                              min = 1, max = 50,
                                                              value = 7, step = 1),
                                                  checkboxInput("ma2",
                                                                label = "Apply a MA to the MA",
                                                                value = FALSE),
                                                  conditionalPanel("input.ma2 == 1",
                                                                   sliderInput("ma3",
                                                                               label = "Order of the MA of the MA",
                                                                               min = 1, max = 10,
                                                                               value = 2, step = 1)))),
                                         column(8,
                                                plotlyOutput("ma_plot"))
                                         
                                       ) # fluidRow
                                       
                                       
                                       
                              ) # tabPanel
                            )
                            ),
                   # Fifth tab - TS decomposition ####
                   tabPanel("TS Decomposition",
                            fluidRow(
                              column(12,
                                     h1("Time series decomposition methods - US Employment")
                              )
                            ),
                            fluidRow(#style='height:50vh',
                              column(4,
                                     wellPanel(
                                       selectInput("t5_ts",
                                                   label = "Choose the time series",
                                                   choices = series,
                                                   selected = "Retail Trade"),
                                       radioButtons("t5_dcmp",
                                                    label = "Decomposition method",
                                                    choices = names(dcmp)),
                                       conditionalPanel("input.t5_dcmp=='Classic'",
                                                        radioButtons("t5_type",
                                                                     label = "Type",
                                                                     choices = c("additive",
                                                                                 "multiplicative"),
                                                                     inline = TRUE)
                                                        ),
                                       conditionalPanel("input.t5_dcmp=='STL'",
                                                        flowLayout(
                                                          verticalLayout(
                                                            h5(strong("Trend component")),
                                                            checkboxInput("t5_stl_man_trend",
                                                                               label = "Choose component automatically",
                                                                               value = TRUE),
                                                            conditionalPanel("input.t5_stl_man_trend==0",
                                                                             numericInput("t5_stl_trend",
                                                                                          label = "Trend window",
                                                                                          min = 1,
                                                                                          value = 7,
                                                                                          step = 2)
                                                                             )
                                                          ),
                                                          verticalLayout(
                                                            h5(strong("Seasonal component")),
                                                            checkboxInput("t5_stl_man_seas",
                                                                               label = "Keep a fixed seasonality (periodic window)",
                                                                               value = TRUE),
                                                            conditionalPanel("input.t5_stl_man_seas==0",
                                                                             numericInput("t5_stl_season",
                                                                                          label = "Seasonal window",
                                                                                          min = 7,
                                                                                          value = 15,
                                                                                          step = 2)
                                                            )
                                                          )
                                                          
                                                          
                                                        ),
                                                        checkboxInput("t5_stl_robust",
                                                                      label = "Make it robust to outliers")
                                                        ),
                                       flowLayout(
                                         selectInput("t5_theme", label = "Select theme for plot", 
                                                     choices = names(themes)),
                                         colourInput("t5_color",
                                                     label = "Choose the color",
                                                     value = "blue",
                                                     showColour = "background")
                                       )
                                       
                                     )
                              ),
                              column(8,
                                     tabsetPanel(
                                       tabPanel("Components Plot",
                                                plotOutput("t5_dcmp") #height = 700
                                                ),
                                       tabPanel("Season-adjusted plot",
                                                plotOutput("t5_dcmp_sadj")
                                                ),
                                       tabPanel("Data",
                                                sliderInput("t5_dt_rows",
                                                            label = "Rows to show",
                                                            min = 1,
                                                            max = 100,
                                                            value = c(1,20)),
                                                tableOutput("t5_dcmp_dt"))
                                     )
                                     
                              )
                            )
                            
                   ) # tabPanel
                   
        
) #navbarPage
) #shinyUI
    
