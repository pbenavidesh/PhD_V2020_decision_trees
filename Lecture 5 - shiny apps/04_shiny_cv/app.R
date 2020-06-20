#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),

    # Application title
    titlePanel(""),

    sidebarLayout(
        sidebarPanel(
            h3(strong("Pablo Benavides-Herrera")),
            p("Todo mi código se puede consultar en ", 
              tags$a(href="https://github.com/pbenavidesh/", "Github")),
              # code("https://github.com/pbenavidesh/")),
            br(),
            br(),
            img(src = "mr_poopybutthole.jpg", 
                height = "70%", width = "70%", align = "center"),
            h5(strong("pbenavides@iteso.mx")),
            h5("33 1605 2603")
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Empleo actual",
                         h3("Profesor de asignatura,", em("ITESO")),
                         p(paste("Profesor de la materia Series de tiempo", 
                                 "en la licenciatura de Ingeniería Financiera.",
                                 sep = " ")
                         ), 
                         br(),
                         h3("Profesor PAP,", em("ITESO")),
                         p("Profesor en el PAP 4J09, donde se trabaja con",
                           "el", strong("CENACE"), "principalmente buscando producir",
                           "pronósticos de la demanda y generación de",
                           "energía eléctrica.")),
                tabPanel("Intereses",
                         h2(strong("Intereses")),
                         fluidRow(
                             column(4,
                                    verticalLayout(h3("Gaming"),
                                                   img(src = "hollow_knight.png",
                                                       height = "100%",width = "100%"))),
                             column(4,
                                    verticalLayout(h3("Golf"),
                                                   img(src = "golf-cart.jpg",
                                                       height = "100%",width = "100%"))),
                             column(4,
                                    verticalLayout(h3("Snowboarding"),
                                                   img(src = "snowboard.jpg",
                                                       height = "100%",width = "100%")))
                         )
                         
                ),
                tabPanel("Hijos",
                         img(src = "pups.gif"))
            )
           
        
    )
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {

   
}

# Run the application 
shinyApp(ui = ui, server = server)
