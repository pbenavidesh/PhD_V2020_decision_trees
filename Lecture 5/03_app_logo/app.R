#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("App agregando una imagen o logotipo"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            "Sidebar"
        ),

        # Show a plot of the generated distribution
        mainPanel(
           img(src = "tidyverse1.png", height = "30%", width = "30%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
