library(shiny)

# Define UI for application
ui <- fluidPage(
    titlePanel("Título de la app con sidebars y texto"),
    sidebarLayout(
        sidebarPanel("sidebar panel"),
        mainPanel("main panel",
                  h1("encabezado nivel 1"),
                  h2("encabezado nivel 2"),
                  h3("encabezado nivel 3"),
                  p("Inicia párrafo con texto normal,", 
                    strong("texto en negritas dentro del párrafo,"),
                    em("también texto en cursiva"), "y",
                    code("texto con formato de código")),
                  p("Un nuevo párrafo."))
    )
)

# Define server logic required
server <- function(input, output) {
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)