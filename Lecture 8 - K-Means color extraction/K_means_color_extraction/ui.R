

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),

    # Application title
    titlePanel("Color extraction using K-Means"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("seed",
                         label = "Set seed",
                         value = 68,
                         min = 1,
                         max = 9999),
            sliderInput("clusters",
                        "Number of clusters (colors)",
                        min = 1,
                        max = 10,
                        value = 5),
            radioButtons("img_src",
                         label = "Choose image source",
                         choices = c("Pre-loaded examples","From pc",
                                     "From web"),
                         inline = TRUE
                         ),
            conditionalPanel("input.img_src=='Pre-loaded examples'",
                             selectInput("examples",
                                       label = "Choose the image",
                                       choices = list.files(path = "./www"),
                                       selected = "lotr.png")
            ),
            conditionalPanel("input.img_src=='From web'",
                             textInput("url",
                                       label = "Specify the image's URL")
                             ),
            conditionalPanel("input.img_src=='From pc'",
                             fileInput("file",
                                       label = "Choose an image",
                                       accept = c("image/jpeg",
                                                  "image/png",
                                                  "image/jpg"),
                                       # multiple = TRUE
                                       )
                             ),
            actionButton("go",
                         label = "Extract colors!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("image")
            # plotOutput("image")
        )
    )
))
