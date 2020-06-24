#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    im <- eventReactive(input$go,{
        
        # gsub("\\\\", "/", input$file$datapath)
        
        if (input$img_src == "From web"){
            load.image(input$url)
        } else {
            load.image(input$file %>% pull(datapath))
            # load.image(input$file)
            
        }
        
    })
    
    bdf <- reactive({
        as.data.frame(im(), wide = "c") 
    }) 
    
    kclust <- reactive({
        kmeans(bdf(), centers = input$clusters) %>% 
            tidy() %>% 
            select(-c(x1,x2)) %>% 
            rgb()
    })

    output$image <- renderPlot({
        tibble(x = c(rep(0,input$clusters)),
               y = seq(from = 1, by = 2, length.out = input$clusters),
               k = factor(1:input$clusters)) %>%
            ggplot(aes(x,y)) +
            geom_point(size = 20, color = kclust())+
            coord_cartesian(xlim = c(-5,.05), ylim = c(-0.3,2   * input$clusters))+
            theme_void()+
            theme(
                panel.background = element_rect(fill = "transparent"), # bg of the panel
                plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                panel.grid.major = element_blank(), # get rid of major grid
                panel.grid.minor = element_blank(), # get rid of minor grid
                legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
            ) +
            annotation_custom(rasterGrob(im()),xmin = -Inf, xmax = Inf,
                              ymin = -Inf, ymax = Inf)
    })
    


})


