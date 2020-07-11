
library(shiny)

# Define server logic
shinyServer(function(input, output) {
    
    im <- eventReactive(input$go,{ #The algorithm doesn't run until the button is clicked.
        
        # Depending on wheter the image will be one of the pre-loaded, loaded from the web or locally
        # it chooses a different input.
        if (input$img_src == "Pre-loaded examples"){
            load.image(paste0("www/",input$examples))
        } else if (input$img_src == "From web"){
            load.image(input$url)
        } else {
            load.image(input$file %>% pull(datapath)) # Given that 'fileInput()' 
            # returns a df, we need to 'pull' only the image's path for
            # the load.image() function to work properly.
            
            
        }
        
    })
    
    bdf <- reactive({
        as.data.frame(im(), wide = "c") # we turn the image into a data.frame
                                        # for the color clustering.
    }) 
    
    kclust <- reactive({
        set.seed(input$seed) # user defined seed
        # the k-means clustering
        kmeans(bdf(), centers = input$clusters, iter.max = 20) %>% 
            tidy() %>% 
            select(-c(x1,x2)) %>% 
            rgb()
    })
    
    # plotting the image, along with the color clusters.
    output$image <- renderPlot({
        tibble(x = c(rep(0,input$clusters)),
               y = seq(from = 1, by = 2, length.out = input$clusters),
               k = factor(1:input$clusters)) %>%
            ggplot(aes(x,y)) +
            geom_point(size = 20, color = kclust())+
            coord_cartesian(xlim = c(-5,.05), ylim = c(-0.3,2   * input$clusters))+
            theme_void()+
            annotation_custom(rasterGrob(im()),xmin = -Inf, xmax = Inf,
                              ymin = -Inf, ymax = Inf)
    })
    


})


