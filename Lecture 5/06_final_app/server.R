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
    
    # First tab - moderndive ####
    output$t1_distPlot <- renderPlot({
        p1 <- ggplot(house_prices, aes(x = log10_size)) + 
            geom_histogram(bins = input$t1_bins1, 
                           color = input$t1_color1, 
                           fill = input$t1_colorLine1) +
            labs(x = "log10 living space (square feet)", 
                 title = "House size") +
            themes[[input$theme]]
        
        
        p2 <- ggplot(house_prices, aes(x = log10_price)) + 
            geom_histogram(bins = input$t1_bins2, 
                           color = input$t1_color2, 
                           fill = input$t1_colorLine2) +
            labs(x = "log10 price", title = "House price")
        
        p1 + p2
    })
    
    # Second tab - mpg ####
    
    autos_df <- reactive({
        if (input$t2_fuel == "City"){
            autos %>% 
                mutate(manufacturer = fct_reorder(manufacturer, cty))
        } else {
            autos %>% 
                mutate(manufacturer = fct_reorder(manufacturer, hwy))
        }
        
    })
    
    output$t2_mpg_plot <- renderPlotly({
        autos_df() %>% 
            ggplot(aes_string(x = "manufacturer", y = fuel[[input$t2_fuel]], 
                       fill = "manufacturer")) +
            plot_types[[input$t2_plot_type]] +
            theme(legend.position = "none") + 
            labs(x = "Manufacturer", y = fuel[[input$t2_fuel]]) +
            coord_flip()
        
    })
    
    # Third tab - gapminder ####
    
    gap <- eventReactive(input$t3_go,{
        gapminder %>% 
            filter(continent %in% input$t3_continents)
    })
    
    output$t3_gap_plot <- renderPlotly({
        p <- gap() %>% 
            ggplot(aes(x = gdpPercap, y = lifeExp, color = country,
                       size = pop, label = country)) +
            geom_point(aes(frame = year)) + 
            scale_x_log10() + 
            ylab("Life expectancy (years)") +
            xlab("GDP per capita (USD, inflation-adjusted)")
        
        if (input$t3_facets == "By facets"){
            p + facet_wrap(~ continent) +
                theme(legend.position = "none")
        } else p + labs(color ="", size="")
            
    })

})
