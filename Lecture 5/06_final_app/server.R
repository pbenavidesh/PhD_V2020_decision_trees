library(shiny)

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
    
    # Fourth tab - transformations ####
    output$boxcox <- renderPlotly({
        aus_production %>%
            autoplot(box_cox(Gas,lambda = input$lambda)) + 
            labs(title =  paste("Australia's gas production, lambda =",
                                input$lambda, sep = " "),
                 x = "Year", y = "(petajoules)")
    })
    gdp_vars <- reactive({
        glob_econ %>% 
            select(Country, Year, gdp = input$gdp_var)
    })
    
    us_ret_ma <- reactive({
        if (input$ma2 == TRUE){
            us_retail_employment %>% 
                mutate(ma1 = slide_dbl(Employed, mean, 
                                       .size = input$ma, 
                                       .align = "cr"),
                       ma_ma =slide_dbl(ma1, mean, 
                                        .size = input$ma3, 
                                        .align = "cl")
                )
        } else {
            us_retail_employment %>% 
                mutate(ma = slide_dbl(Employed, mean,
                                      .size = input$ma,
                                      .align = "cr"))
        }
        
    })
    
    output$gdp <- renderPlotly({
        ge <- gdp_vars() %>% 
            filter(Country %in% input$countries) 
        gg <- ggplot(ge) + 
            aes(x = Year, y = gdp, color = Country) +
            geom_line() + ylab("$USD") + xlab("Year") +
            guides(color = FALSE) +
            theme(legend.position = "top") +
            ggtitle(paste("Evolution of", input$gdp_var,
                          "across countries", sep = " "))
        if (input$logscale == TRUE) {
            gg + scale_y_log10()
            
        } else {
            gg
        }
    })
    
    output$ma_plot <- renderPlotly({
        if (input$ma2 == TRUE){
            us_ret_ma() %>%
                autoplot(Employed, color='gray', size = 0.9) +
                geom_line(aes(y = ma_ma), color = 'red', size = 1) +
                xlab("Year") + ylab("Persons (thousands)") +
                ggtitle(paste0("Total employment in US retail, ",
                               input$ma3,"x",input$ma,"-MA"))
        } else{
            us_ret_ma() %>%
                autoplot(Employed, color='gray', size = 0.9) +
                geom_line(aes(y = ma), color='red', size = 1) +
                xlab("Year") + ylab("Persons (thousands)") +
                ggtitle(paste0("Total employment in US retail, ",
                               input$ma,"-MA"))
        }
        
    })

})
