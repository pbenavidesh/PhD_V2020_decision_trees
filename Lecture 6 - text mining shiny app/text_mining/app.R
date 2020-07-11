# Text mining app
# Jane Austen's books


# pkgs --------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(janeaustenr)
library(tidytext)
library(shinythemes)

# data --------------------------------------------------------------------

book_words <- austen_books() %>%
    unnest_tokens(word, text) %>%
    count(book, word, sort = TRUE)

total_words <- book_words %>% 
    group_by(book) %>% 
    summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

freq_by_rank <- book_words %>% 
    group_by(book) %>% 
    mutate(rank = row_number(), 
           `term frequency` = n/total)

book_words <- book_words %>%
  bind_tf_idf(word, book, n)

# ui ----------------------------------------------------------------------

ui <- fluidPage( #themeSelector(),
    theme = "superhero",
    titlePanel("Analyzing word and document frequency"),
    
    sidebarLayout(
        sidebarPanel(
            p("A central question in text mining and natural 
            anguage processing is how to quantify what a 
            document is about. Can we do this by looking 
            at the words that make up the document?"),
            p("Here, as an example, we will be considering "),
            h3("Jane Austen's novels."),
            img(src = "jane.jpg", height = "90%", width = "90%"),
            br(),
            br(),
            code("by @pbenavidesh")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Goal",
                        br(),
                        br(),
                        h4("Some options are:"),
                        p(strong("tf (term frequency) :"),
                          "how frequently a word occurs in 
                          a document."
                          ),
                        p(strong("idf (inverse document frequency) :"),
                          "which decreases the weight for commonly 
                          used words and increases the weight for 
                          words that are not used very much in a 
                          collection of documents."
                        ),
                        p("The statistic", em("tf-idf"), " is 
                          intended to measure how important a 
                          word is to a document in a collection 
                          of documents."
                        ),
                        withMathJax("$$ idf(\\operatorname{term})=\\ln 
                                    \\left(\\frac{n_{\\text {documents }}}{n_{\\text 
                                    {documents containing term }}}\\right) $$")
                ),
                tabPanel("Data",
                         h4("Summary"),
                         verbatimTextOutput("summary"),
                         fluidRow(
                             column(6,
                                    h4("Table"),
                                    tableOutput("table")),
                             column(6,
                                    br(),
                                    br(),
                                    br(),
                                    p(strong("n"), " is the number of times 
                                      that a word is used in that book",
                                      strong("total"), " is the total 
                                      words in that book")
                                    )
                         )
                    
                ),
                tabPanel("Term frequency plots",
                         plotOutput("plot_tf")
                    
                ),
                tabPanel("Zipf's law",
                         tableOutput("table_rank"),
                         
                         fluidRow(
                             column(8,plotOutput("plot_rank"),
                                    p("Zipf’s law states that the frequency that a word
                                      appears is inversely proportional to its rank.")),
                             column(3,sliderInput("range_rank1",
                                                  "Rank's range:",
                                                  min = 1,
                                                  max = 10000,
                                                  value = c(10,1000)),
                                    withMathJax("$$\\text{frequency} \\propto \\frac{1}{\\text{rank}}$$")
                             )
                             
                         )
                         
                ),
                tabPanel("tf_idf",
                         wellPanel(
                           sliderInput("top_n_tf_idf",
                                       label = "Select the top n words",
                                       min = 1, max = 50,
                                       value = 15, step = 1),
                           checkboxGroupInput("books_tf_idf",
                                       label = "Select books to plot",
                                       choices = book_words %>%  
                                         distinct(book) %>% pull(),
                                       selected = book_words %>%  
                                         distinct(book) %>% pull(),
                                       inline = TRUE)
                         ),
                         plotOutput("plot_tf_idf")
                    
                )
            )
        )
    )
  
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  output$summary <- renderPrint({
      summary(austen_books())
  })
  
  output$table <- renderTable({
      head(book_words,7)
  })
  
  output$plot_tf <- renderPlot({
      book_words %>% 
      ggplot(aes(n/total , fill = book), alpha = 0.85) +
          scale_fill_brewer(palette = "Spectral") +
          geom_histogram(show.legend = FALSE) +
          facet_wrap(~book, ncol = 2, scales = "free") +
          xlim(NA,0.0009) +
          theme_minimal()
  })
  
  output$table_rank <- renderTable({
      head(freq_by_rank,4)
  })
  
  rank_subset <- reactive({
      freq_by_rank %>%
          filter(rank < input$range_rank1[2],
                 rank > input$range_rank1[1])})
  
  output$plot_rank <- renderPlot({
      fitvals = lm(log10(`term frequency`) ~ log10(rank), data = rank_subset() )
      freq_by_rank %>%
          ggplot(aes(rank, `term frequency`, color = book)) +
          geom_line(size = 0.5, alpha = 0.8, show.legend = FALSE) +
          geom_abline(intercept = fitvals$coefficients[1],
                      slope = fitvals$coefficients[2], color = "gray50", linetype = 2) +
          geom_vline(xintercept = c(input$range_rank1[1],input$range_rank1[2]),
                     color = c("blue", "red")) +
          scale_x_log10() +
          scale_y_log10()
  })
  
  output$plot_tf_idf <- renderPlot({
    book_words %>%
      filter(book %in% input$books_tf_idf) %>% 
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word)))) %>% 
      group_by(book) %>% 
      top_n(input$top_n_tf_idf) %>% 
      ungroup() %>%
      ggplot(aes(word, tf_idf, fill = book)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~book, ncol = 2, scales = "free") +
      coord_flip()
  })
  
}

shinyApp(ui, server)







