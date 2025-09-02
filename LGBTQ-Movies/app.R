# Load Libraries
library(shiny)
library(shinydashboard)
library(shiny.fluent)
library(DT)
library(tidyverse)
library(lubridate)
library(rio)
library(here)
library(gglgbtq)
library(tidytext)
library(wordcloud2)
library(RColorBrewer)
library(bslib)
library(plotly)

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "superhero"), 
  
  # Logo at the top
  titlePanel(
    div(
      img(src = "lgbtq_movies_hex_final.png", height = "100px", style = "margin-right:15px;"),
      "LGBT+ Movies Explorer",
      style = "display: flex; align-items: center;"
    )
  ),
  
  # Rainbow banner
  div(
    style = "height: 25px; width: 100%; background: linear-gradient(to right, #E40303, #FF8C00, #FFED00, #008026, #004DFF, #8B00FF);"
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("keyword", "Select Keyword:",
                  choices = c(
                    "All", "LGBT", "Gay", "Lesbian", "Transgender", "Bisexual",
                    "Intersex", "Queer", "Genderqueer", "Non-binary", "Gender", "Asexual"
                  )
      ),
      sliderInput("yearRange", "Release Year:",
                  min = 1950, max = 2025, value = c(2000, 2025), sep = ""
      ),
      checkboxInput("showAdult", "Include Adult Movies", value = FALSE),
      checkboxInput("topRated", "Show Only Top Rated (8+)", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        # tabPanel(
        #   "Overview",
        #   
        #   # Key Movie Stats
        #   fluidRow(
        #     box(
        #       width = 12,
        #       title = "Key Movie Stats",
        #       status = "primary",
        #       solidHeader = TRUE,
        #       fluidRow(
        #         column(width = 4, style = "padding-right:10px;", valueBoxOutput("movieCount")),
        #         column(width = 4, style = "padding-right:10px;", valueBoxOutput("avgRating")),
        #         column(width = 4, valueBoxOutput("popularity"))
        #       )
        #     )
        #   ),
        #   
        #   # Word Cloud
        #   fluidRow(
        #     box(
        #       width = 12,
        #       title = "Common Themes in Movie Descriptions",
        #       status = "info",
        #       solidHeader = TRUE,
        #       plotOutput("wordcloud", width = "100%", height = "450px"),
        #       p(
        #         "This word cloud highlights frequently used words in movie descriptions. Larger words appear more often, while rare words are filtered out for clarity.",
        #         style = "font-style: italic; color: #555; margin-top: 10px;"
        #       )
        #     )
        #   )
        # ),
          # Word Cloud Selector
          tabPanel(
            "Overview",
            
            # Key Movie Stats
            fluidRow(
              box(
                width = 12,
                title = "Key Movie Stats",
                status = "primary",
                solidHeader = TRUE,
                fluidRow(
                  column(width = 4, style = "padding-right:10px;", valueBoxOutput("movieCount")),
                  column(width = 4, style = "padding-right:10px;", valueBoxOutput("avgRating")),
                  column(width = 4, valueBoxOutput("popularity"))
                )
              )
            ),
            
            # Interactive Word Cloud
            fluidRow(
              box(
                width = 12,
                title = "Common Themes in Movie Descriptions",
                status = "info",
                solidHeader = TRUE,
                div(
                  style = "height: 450px;",
                  wordcloud2Output("wordcloud", width = "100%", height = "100%")
                ),
                tags$p(
                  "This word cloud highlights frequently used words in movie descriptions. Larger words appear more often, while rare words are filtered out for clarity.",
                  style = "font-style: italic; color: #555; margin-top: 10px;"
                )
              )
            )
          ),
        
        tabPanel(
          "Trends",
          fluidRow(
            column(
              width = 12,
              card(
                full_screen = TRUE,
                card_header("LGBT+ Movie Releases Over Time"),
                plotOutput("releaseTrend", height = "350px")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              card(
                full_screen = TRUE,
                card_header("Average Rating by Year"),
                plotOutput("ratingTrend", height = "350px")
              )
            )
          )
        ),
        
        tabPanel(
          "Genre Breakdown",
          plotlyOutput("genrePlot")
        ),
        tabPanel(
          "Top Movies",
          dataTableOutput("topMovies")
        ),
        tabPanel(
          "Language Insights",
          plotOutput("langPlot")
        ),
        # Data Dictionary Tab
        tabPanel(
          "Movies DB Data Dictionary",
          fluidRow(
            box(width = 11, status = "info", solidHeader = TRUE,
                p(HTML(
                  "This dataset is part of the <strong>tidyrainbow</strong> project, produced by the <a href='https://rainbowr.org/' target='_blank'><em>rainbowR</em></a> community, 
  which curates metadata on LGBTQ+ themed movies. 
  It includes information such as titles, genres, release dates, ratings, popularity, and language.
  The goal is to support exploration and analysis of LGBTQ+ representation in cinema.<br><br>
  <strong>Source:</strong> <a href='https://github.com/r-lgbtq/tidyrainbow/tree/main/data/LGBTQ-movie-database' target='_blank'>
  LGBTQ Movie Database on GitHub</a>"
                ))
            )
          ),
          dataTableOutput("data_dict")
        )
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  filteredData <- reactive({
    movies_data <- import(here("Data", "movieDataCleaned.xlsx"))
    df <- movies_data
    
    if (input$keyword != "All") {
      df <- df %>%
        filter(str_detect(tolower(overview), tolower(input$keyword)))
    }
    
    df <- df %>% filter(
      lubridate::year(as.Date(release_date)) >= input$yearRange[1],
      lubridate::year(as.Date(release_date)) <= input$yearRange[2]
    )
    
    if (!input$showAdult) {
      df <- df %>% filter(adult == FALSE)
    }
    
    if (input$topRated) {
      df <- df %>% filter(vote_average >= 8)
    }
    
    df
  })
  
  # Reactive text data
  text_data <- reactive({
    filteredData() %>%
      select(overview) %>%
      unnest_tokens(word, overview) %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE) %>%
      filter(n >= 10) %>%
      top_n(80, n)
  })
  
  # Reactive text data
  text_data <- reactive({
    filteredData() %>%
      select(overview) %>%
      unnest_tokens(word, overview) %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE) %>%
      filter(n >= 10) %>%
      top_n(80, n)
  })
  
  # Interactive wordcloud2
  output$wordcloud <- renderWordcloud2({
    wordcloud2(
      data = text_data(),
      size = 1,
      color = "random-dark",
      shape = "circle",
      rotateRatio = 0.2,
      backgroundColor = "white"
    )
  })
  
  # Value boxes
  output$movieCount <- renderValueBox({
    valueBox(formatC(nrow(filteredData()), format = "d", big.mark = ","),
             "Total Movies", icon = icon("film"), color = "blue")
  })
  
  output$avgRating <- renderValueBox({
    avg <- round(mean(filteredData()$vote_average, na.rm = TRUE), 1)
    valueBox(avg, "Average Rating", icon = icon("star"), color = "green")
  })
  
  output$popularity <- renderValueBox({
    pop <- round(mean(filteredData()$popularity, na.rm = TRUE), 1)
    valueBox(pop, "Avg Popularity", icon = icon("fire"), color = "red")
  })
  
  # output$movieCount <- renderValueBox({
  #   valueBox(
  #     formatC(nrow(filteredData()), format = "d", big.mark = ","),
  #     subtitle = "Movies",
  #     icon = icon("film"),
  #     color = "purple",
  #     width = 4
  #   ) %>%
  #     tagAppendAttributes(style = "color: white;") # white text on purple
  # })
  # 
  # output$avgRating <- renderValueBox({
  #   valueBox(
  #     value = round(mean(filteredData()$vote_average, na.rm = TRUE), 2),
  #     subtitle = "Avg Rating",
  #     icon = icon("star"),
  #     color = "yellow",
  #     width = 4
  #   ) %>%
  #     tagAppendAttributes(style = "color: black;") # black text on yellow
  # })
  # 
  # output$popularity <- renderValueBox({
  #   valueBox(
  #     value = round(mean(filteredData()$popularity, na.rm = TRUE), 2),
  #     subtitle = "Avg Popularity",
  #     icon = icon("fire"),
  #     color = "red",
  #     width = 4
  #   ) %>%
  #     tagAppendAttributes(style = "color: white;") # white text on red
  # })
  # 
  # 
  # output$wordcloud <- renderPlot({
  #   text_data <- filteredData() %>%
  #     select(overview) %>%
  #     unnest_tokens(word, overview) %>%
  #     anti_join(stop_words) %>%
  #     count(word, sort = TRUE)
  # 
  #   wordcloud(
  #     words = text_data$word, freq = text_data$n,
  #     max.words = 100, colors = brewer.pal(8, "Dark2")
  #   )
  # })
  
  # output$wordcloud <- renderWordcloud2({
  #   text_data <- filteredData() %>%
  #     select(overview) %>%
  #     unnest_tokens(word, overview) %>%
  #     anti_join(stop_words) %>%
  #     count(word, sort = TRUE) %>%
  #     filter(n >= 5) %>%
  #     top_n(80, n)
  #   
  #   wordcloud2(data = text_data, size = 1, 
  #              color = "random-dark", 
  #              shape = "circle")
  # })
  
  
  # output$wordcloud <- renderPlot({
  #   text_data <- filteredData() %>%
  #     select(overview) %>%
  #     unnest_tokens(word, overview) %>%
  #     anti_join(stop_words) %>%
  #     count(word, sort = TRUE)
  #   
  #   # Build wordcloud
  #   wordcloud2(data = text_data, size = 1.5, color = "random-dark", shape = "circle")
  # wordcloud(
  #   words = text_data$word, 
  #   freq = text_data$n,
  #   min.freq = 5,                   # Increase min frequency (hide very rare words)
  #   max.words = 100,
  #   scale = c(6, 1.2),              # Larger text (increase first number)
  #   colors = brewer.pal(8, "Dark2"),# Stronger contrast
  #   random.order = FALSE,
  #   rot.per = 0.2
  # )
  
  # Add caption under plot
  #   mtext("Word size reflects frequency of occurrence in movie descriptions", 
  #         side = 1, line = 4, cex = 0.8, col = "grey40")
  # })
  
  
  output$releaseTrend <- renderPlot({
    filteredData() %>%
      mutate(year = lubridate::year(as.Date(release_date))) %>%
      count(year) %>%
      ggplot(aes(x = year, y = n)) +
      geom_line(colour = "black", size = 0.8) +
      geom_point(colour = "black", size = 1.2) +
      labs(
        title = "LGBT+ Movie Releases Over Time",
        x = "Year",
        y = "Number of Movies"
      ) +
      theme_minimal()
  })
  
  output$ratingTrend <- renderPlot({
    filteredData() %>%
      mutate(year = lubridate::year(as.Date(release_date))) %>%
      group_by(year) %>%
      summarise(avg_rating = mean(vote_average, na.rm = TRUE)) %>%
      ggplot(aes(x = year, y = avg_rating)) +
      geom_line(color = "#68228B", size = 0.8) +
      geom_point(color = "#68228B", size = 1.2) +
      labs(title = "Average Rating by Year", x = "Year", y = "Avg Rating") +
      ggthemes::theme_hc()
  })
  
  output$genrePlot <- renderPlotly({
    genre_data <- filteredData() %>%
      count(genre_ids_recoded) %>%
      mutate(genre = fct_reorder(genre_ids_recoded, n))
    
    p <- ggplot(genre_data, aes(x = genre, y = n)) +
      geom_col(fill = "darkgreen") +
      coord_flip() +
      labs(title = "Most Common Genres", x = "Genre", y = "Count") +
      ggdark::dark_theme_dark()
    
    ggplotly(p)
  })
  
  
  
  output$langPlot <- renderPlot({
    lang_data <- filteredData() %>%
      count(original_language_recoded) %>%
      top_n(10, n)
    
    ggplot(lang_data, aes(
      x = fct_reorder(original_language_recoded, n),
      y = n,
      fill = original_language_recoded
    )) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Top 10 Languages",
        x = "Language",
        y = "Movies"
      ) +
      scale_fill_manual(
        values = rep(palette_lgbtq("progress"),
                     length.out = length(unique(lang_data$original_language_recoded)))
      ) +
      theme_lgbtq("progress", legend.position = "none")
  })
  
  
  output$topMovies <- renderDataTable({
    filteredData() %>%
      arrange(desc(vote_average)) %>%
      select(title, vote_average, popularity, release_date) |>
      mutate(release_date = ymd(release_date)) |> 
      distinct()
  })
  
  output$data_dict <- renderDataTable({
    import(here("Data", "movie_db_data_dictionary.xlsx")) %>%
      distinct()
  }, options = list(pageLength = 10, scrollX = TRUE))
  
}

# Run the App
shinyApp(ui = ui, server = server)
