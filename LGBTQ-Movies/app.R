# Load Libraries
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(lubridate)
library(rio)
library(here)
library(gglgbtq)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(bslib)
library(plotly)

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "superhero"), 

  # Logo at the top
  titlePanel(
    div(
      img(src = "lgbtq_movies_hex_final.png", height = "50px", style = "margin-right:15px;"),
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
        tabPanel(
          "Overview",
          fluidRow(
            column(
              width = 4,
              style = "padding-right:10px;", 
              valueBoxOutput("movieCount")),
            column(
              width = 4, 
              style = "padding-right:10px;", 
              valueBoxOutput("avgRating")),
            column(
              width = 4, 
              valueBoxOutput("popularity"))
          ),
          hr(),
          h4("Common Themes in Movie Descriptions"),
          plotOutput("wordcloud", height = "450px", width = "600px")
        ),
        tabPanel(
          "Trends",
          plotOutput("releaseTrend"),
          plotOutput("ratingTrend")
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

  output$movieCount <- renderValueBox({
    valueBox(
      value = nrow(filteredData()),
      subtitle = "Movies",
      icon = icon("film"),
      color = "purple",
      width = 4
    ) %>%
      tagAppendAttributes(style = "color: white;") # white text on purple
  })

  output$avgRating <- renderValueBox({
    valueBox(
      value = round(mean(filteredData()$vote_average, na.rm = TRUE), 2),
      subtitle = "Avg Rating",
      icon = icon("star"),
      color = "yellow",
      width = 4
    ) %>%
      tagAppendAttributes(style = "color: black;") # black text on yellow
  })

  output$popularity <- renderValueBox({
    valueBox(
      value = round(mean(filteredData()$popularity, na.rm = TRUE), 2),
      subtitle = "Avg Popularity",
      icon = icon("fire"),
      color = "red",
      width = 4
    ) %>%
      tagAppendAttributes(style = "color: white;") # white text on red
  })


  output$wordcloud <- renderPlot({
    text_data <- filteredData() %>%
      select(overview) %>%
      unnest_tokens(word, overview) %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE)

    wordcloud(
      words = text_data$word, freq = text_data$n,
      max.words = 100, colors = brewer.pal(8, "Dark2")
    )
  })

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
    filteredData() %>%
      count(original_language_recoded) %>%
      top_n(10, n) %>%
      ggplot(aes(
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
      scale_fill_manual(values = palette_lgbtq("progress")) +
      theme_lgbtq("progress", legend.position = "none")
  })

  output$topMovies <- renderDataTable({
    filteredData() %>%
      arrange(desc(vote_average)) %>%
      select(title, vote_average, popularity, release_date) |>
      distinct()
  })
}

# Run the App
shinyApp(ui = ui, server = server)
