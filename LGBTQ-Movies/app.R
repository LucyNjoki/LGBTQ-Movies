library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(lubridate)
library(rio)
library(here)
library(gglgbtq)

ui <- fluidPage(
  titlePanel("LGBT+ Movies Explorer 🌈"),
  div(
    style = "height: 200px; width: 100%; background: linear-gradient(to right, #E40303, #FF8C00, #FFED00, #008026, #004DFF, #8B00FF);"
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("keyword", "Select Keyword:", 
                  choices = c("All", "LGBT", "Gay", "Lesbian", "Transgender", "bisexual", 
                              "Intersex", "Queer", "GendeQqueer", "Non-binary", "gender", "asexual")),
      sliderInput("yearRange", "Release Year:", 
                  min = 1950, max = 2025, value = c(2000, 2025), sep = ""),
      checkboxInput("showAdult", "Include Adult Movies", value = FALSE),
      checkboxInput("topRated", "Show Only Top Rated (8+)", value = FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", 
                 valueBoxOutput("movieCount"),
                 valueBoxOutput("avgRating"),
                 valueBoxOutput("popularity")),
        
        tabPanel("Trends", 
                 plotOutput("releaseTrend"),
                 plotOutput("ratingTrend")),
        
        tabPanel("Genre Breakdown", 
                 plotOutput("genrePlot")),
        
        tabPanel("Top Movies", 
                 dataTableOutput("topMovies")),
        
        tabPanel("Language Insights", 
                 plotOutput("langPlot"))
      )
    )
  )
)


server <- function(input, output, session) {
  filteredData <- reactive({
    movies_data = import(here("Data", "movieDataCleaned.xlsx"))
    df <- movies_data  # your loaded dataset
    
    # Filter by keyword (assuming keyword is a column or derived from a list)
    if (input$keyword != "All") {
      df <- df %>% filter(str_detect(tolower(overview), tolower(input$keyword)))
    }
    
    # Filter by year
    df <- df %>% filter(lubridate::year(as.Date(release_date)) >= input$yearRange[1],
                        lubridate::year(as.Date(release_date)) <= input$yearRange[2])
    
    # Filter adult
    if (!input$showAdult) {
      df <- df %>% filter(adult == FALSE)
    }
    
    # Top rated
    if (input$topRated) {
      df <- df %>% filter(vote_average >= 8)
    }
    
    df
  })
  
  # output$movieCount <- renderValueBox({
  #   valueBox(nrow(filteredData()), "Movies", icon = icon("film"))
  # })
  # 
  # output$avgRating <- renderValueBox({
  #   valueBox(round(mean(filteredData()$vote_average, na.rm = TRUE), 2), "Avg Rating", icon = icon("star"))
  # })
  # 
  # output$popularity <- renderValueBox({
  #   valueBox(round(mean(filteredData()$popularity, na.rm = TRUE), 2), "Avg Popularity", icon = icon("fire"))
  # })
  
  
  output$movieCount <- renderValueBox({
    valueBox(
      nrow(filteredData()), 
      "Movies", 
      icon = icon("film"), 
      color = "purple"
    )
  })
  
  output$avgRating <- renderValueBox({
    valueBox(
      round(mean(filteredData()$vote_average, na.rm = TRUE), 2), 
      "Avg Rating", 
      icon = icon("star"), 
      color = "yellow"
    )
  })
  
  output$popularity <- renderValueBox({
    valueBox(
      round(mean(filteredData()$popularity, na.rm = TRUE), 2), 
      "Avg Popularity", 
      icon = icon("fire"), 
      color = "red"
    )
  })
  
  
  output$releaseTrend <- renderPlot({
    filteredData() %>%
      mutate(year = lubridate::year(as.Date(release_date))) %>%
      count(year) %>%
      ggplot(aes(x = year, y = n)) +
      geom_line() + geom_point() +
      labs(title = "LGBT+ Movie Releases Over Time", 
           x = "Year",
           y = "Number of Movies") +
      ggthemes::theme_stata()
  })
  
  output$ratingTrend <- renderPlot({
    filteredData() %>%
      mutate(year = lubridate::year(as.Date(release_date))) %>%
      group_by(year) %>%
      summarise(avg_rating = mean(vote_average, na.rm = TRUE)) %>%
      ggplot(aes(x = year, y = avg_rating)) +
      geom_line(color = "#68228B") + 
      geom_point(color = "#68228B") +
      labs(title = "Average Rating by Year", 
           x = "Year", 
           y = "Avg Rating") +
      ggthemes::theme_hc()
  })
  
  output$genrePlot <- renderPlot({
    genre_data <- filteredData() %>%
      # unnest(genre_ids) %>%   # assuming it's stored as a list/array
      count(genre_ids_recoded)
    
    ggplot(genre_data, 
           aes(x = fct_reorder(as.factor(genre_ids_recoded), n), 
               y = n)) +
      geom_col(fill = "darkgreen") +
      coord_flip() +
      labs(title = "Most Common Genres", 
           x = "Genre ID", 
           y = "Count")
    
  })
  
  output$langPlot <- renderPlot({
    filteredData() %>%
      count(original_language_recoded) %>%
      top_n(10, n) %>%
      ggplot(aes(x = fct_reorder(original_language_recoded, n),
                 y = n,
                 fill = original_language_recoded)) +
      geom_col() +
      coord_flip() +
      labs(title = "Top Languages", 
           x = "Language", 
           y = "Movies") +
      scale_fill_manual(values = palette_lgbtq("progress")) +
      theme_lgbtq("progress", legend.position = "none")
    
  })
  
  output$topMovies <- renderDataTable({
    filteredData() %>%
      arrange(desc(vote_average)) %>%
      select(title, vote_average, popularity, release_date)
  })
}

shinyApp(ui = ui, server = server)