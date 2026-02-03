#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  filteredData <- reactive({
    movies_data = movies_df
    df <- movies_data  # your loaded dataset

    # Filter by keyword (assuming keyword is a column or derived from a list)
    if (input$keyword != "All") {
      df <- df |> dplyr::filter(stringr::str_detect(tolower(overview), tolower(input$keyword)))
    }

    # Filter by year
    df <- df |> dplyr::filter(lubridate::year(as.Date(release_date)) >= input$yearRange[1],
                        lubridate::year(as.Date(release_date)) <= input$yearRange[2])

    # Filter adult
    if (!input$showAdult) {
      df <- df |> dplyr::filter(adult == FALSE)
    }

    # Top rated
    if (input$topRated) {
      df <- df |> dplyr::filter(vote_average >= 8)
    }

    df
  })

  # Reactive text data
  textData<- reactive({
    filteredData()  |>
      dplyr::select(overview) |>
      tidytext::unnest_tokens(word, overview) |>
      dplyr::anti_join(tidytext::stop_words) |>
      dplyr::count(word, sort = TRUE) |>
      dplyr::filter(n >= 10) |>
      dplyr::top_n(80, n)
  })


  output$movieCount <- renderUI({
    bslib::value_box(
      title = "",
      value = div(dplyr::n_distinct(filteredData()$title), style = "font-size: 13px;"),
      showcase = bsicons::bs_icon("film", size = "1.5rem"),
      theme = "bg-secondary",
      height = "80px"
    )
  })


  output$avgRating <- renderUI({
    bslib::value_box(
      title = "",
      value = div(round(mean(dplyr::n_distinct(filteredData()$vote_average), na.rm = TRUE), 2), style = "font-size: 13px;"),
      showcase = bsicons::bs_icon("fire", size = "2rem"),
      theme = "bg-secondary",
      height = "80px"
    )
  })

  output$popularity <- renderUI({
    bslib::value_box(
      title = "",
      value = div(round(mean(dplyr::n_distinct(filteredData()$popularity), na.rm = TRUE), 2), style = "font-size: 13px;"),
      showcase = bsicons::bs_icon("star", size = "2rem"),
      theme = "bg-secondary",
      height = "80px"
    )
  })


  mod_Overview_server("Overview", data = filteredData, text_data = textData)
  mod_Trends_server("Trends", data = filteredData)
  mod_Genre_Breakdown_server("Genre_Breakdown", data = filteredData)
  mod_Top_Movies_server("Top_Movies", data = filteredData)
  mod_Language_Insights_server("Language_Insights", data = filteredData)
  mod_chat_server("chat", data = filteredData)

}
