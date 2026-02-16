#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  filteredData <- reactive({
    df <- dplyr::collect(movies_df)
    print(paste("Initial:", nrow(df)))

    range <- if (is.null(input$yearRange)) c(2002, 2022) else input$yearRange

    df <- df |>
      dplyr::filter(
        lubridate::year(as.Date(release_date)) >= range[1],
        lubridate::year(as.Date(release_date)) <= range[2]
      )

    print(paste("After filtering year:", nrow(df)))

    if (!is.null(input$keyword) && input$keyword != "All") {
      df <- df |>
        dplyr::filter(stringr::str_detect(
          tolower(overview),
          tolower(input$keyword)
        ))
      print(paste("After keyword:", nrow(df)))
    }

    if (isFALSE(input$showAdult)) {
      df <- df |> dplyr::filter(adult == FALSE)
      print(paste("After adult:", nrow(df)))
    }

    if (!is.null(input$topRated) && input$topRated) {
      df <- df |> dplyr::filter(vote_average >= 8)
      print(paste("Após topRated:", nrow(df)))
    }

    df
  })

  filteredData_unique <- reactive({
    filteredData() |> dplyr::distinct(title, .keep_all = TRUE)
  })

  output$movieCount <- renderText({
    df <- filteredData_unique()
    nrow(df) |> as.character()
  })

  output$avgRating <- renderText({
    df <- filteredData_unique()
    va <- round(mean(df$vote_average, na.rm = TRUE), 2)
    print(va)
    va
  })

  output$popularity <- renderText({
    df <- filteredData_unique()
    p <- round(mean(df$popularity, na.rm = TRUE), 2)
    print(p)
    p
  })

  # Reactive text data
  textData <- reactive({
    filteredData() |>
      dplyr::select(overview) |>
      tidytext::unnest_tokens(word, overview) |>
      dplyr::anti_join(tidytext::stop_words, by = dplyr::join_by(word)) |>
      dplyr::count(word, sort = TRUE) |>
      dplyr::filter(n >= 10) |>
      dplyr::top_n(80, n)
  })

  mod_Overview_server(
    "Overview",
    text_data = textData
  )
  mod_Trends_server("Trends", data = filteredData)
  mod_Genre_Breakdown_server("Genre_Breakdown", data = filteredData)
  mod_Top_Movies_server("Top_Movies", data = filteredData)
  mod_Language_Insights_server("Language_Insights", data = filteredData)
  mod_chat_server("chat")
}
