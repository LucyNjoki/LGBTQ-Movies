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

  mod_Overview_server("Overview", data = filteredData)
  mod_Trends_server("Trends", data = filteredData)
  mod_Genre_Breakdown_server("Genre_Breakdown", data = filteredData)
  mod_Top_Movies_server("Top_Movies", data = filteredData)
  mod_Language_Insights_server("Language_Insights", data = filteredData)

}
