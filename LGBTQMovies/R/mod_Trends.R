#' Trends UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Trends_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      style = "background-color:white;",
      full_screen = TRUE,
      bslib::card_header("LGBT+ Movie Releases Over Time", style = "background-color:white;"),
      plotOutput(ns("releaseTrend"))
     ),
    bslib::card(
      style = "background-color:white;",
      full_screen = TRUE,
      bslib::card_header("Average Rating by Year", style = "background-color:white;"),
      plotOutput(ns("ratingTrend"))
    )

  )
}

#' Trends Server Functions
#'
#' @noRd
mod_Trends_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$releaseTrend <- renderPlot({
      data() |>
        dplyr::mutate(year = lubridate::year(as.Date(release_date))) |>
        dplyr::count(year) |>
        ggplot2::ggplot(ggplot2::aes(x = year, y = n)) +
        ggplot2::geom_line(color = color[[1]]) + ggplot2::geom_point(color = color[[1]]) +
        ggplot2::labs(title = NULL, x = "Year", y = "Number of Movies") +
        theme_custom_dark(base_size = 12, legend_position = "none")
    })

    output$ratingTrend <- renderPlot({
      data() |>
        dplyr::mutate(year = lubridate::year(as.Date(release_date))) |>
        dplyr::group_by(year) |>
        dplyr::summarise(avg_rating = mean(vote_average, na.rm = TRUE)) |>
        ggplot2::ggplot(ggplot2::aes(x = year, y = avg_rating)) +
        ggplot2::geom_line(color = color[[2]]) + ggplot2::geom_point(color = color[[2]]) +
        ggplot2::labs(title = NULL, x = "Year", y = "Avg Rating") +
        theme_custom_dark(base_size = 12, legend_position = "none", base_family = "sans")
    })

  })
}

## To be copied in the UI
# mod_Trends_ui("Trends_1")

## To be copied in the server
# mod_Trends_server("Trends_1")
