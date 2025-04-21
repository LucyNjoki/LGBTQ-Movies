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

    plotOutput(ns("releaseTrend")),
    plotOutput(ns("ratingTrend"))
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
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::labs(title = "LGBT+ Movie Releases Over Time", x = "Year", y = "Number of Movies")
    })

    output$ratingTrend <- renderPlot({
      data() |>
        dplyr::mutate(year = lubridate::year(as.Date(release_date))) |>
        dplyr::group_by(year) |>
        dplyr::summarise(avg_rating = mean(vote_average, na.rm = TRUE)) |>
        ggplot2::ggplot(ggplot2::aes(x = year, y = avg_rating)) +
        ggplot2::geom_line(color = color[[3]]) + ggplot2::geom_point() +
        ggplot2::labs(title = "Average Rating by Year", x = "Year", y = "Avg Rating")
    })

  })
}

## To be copied in the UI
# mod_Trends_ui("Trends_1")

## To be copied in the server
# mod_Trends_server("Trends_1")
