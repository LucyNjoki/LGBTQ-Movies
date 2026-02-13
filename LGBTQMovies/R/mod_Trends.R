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
      bslib::card_header("Movie releases over time", style = "background-color:white;"),
      plotOutput(ns("releaseTrend"))
     ),
    bslib::card(
      style = "background-color:white;",
      full_screen = TRUE,
      bslib::card_header("Movie rating over time", style = "background-color:white;"),
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

      yearly_counts <- data() |>
      dplyr::mutate(year = lubridate::year(as.Date(release_date))) |>
      dplyr::count(year)

      max_row <- yearly_counts |>
          dplyr::filter(n == max(n, na.rm = TRUE))

        ggplot2::ggplot(yearly_counts, ggplot2::aes(x = year, y = n)) +
        ggplot2::geom_line(color = color[[1]], linewidth = 3, lineend = "butt", linejoin = "round", alpha = .8) +
        ggplot2::geom_point(color = color[[1]]) +
        ggplot2::geom_point(data = max_row, colour = "deeppink", size = 3, alpha = 0.9) +
        ggplot2::geom_text(
          data = max_row,
          aes(label = paste0("Max: ", round(n, 2), " (", year, ")")),
          vjust = 0.5,
          hjust = 1.1,
          size = 4,
          colour = "deeppink",
          fontface = "bold"
        ) +
        ggplot2::labs(title = NULL, x = "Year", y = "Number of movies") +
        theme_custom_dark(base_size = 20, legend_position = "none")
    })

    output$ratingTrend <- renderPlot({

        yearly_ratings <- data() |>
        dplyr::mutate(year = lubridate::year(as.Date(release_date))) |>
        dplyr::group_by(year) |>
        dplyr::summarise(avg_rating = mean(vote_average, na.rm = TRUE))

        max_row <- yearly_ratings |>
          dplyr::filter(avg_rating == max(avg_rating, na.rm = TRUE))

        ggplot2::ggplot(yearly_ratings, ggplot2::aes(x = year, y = avg_rating)) +
        ggplot2::geom_line(color = color[[1]], linewidth = 3, lineend = "butt", linejoin = "round", alpha = .8) +
        ggplot2::geom_point(color = color[[1]]) +
          ggplot2::geom_point(data = max_row, colour = "deeppink", size = 3, alpha = 0.9) +
              ggplot2::geom_text(
                data = max_row,
                aes(label = paste0("Max: ", round(avg_rating, 2), "(", year, ")")),
                vjust = 0.5,
                hjust = -0.1,
                size = 4,
                colour = "deeppink",
                fontface = "bold"
              ) +
        ggplot2::labs(title = NULL, x = "Year", y = "Average rating") +
        theme_custom_dark(base_size = 20, legend_position = "none")
    })
  })
}

## To be copied in the UI
# mod_Trends_ui("Trends_1")

## To be copied in the server
# mod_Trends_server("Trends_1")
