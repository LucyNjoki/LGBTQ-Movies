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

# library(ggfx)
#
# theme_app <- function(base_size = 14, base_family = "sans") {
#
#   theme_bw(base_size = base_size, base_family = base_family) +
#
#     theme(
#       # Titles
#       plot.title = element_text(
#         size = base_size + 4,
#         face = "bold",
#         hjust = 0
#       ),
#
#       # Axis titles
#       axis.title = element_text(
#         size = base_size,
#         face = "bold"
#       ),
#
#       # Axis text
#       axis.text = element_text(
#         size = base_size - 2
#       ),
#
#       # Grid
#       panel.grid.minor = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.grid.major.x = element_blank(),
#
#       # Background
#       plot.background = element_rect(fill = "white", color = NA),
#       panel.background = element_rect(fill = "white", color = NA),
#
#       # Legend
#       legend.title = element_text(face = "bold"),
#       legend.position = "right"
#     )
# }

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

    # output$releaseTrend <- renderPlot({
    #
    #   yearly_counts <- filteredData() %>%
    #     dplyr::mutate(year = lubridate::year(as.Date(release_date))) %>%
    #     dplyr::count(year)
    #
    #   max_row <- yearly_counts %>%
    #     dplyr::filter(n == max(n, na.rm = TRUE))
    #
    #   ggplot2::ggplot(yearly_counts, aes(x = year, y = n)) +
    #     geom_line(colour = "black", size = 0.8) +
    #     geom_point(colour = "black", size = 1.2) +
    #
    #     # Highlight max point
    #     geom_point(data = max_row, colour = "red", size = 1.5) +
    #
    #     # Add text label
    #     ggplot2::geom_text(
    #       data = max_row,
    #       aes(label = paste0("Max: ", n, " (", year, ")")),
    #       vjust = -0.5,
    #       colour = "red",
    #       fontface = "bold"
    #     ) +
    #
    #     ggplot2::labs(
    #       title = "LGBT+ Movie Releases Over Time",
    #       x = "Year",
    #       y = "Number of Movies"
    #     ) +
    #     theme_app()
    # })

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

    # output$ratingTrend <- renderPlot({
    #
    #   yearly_ratings <- filteredData() %>%
    #     mutate(year = lubridate::year(as.Date(release_date))) %>%
    #     group_by(year) %>%
    #     summarise(avg_rating = mean(vote_average, na.rm = TRUE))
    #
    #   max_row <- yearly_ratings %>%
    #     filter(avg_rating == max(avg_rating, na.rm = TRUE))
    #
    #   ggplot2::ggplot(yearly_ratings, aes(x = year, y = avg_rating)) +
    #     ggplot2::geom_line(color = "#68228B", size = 0.8) +
    #     ggplot2::geom_point(color = "#68228B", size = 1.2) +
    #
    #     # Highlight max point
    #     ggplot2::geom_point(data = max_row, colour = "red", size = 1.5) +
    #
    #     # Add annotation
    #     ggplot2::geom_text(
    #       data = max_row,
    #       aes(label = paste0("Max: ", round(avg_rating, 2), " (", year, ")")),
    #       vjust = -0.5,
    #       colour = "red",
    #       fontface = "bold"
    #     ) +
    #
    #     ggplot2::labs(title = "Average Rating by Year", x = "Year", y = "Avg Rating") +
    #     theme_app()
    # })

  })
}

## To be copied in the UI
# mod_Trends_ui("Trends_1")

## To be copied in the server
# mod_Trends_server("Trends_1")
