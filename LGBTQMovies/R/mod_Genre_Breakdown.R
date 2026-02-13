#' Genre_Breakdown UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Genre_Breakdown_ui <- function(id) {
  ns <- NS(id)
  tagList(

    plotly::plotlyOutput(ns("genrePlot"))

  )
}

#' Genre_Breakdown Server Functions
#'
#' @noRd
mod_Genre_Breakdown_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$genrePlot <- plotly::renderPlotly({
      genre_data <- data() |>
        tidyr::unnest(genre_ids_recoded) |>    # assuming it's stored as a list/array
        dplyr::count(genre_ids_recoded)

      p <- ggplot2::ggplot(genre_data, ggplot2::aes(x = forcats::fct_reorder(as.factor(genre_ids_recoded), n), y = n)) +
        ggplot2::geom_col(fill = color[[6]]) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.1))) +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Most Common Genres", x = NULL, y = NULL) +
        theme_custom_dark(base_size = 13, legend_position = "none", base_family = "inter", grid_major = FALSE) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank()
        )
      plotly::ggplotly(p, tooltip = "y")
    })

    # output$genrePlot <- renderPlotly({
    #
    #   genre_data <- filteredData() %>%
    #     dplyr::count(genre_ids_recoded) %>%
    #     dplyr::mutate(
    #       genre = fct_reorder(genre_ids_recoded, n)
    #     )
    #
    #   p <- ggplot2::ggplot(
    #     genre_data,
    #     ggplot2::aes(
    #       x = genre,
    #       y = n,
    #       text = paste0(
    #         "Genre: ", genre,
    #         "<br>Count: ", n
    #       )
    #     )
    #   ) +
    #     ggplot2::geom_col(fill = "#2ecc71") +
    #     ggplot2::coord_flip() +
    #     ggplot2::labs(
    #       title = "Most Common Genres",
    #       x = NULL,      # remove axis label
    #       y = NULL       # remove y label (since we hide axis)
    #     ) +
    #     theme_app() +
    #     ggplot2::theme(
    #       axis.title.x = element_blank(),
    #       axis.text.x  = element_blank(),
    #       axis.ticks.x = element_blank(),
    #       panel.grid.minor = element_blank()
    #     )
    #
    #   ggplotly(p, tooltip = "text") %>%
    #     layout(
    #       font = list(
    #         family = "sans",
    #         size = 14,
    #         color = "black"
    #       ),
    #       xaxis = list(
    #         showgrid = FALSE,
    #         zeroline = FALSE
    #       ),
    #       yaxis = list(
    #         title = ""
    #       )
    #     )
    #
    # })
  }
)}

## To be copied in the UI
# mod_Genre_Breakdown_ui("Genre_Breakdown_1")

## To be copied in the server
# mod_Genre_Breakdown_server("Genre_Breakdown_1")
