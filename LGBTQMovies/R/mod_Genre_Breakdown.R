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

    plotOutput(ns("genrePlot"))

  )
}

#' Genre_Breakdown Server Functions
#'
#' @noRd
mod_Genre_Breakdown_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$genrePlot <- renderPlot({
      genre_data <- data() |>
        tidyr::unnest(genre_ids) |>    # assuming it's stored as a list/array
        dplyr::count(genre_ids)

      ggplot2::ggplot(genre_data, ggplot2::aes(x = forcats::fct_reorder(as.factor(genre_ids), n), y = n)) +
        ggplot2::geom_col(fill = color[[1]]) +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Most Common Genres", x = "Genre ID", y = "Count") +
        ggplot2::theme_classic(base_family = "Helvetica") +
        ggplot2::theme(
          text = ggplot2::element_text(color = "#333333"),
          plot.title = ggplot2::element_text(size = 18, face = "bold", color = "black"),
        )
    })
  }
)}

## To be copied in the UI
# mod_Genre_Breakdown_ui("Genre_Breakdown_1")

## To be copied in the server
# mod_Genre_Breakdown_server("Genre_Breakdown_1")
