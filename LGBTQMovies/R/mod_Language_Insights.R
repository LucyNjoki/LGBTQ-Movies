#' Language_Insights UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Language_Insights_ui <- function(id) {
  ns <- NS(id)
  tagList(

    plotly::plotlyOutput(ns("langPlot"))

  )
}

#' Language_Insights Server Functions
#'
#' @noRd
mod_Language_Insights_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$langPlot <- plotly::renderPlotly({
      p <- data() |>
        dplyr::count(original_language_recoded) |>
        dplyr::top_n(10, n) |>
        ggplot2::ggplot(
          ggplot2::aes(
            x = forcats::fct_reorder(original_language_recoded, n),
            y = n
          )
        ) +
        ggplot2::geom_col(fill = color[[3]]) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.1))) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = "Top languages",
          x = NULL,
          y = "Movies"
        ) +
        theme_custom_dark(
          base_size = 13,
          legend_position = "none",
          grid_major = FALSE
        ) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(face = "bold"),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank()
        )

      plotly::ggplotly(p, tooltip = "y") |>
        plotly::layout(
          xaxis = list(title = list(font = list(family = "Inter"))),
          yaxis = list(title = list(font = list(family = "Inter"))),
          font  = list(family = "Inter")
        )
  })

})}

## To be copied in the UI
# mod_Language_Insights_ui("Language_Insights_1")

## To be copied in the server
# mod_Language_Insights_server("Language_Insights_1")
