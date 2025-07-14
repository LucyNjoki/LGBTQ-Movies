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

    plotOutput(ns("langPlot"))

  )
}

#' Language_Insights Server Functions
#'
#' @noRd
mod_Language_Insights_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$langPlot <- renderPlot({
      data() |>
        dplyr::count(original_language) |>
        dplyr::top_n(10, n) |>
        ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(original_language, n), y = n)) +
        ggplot2::geom_col(fill = color[[2]]) +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Top Languages", x = "Language", y = "Movies") +
        ggplot2::theme_classic(base_family = "Helvetica") +
        ggplot2::theme(
          text = ggplot2::element_text(color = "#333333"),
          plot.title = ggplot2::element_text(size = 18, face = "bold", color = "black"),
        )
    })

  })
}

## To be copied in the UI
# mod_Language_Insights_ui("Language_Insights_1")

## To be copied in the server
# mod_Language_Insights_server("Language_Insights_1")
