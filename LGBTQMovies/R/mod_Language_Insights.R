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
        dplyr::mutate(original_language = stringr::str_to_upper(original_language)) |>
        ggplot2::ggplot(
          ggplot2::aes(
            x = forcats::fct_reorder(original_language, n),
            y = n
          )
        ) +
        ggplot2::geom_col(fill = color[[3]]) +
        ggplot2::geom_text(
          ggplot2::aes(label = n),
          hjust = -0.1, color = "white"
        ) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.1))) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = "Top Languages",
          x = "Language",
          y = "Movies"
        ) +
        theme_custom_dark(
          base_size = 12,
          legend_position = "none",
          base_family = "sans"
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank()
        )
    })

    # output$langPlot <- renderPlot({
    #   lang_data <- filteredData() %>%
    #     dplyr::count(original_language_recoded) %>%
    #     dplyr::top_n(10, n)
    #
    #   ggplot2::ggplot(lang_data, aes(
    #     x = fct_reorder(original_language_recoded, n),
    #     y = n,
    #     fill = original_language_recoded,
    #     text = paste0(
    #       "Language: ", original_language_recoded,
    #       "<br>Movies Count: ", n
    #     )
    #   )) +
    #     ggplot2::geom_col() +
    #     # Shadow layer
    #     ggfx::with_shadow(
    #       geom_col(),
    #       sigma = 5,        # blur strength
    #       x_offset = 4,     # horizontal shift
    #       y_offset = -2,    # vertical shift
    #       colour = "grey40"
    #     ) +
    #     ggplot2::coord_flip() +
    #     ggplot2::labs(
    #       title = "Top 10 Languages",
    #       x = "Language",
    #       y = "Count of Movies"
    #     ) +
    #     ggplot2::scale_fill_manual(
    #       values = rep(palette_lgbtq("progress"),
    #                    length.out = length(unique(lang_data$original_language_recoded)))
    #     ) +
    #     theme_app() +
    #     ggplot2::theme(legend.position = "none")
    #
    # })
  })
}

## To be copied in the UI
# mod_Language_Insights_ui("Language_Insights_1")

## To be copied in the server
# mod_Language_Insights_server("Language_Insights_1")
