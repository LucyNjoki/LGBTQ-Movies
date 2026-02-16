#' Overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Common themes",
        status = "info",
        solidHeader = TRUE,
        div(
          style = "height: 400px;",
          wordcloud2::wordcloud2Output(
            ns("wordcloud"),
            width = "100%",
            height = "100%"
          )
        ),
        tags$p(
          "This word cloud highlights frequently used words in movie descriptions. Larger words appear more often, while rare words are filtered out for clarity.",
          style = "font-style: italic; color: #555; margin-top: 10px;"
        )
      )
    )
  )
}

#' Overview Server Functions
#'
#' @noRd
mod_Overview_server <- function(id, data, text_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Interactive wordcloud2
    output$wordcloud <- wordcloud2::renderWordcloud2({
      wordcloud2::wordcloud2(
        data = text_data(),
        size = 1,
        color = "random-light",
        shape = "circle",
        rotateRatio = 0.2,
        backgroundColor = "black",
        shuffle = FALSE
      )
    })
  })
}

## To be copied in the UI
# mod_Overview_ui("Overview_1")

## To be copied in the server
# mod_Overview_server("Overview_1")
