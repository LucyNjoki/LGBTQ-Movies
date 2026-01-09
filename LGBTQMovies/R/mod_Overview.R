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

    # shinydashboard::valueBoxOutput(ns("movieCount")),
    # shinydashboard::valueBoxOutput(ns("avgRating")),
    # shinydashboard::valueBoxOutput(ns("popularity"))
    # Key Movie Stats
    fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Key Movie Stats",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(
          column(width = 4, style = "padding-right:10px;", shinydashboard::valueBoxOutput(ns("movieCount"))),
          column(width = 4, style = "padding-right:10px;", shinydashboard::valueBoxOutput(ns("avgRating"))),
          column(width = 4, shinydashboard::valueBoxOutput(ns("popularity")))
        )
      )
    ),

    # Interactive Word Cloud
    # this is causing the error
    fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Common Themes in Movie Descriptions",
        status = "info",
        solidHeader = TRUE,
        div(
          style = "height: 450px;",
          wordcloud2::wordcloud2Output(ns("wordcloud"), width = "100%", height = "100%")
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
mod_Overview_server <- function(id, data, text_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$movieCount <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(nrow(data()), "Movies", icon = icon("film"))
    })

    output$avgRating <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(round(mean(data()$vote_average, na.rm = TRUE), 2),
                               "Avg Rating", icon = icon("star"))
    })

    output$popularity <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(round(mean(data()$popularity, na.rm = TRUE), 2),
                               "Avg Popularity", icon = icon("fire"))
    })

    # Interactive wordcloud2
    output$wordcloud <- wordcloud2::renderWordcloud2({
      wordcloud2::wordcloud2(
        data = text_data(),
        size = 1,
        color = "random-dark",
        shape = "circle",
        rotateRatio = 0.2,
        backgroundColor = "white"
      )
    })

  })
}

## To be copied in the UI
# mod_Overview_ui("Overview_1")

## To be copied in the server
# mod_Overview_server("Overview_1")
