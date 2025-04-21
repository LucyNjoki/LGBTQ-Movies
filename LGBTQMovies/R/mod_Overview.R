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

    shinydashboard::valueBoxOutput(ns("movieCount")),
    shinydashboard::valueBoxOutput(ns("avgRating")),
    shinydashboard::valueBoxOutput(ns("popularity"))
  )
}

#' Overview Server Functions
#'
#' @noRd
mod_Overview_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$movieCount <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(nrow(data()), "Movies", icon = icon("film"))
    })

    output$avgRating <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(round(mean(data()$vote_average, na.rm = TRUE), 2), "Avg Rating", icon = icon("star"))
    })

    output$popularity <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(round(mean(data()$popularity, na.rm = TRUE), 2), "Avg Popularity", icon = icon("fire"))
    })

  })
}

## To be copied in the UI
# mod_Overview_ui("Overview_1")

## To be copied in the server
# mod_Overview_server("Overview_1")
