#' Top_Movies UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Top_Movies_ui <- function(id) {
  ns <- NS(id)
  tagList(

    DT::dataTableOutput(ns("topMovies"))

  )
}

#' Top_Movies Server Functions
#'
#' @noRd
mod_Top_Movies_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$topMovies <- DT::renderDataTable({
      data() |>
        dplyr::arrange(dplyr::desc(vote_average)) |>
        dplyr::select(title, vote_average, popularity, release_date)
    })

  })
}

## To be copied in the UI
# mod_Top_Movies_ui("Top_Movies_1")

## To be copied in the server
# mod_Top_Movies_server("Top_Movies_1")
