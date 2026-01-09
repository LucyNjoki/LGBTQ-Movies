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
        dplyr::select(title, vote_average, popularity, release_date) |>
        dplyr::rename(
          "Title" = title,
          "Vote avarage" = vote_average,
          "Popularity" = popularity,
          "Release date" = release_date
        ) |>
        dplyr::distinct()
    },
    options = list(
      pageLength = 10,
      searching = TRUE,
      ordering = TRUE,
      dom = 'tip',
      columnDefs = list(
        list(className = 'dt-center', targets = 1:3),
        list(className = 'dt-left', targets = 0)
      )
    ),
    class = 'stripe hover order-column cell-border',
    rownames = FALSE
    )

  })
}

## To be copied in the UI
# mod_Top_Movies_ui("Top_Movies_1")

## To be copied in the server
# mod_Top_Movies_server("Top_Movies_1")
