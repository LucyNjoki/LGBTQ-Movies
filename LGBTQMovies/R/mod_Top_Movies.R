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
        dplyr::arrange(dplyr::desc(vote_count), dplyr::desc(vote_average), dplyr::desc(popularity)) |>
        dplyr::select(title, vote_average, popularity, release_date, vote_count) |>
        dplyr::rename(
          "Title" = title,
          "Popularity" = popularity,
          "Vote average" = vote_average,
          "Number of votes" = vote_count,
          "Release date" = release_date
        ) |>
        dplyr::distinct()
    },
    options = list(
      selection = "multiple",
      quoted = TRUE,
      pageLength = 10,
      searching = TRUE,
      searchHighlight = TRUE,
      ordering = TRUE,
      dom = 'ftip',
      columnDefs = list(
        list(className = 'dt-center', targets = 1:4),
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
