#' chat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # QUERYCHAT - in progress

    tags$div(
      style = "
              display: flex;
              align-items: center;
              gap: 12px;
              background: linear-gradient(90deg, #ff0000, #ff7f00, #ffff00, #00ff00, #0000ff, #4b0082, #8b00ff);
              padding: 12px;
              border-radius: 12px;
              box-shadow: 0 4px 10px rgba(0, 0, 0, 0.15);
            ",
      tags$div(
        style = "
                flex: 1;
                background-color: white;
                border-radius: 10px;
                padding: 10px;
                box-shadow: 0 2px 5px rgba(0,0,0,0.1);
              ",

        # NEW CHAT - query
        qc$ui(id = ns("qc"),
              placeholder = "...",
              icon_assistant = bsicons::bs_icon("rainbow"),
              width = "100%",
              fill = FALSE
        )
      )
    ),
    br(),
    DT::dataTableOutput(ns('table'))
  )
}

#' chat Server Functions
#'
#' @noRd
mod_chat_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    suppressWarnings({
      qc_vals <- qc$server(id = "qc")
    })

    output$table <- DT::renderDataTable({

        df <- qc_vals$df()
        req(df, nrow(df) > 0)

        df
      },
      rownames = FALSE,
      options = list(
        selection = "single",
        pageLength = 5,
        searching = FALSE,
        ordering = TRUE,
        dom = 'ftip',
        columnDefs = list(
          list(className = 'dt-center', targets = 1:9),
          list(className = 'dt-left', targets = 0)
        ),
        class = 'stripe hover order-column cell-border'
      ))
    })
}
## To be copied in the UI
# mod_chat_ui("chat_1")

## To be copied in the server
# mod_chat_server("chat_1")
