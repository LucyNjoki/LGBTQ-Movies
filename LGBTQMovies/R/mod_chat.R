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

        #  OLD CHAT
        # shinychat::chat_ui(
        #   id = ns("chat"),
        #   messages = "What would you like to know about queer cinema?",
        #   placeholder = "...",
        #   icon_assistant = bsicons::bs_icon("rainbow"),
        #   width = "100%"
        # ),

        # NEW CHAT - query
        qc$ui(id = ns("qc"),
                        placeholder = "...",
                        icon_assistant = bsicons::bs_icon("rainbow"),
                        width = "100%"
        )
      )
    ),
    br(),
    DT::DTOutput(ns('table'))
  )
}

#' chat Server Functions
#'
#' @noRd
mod_chat_server <- function(id, data, qc_vals){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # OLD CHAT

    # context <- btw::btw(data)
    #
    # chat <- ellmer::chat_groq(
    #   model = "llama-3.3-70b-versatile",
    #   api_key = Sys.getenv("GROQ_API_KEY_LGBTQ"),
    #   system_prompt = paste("You are an expert in LGBT+ movies. Your task is to provide insightful and engaging responses about LGBT+ cinema, including recommendations, historical context, and cultural significance. You should be able to discuss various genres, notable films, and trends within the LGBT+ film industry. Please respond in a friendly and informative manner. Base ALL your answers on this database:", context, "If the information is not in the database, tell the user."),
    #   seed = 123,
    #   api_args = list(temperature = 0.8)
    # )
    #
    # # shinychat
    # observeEvent(input$chat_user_input, {
    #
    #   chat_result <- try(chat$stream_async(input$chat_user_input), silent = TRUE)
    #
    #   if (inherits(chat_result, "try-error")) {
    #     shinychat::chat_append("chat", "⚠️ Error processing your request. Please try again later")
    #   } else {
    #     shinychat::chat_append("chat", chat_result)
    #   }
    # })

    # QUERYCHAT  - new
    qc_vals <- qc$server(id = "qc", session = session)

    output$table <- DT::renderDT({
      DT::datatable(qc_vals$df() |> dplyr::select(title:original_title, original_language = original_language_recoded, genre = genre_ids_recoded, overview:adult))
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
        list(className = 'dt-center', targets = 1:11),
        list(className = 'dt-left', targets = 0),
        list(targets = 3:4, width = '1000px')
      ),
      rowDefs = list(
        list(render = DT::JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data !== null && data.length > 20 ?",
          "'<div style=\"white-space: normal; word-wrap: break-word; width: 200px;\">' + data + '</div>' : data;",
          "}"
        ), targets = "_all"
      )),
    class = 'stripe hover order-column cell-border',
    rownames = FALSE
    )
  )
  })
}

## To be copied in the UI
# mod_chat_ui("chat_1")

## To be copied in the server
# mod_chat_server("chat_1")
