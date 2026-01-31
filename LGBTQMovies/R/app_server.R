#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  filteredData <- reactive({
    movies_data = movies_df
    df <- movies_data  # your loaded dataset

    # Filter by keyword (assuming keyword is a column or derived from a list)
    if (input$keyword != "All") {
      df <- df |> dplyr::filter(stringr::str_detect(tolower(overview), tolower(input$keyword)))
    }

    # Filter by year
    df <- df |> dplyr::filter(lubridate::year(as.Date(release_date)) >= input$yearRange[1],
                        lubridate::year(as.Date(release_date)) <= input$yearRange[2])

    # Filter adult
    if (!input$showAdult) {
      df <- df |> dplyr::filter(adult == FALSE)
    }

    # Top rated
    if (input$topRated) {
      df <- df |> dplyr::filter(vote_average >= 8)
    }

    df
  })

  # Reactive text data
  textData<- reactive({
    filteredData()  |>
      dplyr::select(overview) |>
      tidytext::unnest_tokens(word, overview) |>
      dplyr::anti_join(tidytext::stop_words) |>
      dplyr::count(word, sort = TRUE) |>
      dplyr::filter(n >= 10) |>
      dplyr::top_n(80, n)
  })


  output$movieCount <- renderUI({
    bslib::value_box(
      title = "",
      value = dplyr::n_distinct(filteredData()$title),
      showcase = bsicons::bs_icon("film", size = "1.5rem"),
      theme = "bg-secondary",
      height = "100px"
    )
  })


  output$avgRating <- renderUI({
    bslib::value_box(
      title = "",
      value = round(mean(dplyr::n_distinct(filteredData()$vote_average), na.rm = TRUE), 2),
      showcase = bsicons::bs_icon("fire", size = "2rem"),
      theme = "bg-secondary",
      height = "100px"
    )
  })

  output$popularity <- renderUI({
    bslib::value_box(
      title = "",
      value = round(mean(dplyr::n_distinct(filteredData()$popularity), na.rm = TRUE), 2),
      showcase = bsicons::bs_icon("fire", size = "2rem"),
      theme = "bg-secondary",
      height = "100px"
    )
    })

  context <- btw::btw(movies_df)

  chat <- ellmer::chat_groq(
    model = "llama-3.3-70b-versatile",
    api_key = Sys.getenv("GROQ_API_KEY_LGBTQ"),
    system_prompt = paste("You are an expert in LGBT+ movies. Your task is to provide insightful and engaging responses about LGBT+ cinema, including recommendations, historical context, and cultural significance. You should be able to discuss various genres, notable films, and trends within the LGBT+ film industry. Please respond in a friendly and informative manner. Base ALL your answers on this database:", context, "If the information is not in the database, tell the user."),
    seed = 123,
    api_args = list(temperature = 0.8)
  )

  shinychat::chat_ui("chat", message = "What would you like to know about queer cinema?", icon_assistant = bsicons::bs_icon("rainbow"))

  # shinychat
  observeEvent(input$chat_user_input, {
    ns <- session$ns

    chat_result <- try(chat$stream_async(input$chat_user_input), silent = TRUE)

    if (inherits(chat_result, "try-error")) {
      shinychat::chat_append(ns("chat"), "⚠️ Error processing your request. Please try again later")
    } else {
      shinychat::chat_append(ns("chat"), chat_result)
    }
  })

  # QUERYCHAT - in progress
  # qc_vals <- qc$server()
  #
  # output$table <- DT::renderDT({
  #   datatable(qc_vals$df())
  # })

  mod_Overview_server("Overview", data = filteredData, text_data = textData)
  mod_Trends_server("Trends", data = filteredData)
  mod_Genre_Breakdown_server("Genre_Breakdown", data = filteredData)
  mod_Top_Movies_server("Top_Movies", data = filteredData)
  mod_Language_Insights_server("Language_Insights", data = filteredData)

}
