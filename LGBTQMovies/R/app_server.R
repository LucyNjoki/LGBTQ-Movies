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

  chat <- ellmer::chat_groq(
    model = "llama-3.3-70b-versatile",
    api_key = Sys.getenv("GROQ_API_KEY_LGBTQ"),
    system_prompt = "You are an expert in LGBT+ movies. Your task is to provide insightful and engaging responses about LGBT+ cinema, including recommendations, historical context, and cultural significance. You should be able to discuss various genres, notable films, and trends within the LGBT+ film industry. Please respond in a friendly and informative manner.",
    seed = 123,
    api_args = list(temperature = 0.8)
  )

  # Implementação do chat usando shinychat
  observeEvent(input$chat_user_input, {
    ns <- session$ns

    chat_result <- try(chat$stream_async(input$chat_user_input), silent = TRUE)

    if (inherits(chat_result, "try-error")) {
      shinychat::chat_append(ns("chat"), "⚠️ Error processing your request. Please try again later")
    } else {
      shinychat::chat_append(ns("chat"), chat_result)
    }
  })

  mod_Overview_server("Overview", data = filteredData, text_data = textData)
  mod_Trends_server("Trends", data = filteredData)
  mod_Genre_Breakdown_server("Genre_Breakdown", data = filteredData)
  mod_Top_Movies_server("Top_Movies", data = filteredData)
  mod_Language_Insights_server("Language_Insights", data = filteredData)

}
