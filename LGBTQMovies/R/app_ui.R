#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    tags$head(
      tags$style(HTML("style.css"))
    ),
    # Your application UI logic
    fluidPage(

      theme = bslib::bs_theme(bootswatch = "cyborg", primary = "#f4b51b"),

      titlePanel(
        div(
          img(src = "www/lgbtq_movies_hex_final.png", height = "100px", style = "margin-right:15px;"),
          "LGBTQ+ Movies Explorer",
          style = "display: flex; align-items: center;"
        )
      ),

      # Rainbow banner
      div(
        style = "height: 25px; width: 100%; background: linear-gradient(to right, #E40303, #FF8C00, #FFED00, #008026, #004DFF, #8B00FF);"
      ),

      sidebarLayout(
        sidebarPanel(
          selectInput("keyword", "Select Keyword:",
                      choices = c("All", "LGBT", "Gay", "Lesbian", "Transgender", "Bisexual",
                                  "Intersex", "Queer", "Genderqueer", "Non-binary", "Gender", "Asexual")),
          sliderInput("yearRange", "Release Year:",
                      min = min(lubridate::year(LGBTQMovies::movies_df$release_date), na.rm = TRUE), max = max(lubridate::year(LGBTQMovies::movies_df$release_date), na.rm = TRUE), value = c(2002, 2022), sep = ""),
          checkboxInput("showAdult", "Include Adult Movies", value = FALSE),
          checkboxInput("topRated", "Show Only Top Rated (8+)", value = FALSE),

            bslib::layout_column_wrap(
              width = 1/3,
              uiOutput("movieCount"),
              uiOutput("avgRating"),
              uiOutput("Popularity")
          )
          # QUERYCHAT - in progress
         # qc$sidebar()
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("Overview",
                     br(),
                     tags$div(
                       mod_Overview_ui("Overview"),
                       style = "margin-top: 10px;"  # it didnt worked
                      )
                     ),
            tabPanel("Trends",
                     br(),
                     mod_Trends_ui("Trends"),
                     br()),
            tabPanel("Top Movies",
                     br(),
                     mod_Top_Movies_ui("Top_Movies"),
                     br()),
            tabPanel("Genres",
                     br(),
                     mod_Genre_Breakdown_ui("Genre_Breakdown"),
                     br()),
            tabPanel("Languages",
                     br(),
                     mod_Language_Insights_ui("Language_Insights"),
                     br())
          ),
          br(),
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
              shinychat::chat_ui(
                'chat',
                placeholder = "Ask me anything about LGBT+ cinema! 🌈✨",
                width = "100%"
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "LGBTQMovies"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
