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
          img(
            src = "www/lgbtq_movies_hex_final.png",
            height = "100px",
            style = "margin-right:15px;"
          ),
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
          selectInput(
            "keyword",
            "Select Keyword:",
            choices = c(
              "All",
              "LGBT",
              "Gay",
              "Lesbian",
              "Transgender",
              "Bisexual",
              "Intersex",
              "Queer",
              "Genderqueer",
              "Non-binary",
              "Gender",
              "Asexual"
            )
          ),
          sliderInput(
            "yearRange",
            "Release Year:",
            min = min(
              lubridate::year(LGBTQMovies::movies_df$release_date),
              na.rm = TRUE
            ),
            max = max(
              lubridate::year(LGBTQMovies::movies_df$release_date),
              na.rm = TRUE
            ),
            value = c(2002, 2022),
            sep = ""
          ),
          checkboxInput("showAdult", "Include Adult Movies", value = FALSE),
          checkboxInput("topRated", "Show Only Top Rated (8+)", value = FALSE)
        ),

        mainPanel(
          tabsetPanel(
            tabPanel(
              "Overview",
              br(),
              # bslib::layout_column_wrap(
              #   width = 1 / 3,
              #   height = "80px",
              #   bslib::value_box(
              #     title = "",
              #     value = div(12),
              #      showcase = bsicons::bs_icon("film", size = "1em"),
              #      showcase_layout = "top right"
              #   ),
              #   bslib::value_box(
              #     title = "",
              #     value = div(textOutput("avgRating")),
              #     showcase = bsicons::bs_icon("star", size = "1em"),
              #     showcase_layout = "top right"
              #   ),
              #   bslib::value_box(
              #     title = "",
              #     value = textOutput("popularity"),
              #     showcase = bsicons::bs_icon("fire", size = "1em"),
              #     showcase_layout = "top right"
              #   )
              # ),
              mod_Overview_ui("Overview"),
              br()
            ),
            tabPanel("Trends", br(), mod_Trends_ui("Trends"), br()),
            tabPanel("Top Movies", br(), mod_Top_Movies_ui("Top_Movies"), br()),
            tabPanel(
              "Genres",
              br(),
              mod_Genre_Breakdown_ui("Genre_Breakdown"),
              br()
            ),
            tabPanel(
              "Languages",
              br(),
              mod_Language_Insights_ui("Language_Insights"),
              br()
            ),
            tabPanel("🌈", br(), mod_chat_ui("chat"), br())
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
