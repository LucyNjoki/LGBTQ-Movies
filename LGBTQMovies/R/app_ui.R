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
    # Your application UI logic
    fluidPage(

      titlePanel("LGBT+ Movies Explorer 🌈"),
      sidebarLayout(
        sidebarPanel(
          selectInput("keyword", "Select Keyword:",
                      choices = c("All", "lgbt", "gay", "lesbian", "transgender", "bisexual",
                                  "intersex", "queer", "genderqueer", "non-binary", "gender", "asexual")),
          sliderInput("yearRange", "Release Year:",
                      min = 1950, max = 2025, value = c(2000, 2025), sep = ""),
          checkboxInput("showAdult", "Include Adult Movies", value = FALSE),
          checkboxInput("topRated", "Show Only Top Rated (8+)", value = FALSE)
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("Overview",
                     mod_Overview_ui("Overview")),

            tabPanel("Trends",
                     mod_Trends_ui("Trends")),

            tabPanel("Genre Breakdown",
                     mod_Genre_Breakdown_ui("Genre_Breakdown")),

            tabPanel("Top Movies",
                     mod_Top_Movies_ui("Top_Movies")),
            tabPanel("Language Insights",
                     mod_Language_Insights_ui("Language_Insights"))
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
