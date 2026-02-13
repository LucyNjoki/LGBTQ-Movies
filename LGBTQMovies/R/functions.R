# Functions for plots themes
library(ggplot2)
library(grid)   # for unit()
library(sysfonts)
library(showtext)

# Add Google font
font_add_google("Inter", "inter")
showtext_auto()

#' Create a customisable ggplot2 theme
#'
#' @param base_size numeric base font size (default 11)
#' @param base_family character font family (default "sans")
#' @param axis_title_size numeric axis title size (relative to base)
#' @param axis_text_size numeric axis text size (relative to base)
#' @param legend_size numeric legend text size (relative to base)
#' @param strip_text_size numeric facet strip text size (relative to base)
#' @param panel_border logical whether to draw panel border
#' @param grid_major logical show major grid lines
#' @param grid_minor logical show minor grid lines
#' @param grid_color color of grid lines
#' @param panel_fill panel background fill color
#' @param plot_background overall plot background color
#' @param legend_position legend position (e.g. "right", "top", c(0.8,0.2))
#' @param plot_title_face font face for plot title ("bold", "plain", etc.)
#' @param subtitle_face font face for subtitle
#' @param caption_face font face for caption
#' @return a ggplot2 theme object
theme_custom <- function(
    base_size = 11, base_family = "inter",
    axis_title_size = base_size * 1.05,
    axis_text_size  = base_size * 0.9,
    legend_size     = base_size * 0.9,
    strip_text_size = base_size * 0.95,
    panel_border    = FALSE,
    grid_major      = TRUE,
    grid_minor      = FALSE,
    grid_color      = "grey90",
    panel_fill      = "white",
    plot_background = "white",
    legend_position = "right",
    plot_title_face = "bold",
    subtitle_face   = "plain",
    caption_face    = "italic"
) {
  # Base theme to build upon
  th <- theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      # Plot-level
      plot.background = element_rect(fill = plot_background, colour = NA),
      panel.background = element_rect(fill = panel_fill, colour = NA),
      plot.title = element_text(size = rel(1.15), face = plot_title_face, margin = margin(b = 6)),
      plot.subtitle = element_text(size = rel(0.95), face = subtitle_face, margin = margin(b = 6)),
      plot.caption = element_text(size = rel(0.75), face = caption_face, hjust = 1, margin = margin(t = 6)),
      plot.margin = margin(10, 10, 10, 10),

      # Axes
      axis.title = element_text(size = axis_title_size, face = "bold"),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.5),
      axis.text = element_text(size = axis_text_size),

      # Panel & grid
      panel.grid.major = if (grid_major) element_line(color = grid_color, linewidth = 0.3) else element_blank(),
      panel.grid.minor = if (grid_minor) element_line(color = grid_color, linewidth = 0.15) else element_blank(),
      panel.border = if (panel_border) element_rect(fill = NA, colour = "grey80") else element_blank(),

      # Legend
      legend.background = element_rect(fill = NA, colour = NA),
      legend.key = element_rect(fill = NA, colour = NA),
      legend.text = element_text(size = legend_size),
      legend.title = element_blank(),
      legend.position = legend_position,

      # Facets
      strip.background = element_rect(fill = "grey95", colour = NA),
      strip.text = element_text(size = strip_text_size, face = "bold"),

      # Lines / points default tweak
      axis.ticks = element_blank(),
      panel.spacing = unit(0.5, "lines")
    )

  # Return theme
  th
}

# Optional: a dark variant
theme_custom_dark <- function(..., panel_fill = "#000000", plot_background = "#000000", grid_color = "#2b2b2b") {
  theme_custom(..., panel_fill = panel_fill, plot_background = plot_background, grid_color = grid_color) +
    theme(
      text = element_text(colour = "white"),
      axis.text = element_text(colour = "white"),
      axis.title = element_text(colour = "white"),
      panel.grid.major = element_line(color = grid_color),
      panel.grid.minor = element_line(color = grid_color),
      panel.background = element_rect(fill = panel_fill, color = panel_fill),
      plot.background  = element_rect(fill = plot_background, color = plot_background)
    )
}

# is_shinyapps <- function() {
#   nzchar(Sys.getenv("SHINY_PORT"))
# }
#
# if (is_shinyapps()) {
#   chat_ellmer <- ellmer::chat_groq(
#     model = "llama-3.1-8b-instant",
#     api_key = Sys.getenv("GROQ_API_KEY_LGBTQ"),
#     seed = 123,
#     api_args = list(temperature = 0.8)
#   )
# } else {
#   chat_ellmer <- ellmer::chat_ollama(
#     model = "llama3.2",
#     seed = 123,
#     api_args = list(temperature = 0.8)
#   )
# }

chat_ellmer <- ellmer::chat_ollama(
  model = "llama3.2",
  seed = 123,
  api_args = list(temperature = 0.8))

# qc2 <-
#   querychat::QueryChat$new(
#     movies_red,
#     client = chat_ellmer)
#
# greeting <- qc2$generate_greeting("none")
# writeLines(greeting, "movies_greeting.md")

instructions <- "
- Use British spelling conventions (e.g., 'colour', 'favourite', 'analyse')
- When you don't understand the user's command or cannot generate a valid query, clearly state this and suggest an alternative phrasing they could use. For example: 'I didnt understand that request. Try asking: Show me movies from 2020 or Filter by genre Drama'
- Politely refuse to answer questions unrelated to the LGBTQ+ movies database. For example: I can only help with queries about LGBTQ+ films in this database. Please ask about movies, genres, release dates, or ratings.
- Only generate queries that will return valid dataframes based on the available columns.
- When searching for languages use the extensive version, e.g., English, Spanish"

qc <-
  querychat::QueryChat$new(
    LGBTQMovies::movies_red,
    "queer_movies",
    client = chat_ellmer,
    data_description = "./data_description.md",
    greeting = "./movies_greeting.md",
    extra_instructions = "instructions",
    cleanup = TRUE
    )
