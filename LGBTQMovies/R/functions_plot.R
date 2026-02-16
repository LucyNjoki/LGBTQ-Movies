
# Add Google font
sysfonts::font_add_google("Inter", "inter")
showtext::showtext_auto()

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
  th <- ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Plot-level
      plot.background = ggplot2::element_rect(fill = plot_background, colour = NA),
      panel.background = ggplot2::element_rect(fill = panel_fill, colour = NA),
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.15), face = plot_title_face, margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.95), face = subtitle_face, margin = ggplot2::margin(b = 6)),
      plot.caption = ggplot2::element_text(size = ggplot2::rel(0.75), face = caption_face, hjust = 1, margin = ggplot2::margin(t = 6)),
      plot.margin = ggplot2::margin(10, 10, 10, 10),

      # Axes
      axis.title = ggplot2::element_text(size = axis_title_size, face = "bold"),
      axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
      axis.title.x = ggplot2::element_text(vjust = -0.5),
      axis.text = ggplot2::element_text(size = axis_text_size),

      # Panel & grid
      panel.grid.major = if (grid_major) ggplot2::element_line(color = grid_color, linewidth = 0.3) else ggplot2::element_blank(),
      panel.grid.minor = if (grid_minor) ggplot2::element_line(color = grid_color, linewidth = 0.15) else ggplot2::element_blank(),
      panel.border = if (panel_border) ggplot2::element_rect(fill = NA, colour = "grey80") else ggplot2::element_blank(),

      # Legend
      legend.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.key = ggplot2::element_rect(fill = NA, colour = NA),
      legend.text = ggplot2::element_text(size = legend_size),
      legend.title = ggplot2::element_blank(),
      legend.position = legend_position,

      # Facets
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text = ggplot2::element_text(size = strip_text_size, face = "bold"),

      # Lines / points default tweak
      axis.ticks = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0.5, "lines")
    )

  # Return theme
  th
}

# Optional: a dark variant
theme_custom_dark <- function(..., panel_fill = "#000000", plot_background = "#000000", grid_color = "#2b2b2b") {
  theme_custom(..., panel_fill = panel_fill, plot_background = plot_background, grid_color = grid_color) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "white"),
      axis.text = ggplot2::element_text(colour = "white"),
      axis.title = ggplot2::element_text(colour = "white"),
      panel.grid.major = ggplot2::element_line(color = grid_color),
      panel.grid.minor = ggplot2::element_line(color = grid_color),
      panel.background = ggplot2::element_rect(fill = panel_fill, color = panel_fill),
      plot.background  = ggplot2::element_rect(fill = plot_background, color = plot_background)
    )
}
