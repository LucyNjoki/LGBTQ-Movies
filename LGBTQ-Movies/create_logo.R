# Load required libraries
library(ggplot2)
library(dplyr)
library(png)
library(grid)
library(hexSticker)
library(here)
library(magick)

# Create full circular pride flag

# Load and resize the pride flag
flag_path <- here("LGBTQ-Movies", "Logo", "progress-pride-intersex-flag.png")
flag_img <- image_read(flag_path) %>%
  image_resize("800x800")  # Larger flag

# Create a circular alpha mask using fx expression
mask <- image_blank(800, 800, color = "white") %>%
  image_fx(expression = "1 - (pow(i-400,2) + pow(j-400,2) > pow(400,2))")

# Apply the circular mask to the flag
flag_circle <- image_composite(flag_img, mask, operator = "copyopacity")

# Add glow effect (white outer blur)
glow <- image_blur(flag_circle, radius = 10, sigma = 20)
flag_glow <- image_composite(glow, flag_circle, operator = "Over")

# Save the circular flag with glow
flag_circle_path <- here("LGBTQ-Movies", "Logo", "flag_circle_glow.png")
image_write(flag_glow, path = flag_circle_path, format = "png")

# Load circular flag and film icon as grobs
flag_grob <- rasterGrob(readPNG(flag_circle_path), interpolate = TRUE)
movie_icon <- readPNG(here("LGBTQ-Movies", "Logo", "film.png"))
movie_grob <- rasterGrob(movie_icon, interpolate = TRUE)

# Create larger hexagon ring

# Function to create hexagon coordinates
hexagon <- function(center_x, center_y, size = 0.22) {
  angle <- seq(0, 2 * pi, length.out = 7)
  data.frame(
    x = center_x + size * cos(angle),
    y = center_y + size * sin(angle)
  )
}

# Arrange hexagons in a wider ring around the flag
hex_centers <- data.frame(
  x = 0.5 + 0.75 * cos(seq(0, 2 * pi, length.out = 7)[-7]),
  y = 0.5 + 0.75 * sin(seq(0, 2 * pi, length.out = 7)[-7]),
  color = c("#FF0018", "#FFA52C", "#FFFF41", "#008018", "#0000F9", "#86007D")
)

hex_shapes <- bind_rows(lapply(1:nrow(hex_centers), function(i) {
  hexagon(hex_centers$x[i], hex_centers$y[i]) %>%
    mutate(group = i, fill = hex_centers$color[i])
}))

# STEP 3: Compose the plot

base_plot <- ggplot() +
  annotation_custom(flag_grob, xmin = 0.1, xmax = 0.9, ymin = 0.1, ymax = 0.9) +  # Larger flag
  annotation_custom(movie_grob, xmin = 0.4, xmax = 0.6, ymin = 0.4, ymax = 0.6) +  # Film icon
  geom_polygon(data = hex_shapes, aes(x = x, y = y, group = group, fill = fill), 
               color = "white", linewidth = 1) +
  scale_fill_identity() +
  theme_void() +
  coord_fixed()

# Export as hex sticker

sticker(
  subplot = base_plot,
  package = "LGBTQ+ Movies",
  p_size = 30,                    # Larger text
  p_color = "#FFB90F",            # Gold text
  s_x = 1, s_y = 0.8,
  s_width = 1.1,
  h_fill = "#1C1C1C",           # Transparent hex background
  h_color = "#FFB90F",            # Gold border
  spotlight = FALSE,
  dpi = 600,
  filename = here("LGBTQ-Movies", "Logo", "lgbtq_movies_hex_final.png")
)
