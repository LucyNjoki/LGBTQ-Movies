
# Colors:

color <- list("steelblue", "darkgreen", "purple")

utils::globalVariables("color")


## code to prepare `DATASET` dataset goes here
library(dplyr)
setwd(here::here("../LGBTQMovies"))
# write here the code to clean and trasnform df
movies_df <- utils::read.csv('data-raw/movieData.csv') |>
  mutate(
    id = as.character(id),
    original_language = as.factor(original_language),
    release_date = as.Date(release_date),
    adult = as.logical(adult),
    video = as.logical(video),
    genre_ids = stringr::str_extract_all(genre_ids, "\\d+") |>
      purrr::map(~ trimws(.))
  )

dplyr::glimpse(movies_df)
skimr::skim(movies_df)

# save processed data into package
usethis::use_data(movies_df, overwrite = TRUE)

# run it on time for each data/global values
utils::globalVariables("movies_df") # to suppress an error that appears when checking the package



