
# Colors:

color <- rep("#f4b51b", 7)

usethis::use_data(color, overwrite = TRUE)

utils::globalVariables("color")


## code to prepare `DATASET` dataset goes here
library(dplyr)
setwd(here::here("../LGBTQMovies"))
# write here the code to clean and trasnform df
movies_df <- utils::read.csv('data-raw/movieData.csv') |>
  mutate(
    id = as.character(id),
    original_language = as.factor(original_language),
    release_date = lubridate::ymd(release_date),
    adult = as.logical(adult),
    video = as.logical(video)
    # genre_ids = stringr::str_extract_all(genre_ids, "\\d+") |>
      # purrr::map(~ trimws(.))
  ) |>
  mutate(original_language = as.factor(original_language)) |>
  mutate(original_language_recoded = fct_recode(
    original_language,
    "Afrikaans" = "af",
    "Amharic" = "am",
    "Arabic" = "ar",
    "Azerbaijani" = "az",
    "Belarusian" = "be",
    "Bulgarian" = "bg",
    "Bengali" = "bn",
    "Bosnian" = "bs",
    "Catalan" = "ca",
    "Chinese" = "cn",
    "Czech" = "cs",
    "Danish" = "da",
    "German" = "de",
    "Greek" = "el",
    "English" = "en",
    "Spanish" = "es",
    "Persian" = "fa",
    "Finnish" = "fi",
    "French" = "fr",
    "Irish" = "ga",
    "Galician" = "gl",
    "Hebrew" = "he",
    "Hindi" = "hi",
    "Croatian" = "hr",
    "Hungarian" = "hu",
    "Indonesian" = "id",
    "Icelandic" = "is",
    "Italian" = "it",
    "Japanese" = "ja",
    "Georgian" = "ka",
    "Khmer" = "km",
    "Korean" = "ko",
    "Kurdish" = "ku",
    "Latin" = "la",
    "Lao" = "lo",
    "Lithuanian" = "lt",
    "Macedonian" = "mk",
    "Malayalam" = "ml",
    "Mongolian" = "mn",
    "Burmese" = "my",
    "Nepali" = "ne",
    "Dutch" = "nl",
    "Norwegian" = "no",
    "Punjabi" = "pa",
    "Polish" = "pl",
    "Portuguese" = "pt",
    "Romanian" = "ro",
    "Russian" = "ru",
    "Slovak" = "sk",
    "Slovenian" = "sl",
    "Albanian" = "sq",
    "Serbian" = "sr",
    "Swedish" = "sv",
    "Swahili" = "sw",
    "Tamil" = "ta",
    "Telugu" = "te",
    "Thai" = "th",
    "Tagalog" = "tl",
    "Turkish" = "tr",
    "Ukrainian" = "uk",
    "Urdu" = "ur",
    "Vietnamese" = "vi",
    "Xhosa" = "xh",
    "No Language" = "xx", # TMDB sometimes uses 'xx' for no dialogue
    "Chinese" = "zh"      # Mandarin (zh) grouped under Chinese
  )) |>
  mutate(
    genre_ids = gsub("\\[|\\]", "", genre_ids),  # Remove square brackets
    genre_ids = strsplit(genre_ids, ",\\s*")     # Split by comma and optional space
  ) |>
  tidyr::unnest(genre_ids)  |>
  mutate(genre_ids = as.factor(genre_ids)) |>
  mutate(genre_ids_recoded = forcats::fct_recode(
    genre_ids,
    "Action" = "28",
    "Adventure" = "12",
    "Animation" = "16",
    "Comedy" = "35",
    "Crime" = "80",
    "Documentary" = "99",
    "Drama" = "18",
    "Family" = "10751",
    "Fantasy" = "14",
    "History" = "36",
    "Horror" = "27",
    "Music" = "10402",
    "Mystery" = "9648",
    "Romance" = "10749",
    "Science Fiction" = "878",
    "TV Movie" = "10770",
    "Thriller" = "53",
    "War" = "10752",
    "Western" = "37"
  ))


dplyr::glimpse(movies_df)
skimr::skim(movies_df)

# save processed data into package
usethis::use_data(movies_df, overwrite = TRUE)

# run it on time for each data/global values
utils::globalVariables("movies_df") # to suppress an error that appears when checking the package



