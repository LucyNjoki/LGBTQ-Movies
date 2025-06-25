movies_data = import(here("Data", "movieData.csv"))

# Language
table(movies_data$original_language)

movies_data_clean <- movies_data |> 
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
  ))


# Clean genre:
# Following this link: https://www.themoviedb.org/talk/5daf6eb0ae36680011d7e6ee
table(movies_data_clean$genre_ids)

movies_data_clean <- movies_data_clean |> 
  mutate(
    genre_ids = gsub("\\[|\\]", "", genre_ids),  # Remove square brackets
    genre_ids = strsplit(genre_ids, ",\\s*")     # Split by comma and optional space
  ) |> 
  unnest(genre_ids)  |> 
  mutate(genre_ids = as.factor(genre_ids)) |> 
  mutate(genre_ids_recoded = fct_recode(
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

export(movies_data_clean, here("Data", "movieDataCleaned.xlsx"))
