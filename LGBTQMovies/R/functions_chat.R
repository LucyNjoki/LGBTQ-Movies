# conditional local or online
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

# create chat with local model
chat_ellmer <- ellmer::chat_ollama(
  model = "qwen2.5", #"llama3.2",
  seed = 123,
  api_args = list(temperature = 0.8))

# create greeting once
# qc2 <-
#   querychat::QueryChat$new(
#     movies_red,
#     client = chat_ellmer)
#
# greeting <- qc2$generate_greeting("none")
# writeLines(greeting, "movies_greeting.md")

# create instructions - to be improved
instructions <- "
- Use British spelling conventions (e.g., 'colour', 'favourite', 'analyse').

- If you do not understand the user's request or cannot generate a valid query, clearly state this and suggest an alternative phrasing. For example: 'I did not understand that request. Try asking: Show me movies from 2020 or Filter by genre Drama.'

- When the user asks for the number of votes for a specific movie, filter by that movie and retrieve the value from the vote_count variable.

- Politely refuse to answer questions unrelated to the LGBTQ+ movies database. For example: 'I can only help with queries about LGBTQ+ films in this database. Please ask about movies, genres, release dates, or ratings.'

- Only generate queries that return valid data frames based on the available columns.

- When filtering by language, use the full language name (e.g., English, Spanish), not abbreviations.

- When searching for a movie by name, check both the title and original_title columns."

# create chat
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
