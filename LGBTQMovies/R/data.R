#' @title LGBTQ+ Movies Dataset
#' @description A dataset containing metadata about 7,165 LGBTQ+ themed movies, curated by github.com/cacalc.
#' @format A data frame with 7,165 rows and 12 variables:
#' \describe{
#'   \item{id}{Movie ID (Identifier from the database TMDB).}
#'   \item{title}{Title of the movie in English or the language of release.}
#'   \item{original_title}{Original title of the movie, in its original language.}
#'   \item{original_language}{Language in which the movie was originally produced.}
#'   \item{overview}{Short synopsis or description of the movie.}
#'   \item{release_date}{Official release date of the movie (format: YYYY-MM-DD).}
#'   \item{popularity}{Popularity score.}
#'   \item{vote_average}{Average rating of the movie given by users.}
#'   \item{vote_count}{Total number of user votes received.}
#'   \item{adult}{Indicates whether the movie is marked as adult content (TRUE or FALSE).}
#'   \item{video}{Indicates whether the item is a video (TRUE or FALSE).}
#'   \item{genre_ids}{List of genre identifiers associated with the movie (e.g., [35, 10749]).}
#' }
#' @source Generated for this Shiny app.
"movies_df"

#'@title Color list
#'@description A list of colors used in the app.
#'@format A list with 3 elements:
#' \describe{
#'  \item{color1}{Color for gender breakdown.}
#'  \item{color2}{Color for the language insights.}
#'  \item{color3}{Color for trend analysis.}
#'  }
#' @source Generated for this Shiny app.
"color"
