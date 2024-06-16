#' SteamSpy
#' @description
#' Queries the SteamSpy API. SteamSpy is an unofficial API that provides
#' endpoints for app lists and app details. \code{steamspy} automatically
#' iterates through pagination with a rate limit of 1 request per minute. To
#' limit the number of pages returned, set \code{options(steamr_max_reqs)}.
#'
#' Due to rate restrictions, retrieving all games can take up to an hour of
#' time. For a much faster approach, see \code{\link{get_app_list}}, which
#' makes use of the official Steam Web API.
#'
#' @param genre If specified, returns games in this particular genre. Queries
#' the `genre` endpoint.
#' @param tag If specified, returns game with this particular tag. Queries
#' the `tag` endpoint.
#' @param top100 If \code{TRUE}, returns the top 100 most played games
#' since March 2009. Queries the `top100forever` endpoint. Ignored if
#' \code{genre} or \code{tag} are specified.
#' @param top100_2w If \code{TRUE}, returns the top 100 most played games
#' in the last two weeks. Queries the `top100in2weeks` endpoint. Ignored if
#' one of the previous three arguments are specified.
#' @param top100_owned If \code{TRUE} returns the top 100 games by owners.
#' Queries the `top100owned` endpoint. Ignored if one of the previous
#' four arguments is specified.
#'
#' @returns A dataframe with the following columns:
#'
#' \code{appid}: Steam application ID. If it is 999999, the data is hidden.
#'
#' \code{name}: Application name
#'
#' \code{developer}: Comma-seperated list of developers
#'
#' \code{publisher}: Comma-seperated list of publishers
#'
#' \code{score_rank}: Score based on user reviews
#'
#' \code{owners}: Estimated number of owners
#'
#' \code{average_forever}: Average playtime since March 2009 in minutes
#'
#' \code{average_2weeks}: Average playtime in the last two weeks in minutes
#'
#' \code{median_forever}: Median playtime since March 2009 in minutes
#'
#' \code{median_2weeks}: Median playtime in the last two weeks in minutes
#'
#' \code{ccu}: Peak currently connected users (CCU) yesterday
#'
#' \code{price}: Current US price in cents
#'
#' \code{initialprice}: Original US price in cents
#'
#' \code{discount}: Current discount in percent
#'
#' \code{tags}: List of tags
#'
#' \code{language}: List of supported languages
#'
#' \code{genre}: List of genres
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # get all games
#' steamspy()
#'
#' # restrict query to the first 1,000 games
#' options(steamr_max_reqs = 1)
#' steamspy()
#'
#' # get all early access titles
#' steamspy(tag = "Early Access")
#'
#' # get top 100 games of all time
#' steamspy(top100 = TRUE)
#' }
steamspy <- function(genre = NULL,
                     tag = NULL,
                     top100 = FALSE,
                     top100_2w = FALSE,
                     top100_owned = FALSE) {
  check_string(genre, null = TRUE)
  check_length(genre, ge = 0, le = 1)
  check_string(tag, null = TRUE)
  check_length(tag, ge = 0, le = 1)
  check_bool(top100)
  check_bool(top100_2w)
  check_bool(top100_owned)

  if (!is.null(genre)) {
    params <- list(request = "genre", genre = genre)
  } else if (!is.null(tag)) {
    params <- list(request = "tag", tag = tag)
  } else if (isTRUE(top100)) {
    params <- list(request = "top100forever")
  } else if (isTRUE(top100_2w)) {
    params <- list(request = "top100in2weeks")
  } else if (isTRUE(top100_owned)) {
    params <- list(request = "top100owned")
  } else {
    params <- list(request = "all")
  }

  res <- request_steamspy(params)
  as_data_frame(res)
}
