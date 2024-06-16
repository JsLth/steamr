#' App list
#' @description
#' Get a list of (all) applications in the Steam store.
#'
#' \code{get_app_list} fetches all appIDs in the Steam store along with
#' their names.
#'
#' \code{get_games_by_ccu} fetches a list of the most played games by
#' concurrent users (CCU).
#'
#' \code{get_most_played_games} fetches a list of the most played games of
#' all time.
#'
#' \code{get_top_releases} fetches a list of the most played new releases
#' in the Steam store.
#'
#' @inheritParams get_items
#' @export
#' @rdname get_app_list
#'
#' @seealso
#'
#' \code{\link{steamspy}} for a similar approach by the SteamSpy API
#'
#' @examples
#' \dontrun{
#' get_all_list()
#'
#' # get most played games
#' get_games_by_ccu()
#'
#' # get most popular games of all time
#' get_most_played_games()
#'
#' # get the best releases
#' get_top_releases()
#' }
get_app_list <- function() {
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamApps",
    method = "GetAppList",
    version = "v2"
  )

  as_data_frame(res$applist$apps)
}


#' @rdname get_app_list
#' @export
get_games_by_ccu <- function(language = "english",
                             elanguage = NULL,
                             country_code = "US",
                             steam_realm = 1L,
                             include = NULL,
                             apply_user_filters = FALSE) {
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamChartsService",
    method = "GetGamesByConcurrentPlayers"
  )$response$ranks
  as_data_frame(res)
}


#' @rdname get_app_list
#' @export
get_most_played_games <- function() {
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamChartsService",
    method = "GetMostPlayedGames"
  )$response$ranks
  as_data_frame(res)
}


#' @rdname get_app_list
#' @export
get_top_releases <- function() {
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamChartsService",
    method = "GetTopReleasesPages",
    version = "v1"
  )$response$pages
  res$item_ids <- list(unlist(res$item_ids, use.names = FALSE))
  res$start_of_month <- as.POSIXct(res$start_of_month)
  as_data_frame(res)
}
