#' App list
#' @description
#' Get a list of applications in the Steam store, either a complete list
#' or the top games by concurrent players, all-time players, or top releases.
#'
#' @inheritParams common
#' @returns \describe{
#'  \item{\code{get_app_list}}{A dataframe containing the appID and name.}
#'
#'  \item{\code{get_games_by_ccu}}{A dataframe containing the appID, the
#'  rank by CCU and the total number and all-time record of concurrent players.}
#'
#'  \item{\code{get_most_played_game}}{A dataframe containing the appID, the
#'  rank by total all-time players and players last week as well as the
#'  all-time record of concurrent players.}
#'
#'  \item{get_top_releases}{A dataframe with three rows containing the top
#'  releases of the last three months. Each row contains the start of the
#'  month, the url path for the top releases and a vector top release appIDs.
#'  The URL path can be appended to the following URL:
#'
#'  \code{https://store.steampowered.com/charts/topnewreleases/}}
#' }
#'
#' @export
#'
#'
#' @seealso
#'
#' \code{\link{steamspy}} for a similar approach by the SteamSpy API
#'
#' @examples
#' get_app_list()
#'
#' # get most played games
#' get_games_by_ccu()
#'
#' # get most popular games of all time
#' get_most_played_games()
#'
#' # get the best releases
#' get_top_releases()
get_app_list <- function() {
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamApps",
    method = "GetAppList",
    version = "v2",
    cache = TRUE
  )$applist$apps

  res <- res[order(res$appid), ]
  as_data_frame(res)
}


#' @rdname get_app_list
#' @export
get_games_by_ccu <- function(language = "english",
                             elanguage = NULL,
                             country_code = "US",
                             steam_realm = 1L,
                             include = NULL,
                             apply_user_filters = FALSE) {
  check_string(language)
  check_integerish(elanguage)
  check_string(country_code)
  check_integerish(steam_realm)
  check_string(include, null = TRUE)
  check_bool(apply_user_filters)

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamChartsService",
    method = "GetGamesByConcurrentPlayers",
    params = params
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



#' @rdname get_app_list
#' @export
get_apps_in_genre <- function(genre, language = "english", country_code = "US") {
  check_string(genre)
  check_string(language)
  check_string(country_code)

  params <- .make_params(key = FALSE)
  res <- request_storefront(
    api = store_api(),
    interface = "api",
    method = "getappsingenre",
    params = params
  )$tabs
  res <- bind_rows(lapply(res, "[[", "items"), .id = "tab")
  as_data_frame(res)
}


#' @rdname get_app_list
#' @export
get_apps_in_category <- function(category,
                                 language = "english",
                                 country_code = "US") {
  check_string(category)
  check_string(language)
  check_string(country_code)

  params <- .make_params(
    category = category,
    cc = country_code,
    l = language,
    key = FALSE
  )
  res <- request_storefront(
    api = store_api(),
    interface = "api",
    method = "getappsincategory",
    params = params
  )

  if (!identical(res$status, 1L)) {
    stop(sprintf("Request failed with status code %s", res$status))
  }

  res <- bind_rows(lapply(res$tabs, "[[", "items"), .id = "tab")
  as_data_frame(res)
}
