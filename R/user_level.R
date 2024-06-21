#' Steam levels
#' @description
#' Get steam levels of users, their percentiles and distributions.
#'
#' \code{get_steam_level} retrieves the level of a user.
#'
#' \code{steam_level_percentile} retrieves the percentile of a user level
#' compared to all other user levels.
#'
#' \code{steam_level_distribution} retrieves the user level percentile for
#' a number of levels and shows their distribution.
#'
#' @inheritParams common
#' @param level User level to retrieve.
#' @param levels A vector of user levels to show the distribution of.
#' @returns \code{get_steam_level} and \code{get_level_percentile} return
#' a length-1 numeric vector. \code{steam_level_distribution} returns a numeric
#' vector of length \code{length(levels)}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' steamid <- "76561197984981409"
#'
#' # get level of a user
#' get_steam_level("76561197984981409")
#'
#' # get percentile of a user
#' get_level_percentile("76561197984981409")
#'
#' # plot distribution
#' dist <- steam_level_distribution(1:20)
#' plot(dist)
#' }
get_steam_level <- function(steamid) {
  check_string(steamid)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetSteamLevel",
    version = "v1",
    params = params
  )$response$player_level
}


#' @rdname get_steam_level
#' @export
steam_level_distribution <- function(levels = 1:100) {
  nvapply(levels, get_level_percentile, use_names = FALSE)
}


#' @rdname get_steam_level
#' @export
get_level_percentile <- function(level) {
  params <- .make_params(player_level = level)
  request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetSteamLevelDistribution",
    version = "v1",
    params = params
  )$response$player_level_percentile
}
