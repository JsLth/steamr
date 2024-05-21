#' Get user info
#' @description
#' Functions to retrieve information on a given user including friends,
#' stats, achievements, groups.
#'
#' @param steamid Steam user ID as returned by \code{\link{get_steam_id}}.
#' @param relationship Type of relationship to the user. One of \code{all}
#' and \code{friend}.
#'
#' @returns \itemize{
#' \item{\code{get_friend_list} returns a dataframe on relationship status
#' and time.}
#' \item{\code{\code{get_user_stats_for_game} returns a list with names of the
#' type of statistic, its value, and the type of statistic (achievement / stat).}}
#' \item{\code{get_user_group} returns a character vector of group IDs.}
#' \item{\code{get_player_summary returns a dataframe with one row for each
#' user containing basic information on the user account.}}
#' \item{\code{get_player_bans} returns a dataframe containing information
#' on bans (e.g. VAC, community bans)}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # get steam ID
#' steamid <- get_steam_id("gabelogannewell")
#'
#' # get all friends
#' get_friend_list(steamid)
#'
#' # user stats for team fortress
#' get_user_stats_for_game
#' }
#'
get_player_summary <- function(steamids) {
  check_string(steamids)
  check_length(steamids, ge = 1, le = 100)

  steamids <- paste(steamids, collapse = ",")
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUser",
    method = "GetPlayerSummaries",
    version = "v2",
    params = params
  )

  as_data_frame(res$response$players)
}



get_friend_list <- function(steamid, relationship = "all") {
  check_string(steamid)
  check_string(relationship)

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUser",
    method = "GetFriendList",
    version = "v1",
    params = params
  )

  as_data_frame(res$friendslist$friends)
}


get_user_stats_for_game <- function(steamid, appid, language = "english") {
  check_string(steamid)
  check_string(language)

  params <- .make_params(steamid = steamid, appid = appid, l = language)
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUserStats",
    method = "GetUserStatsForGame",
    version = "v1",
    params = params
  )$playerstats

  stats <- pivot_longer_list(res$stats)
  achievements <- pivot_longer_list(res$achievements)
  res <- bind_rows(stats = stats, achievements = achievements, .id = "type")
  res
}


#' Get user groups
#' @description
#' Get a list of user group IDs (gid).
#'
#' @param steamid
#'
get_user_group_list <- function(steamid) {
  check_string(steamid)

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUser",
    method = "GetUserGroupList",
    version = "v1",
    params = params
  )
  res$response$groups$gid
}


get_player_bans <- function(steamids) {
  check_string(steamids)
  check_length(steamids, ge = 1, le = 100)

  steamids <- paste(steamids, collapse = ",")
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUser",
    method = "GetPlayerBans",
    version = "v1",
    params = params
  )
  as_data_frame(res$players)
}
