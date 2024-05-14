#' Get User ID
#' @description
#' Retrieves the Steam ID64 of a user profile, group or game hub based on
#' vanity IDs.
#'
#' @param name Name / Vanity ID of a user account, group or game hub.
#' See details and examples.
#' @param type Type of Steam name. \code{profile} returns the Steam ID of
#' a user profile, \code{group} returns the ID of a public group and
#' \code{game_group} returns the ID of a game's official game hub.
#'
#' @returns A length-1 character vector containing the Steam ID corresponding
#' to the input name.
#'
#' @details
#' There are various way of retrieving vanity IDs depending on the type of
#' vanity URL type. The vanity URL of a Steam user is the account name
#' (not the display name). It can be retrieved by inspecting the profile URL:
#' \preformatted{https://steamcommunity.com/id/{vanity_id}/}.
#'
#' Vanity IDs of groups can be retrieved in a similar way:
#' \preformatted{https://steamcommunity.com/groups/{vanity_id}/}.
#'
#' Vanity IDs of game hubs are not easily locatable as game hubs are
#' closely linked to store pages. They can be found by inspecting the
#' source code of a game page and searching for \code{VANITY_ID}. Vanity IDs
#' of game hubs are usually the application ID or an abbreviation of the
#' original title, e.g. \code{dota2} for DOTA 2, \code{TF2} for Team Fortress 2
#' or simply \code{70} for Half-Life
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # get user ID
#' get_steam_id("gabelogannewell")
#'
#' # get group ID
#' get_steam_id("SteamDB", type = "group")
#'
#' # get game hub ID
#' get_steam_id("TF2", type = "game_group")
#' }
get_steam_id <- function(name, type = "profile") {
  check_string(name)
  check_string(type)

  type <- switch(type, profile = 1, group = 2, game_group = 3)
  params <- .make_params(vanityurl = name, url_type = type)
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUser",
    method = "ResolveVanityURL",
    version = "v1",
    params = params
  )

  code <- res$response$success
  if (!identical(code, 1L)) {
    msg <- res$response$message
    stop(sprintf("Could not resolve vanity URL. Error code %s: %s", code, msg))
  }

  res$response$steamid
}


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
