#' Achievements
#' @description
#' Retrieve game achievements, either globally or for a specific user.
#'
#' @param appid appID of an application in the Steam store.
#' @inheritParams common
#'
#' @returns \describe{
#'  \item{\code{get_game_achievements}}{A dataframe containing information
#'  about the achievements unlockable in a game. \code{internal_name}
#'  provides a machine-readable name for each achievement.
#'  \code{localized_name} and \code{localized_desc} provide human-readable
#'  and localized variants of achievement name and description. Both
#'  \code{icon} and \code{icon_gray} are files that can be accessed by
#'  appending it to the following URL:
#'
#'  \code{https://cdn.akamai.steamstatic.com/steamcommunity/public/images/apps/{appid}/}
#'
#'  \code{hidden} reports on whether an achievement is hidden to the player.
#'  \code{player_percentage_unlocked} is a global statistic on how many
#'  total players have unlocked the achievement.
#'  }
#'
#'  \item{\code{get_top_achievements}}{A dataframe in long format containing
#'  the top \code{max_achievements} game achievements for a player and for
#'  each game in \code{appids}. The output columns largely correspond to the
#'  output of \code{get_game_achievements}.}
#'
#'  \item{\code{get_player_achievements}}{A dataframe containing all
#'  achievements of a user in a game. \code{apiname}, \code{name}, and
#'  \code{description} correspond to machine-readable and human-readable
#'  achievement names. \code{achieved} reports whether an achievement has
#'  been unlocked by the user. \code{unlocktime} reports on the time of doing
#'  so. If access to the user achievements is denied, an empty dataframe is
#'  returned.}
#' }
#'
#' @details
#' \code{get_top_achievements} and \code{get_player_achievements} require
#' access to the user's game achievements. \code{get_top_achievements} returns
#' an error code 15: \code{AccessDenied}, while \code{get_player_achievements}
#' simply returns an empty dataframe.
#'
#' @export
get_game_achievements <- function(appid, language = "english") {
  check_number(appid)
  check_string(language)

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetGameAchievements",
    version = "v1",
    params = params
  )$response$achievements
  as_data_frame(res)
}


#' @rdname get_game_achievements
#' @export
#' @param max_achievements Maximum number of achievements to return for each
#' game. Defaults to the top 5 achievements.
get_top_achievements <- function(steamid,
                                 appids,
                                 max_achievements = 5L,
                                 language = "english") {
  check_string(steamid)
  check_integerish(max_achievements)
  check_string(language)
  check_number(appids)
  steamid <- convert_steamid(steamid, to = "steam64")
  appids <- box(appids)

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetTopAchievementsForGames",
    version = "v1",
    params = params
  )$response$games
  total <- res$total_achievements
  res <- res$achievements
  names(res) <- appids
  res <- bind_rows(res, .id = "appid")
  attr(res, "total") <- total
  res
}


#' @rdname get_game_achievements
#' @export
get_player_achievements <- function(steamid, appid, language = "english") {
  check_string(steamid)
  check_number(appid)
  check_string(language)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params(steamid = steamid, appid = appid, l = language)
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUserStats",
    method = "GetPlayerAchievements",
    version = "v1",
    params = params
  )$playerstats$achievements
  as_data_frame(res)
}
