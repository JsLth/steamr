#' Achievements
#' @description
#' Retrieve achievements for a game.
#'
#' @param appid appID of an application in the Steam store.
#' @inheritParams get_items
#'
#' @returns A dataframe containing information about a game's achievements.
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


get_top_achievements <- function(steamid,
                                 appids,
                                 max_achievements = 8L,
                                 language = "english") {
  check_string(steamid)
  check_integerish(max_achievements)
  check_string(language)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetTopAchievementsForGames",
    version = "v1",
    params = params
  )$response
}


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


get_achievement_percentages <- function(appid) {
  check_number(appid)

  params <- .make_params(gameid = appid)
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUserStats",
    method = "GetGlobalAchievementPercentagesForApp",
    version = "v1",
    params = params
  )$achievementpercentages$achievements$achievement
  as_data_frame(res)
}
