#' Game stats
#' @description
#' Retrieve game stats and achievements for a game. These functions generally
#' operate more on stats than on achievements. For functions tailored to
#' achievements, see \code{\link{get_game_achievements}}.
#'
#' \code{get_game_schema} returns all defined stats and achievements for an
#' appID.
#'
#' \code{get_game_stats} returns aggregated global stats for an appID. Stats
#' need to be explicitly marked as aggregated and not all stats can be
#' aggregated to a global stat
#' (see \href{https://partner.steamgames.com/doc/features/achievements#global_stats}{here}
#' for details). Finding out which stats are global involves quite a lot of
#' trial-and-error.
#'
#' @returns
#' \describe{
#'  \item{\code{get_game_schema}}{A dataframe containing data about game
#'  stats and achievements. Stats and achievements contain data about
#'  API names (\code{name}) and display names \code{displayName}.
#'  \code{defaultvalue} is always 0 as the default value of a stat is
#'  unachieved. Additionally, achievements also have data on achievement
#'  visibility (\code{hidden}), and icon URLs (\code{icon} and \code{icongray}).
#'  The \code{type} column specifies whether the row is about a stat or
#'  achievement.}
#'
#'  \item{\code{get_game_stats}}{A dataframe containing two columns
#'  \code{stat} and \code{value} representing the stat name and the
#'  corresponding global value of the stat.}
#'
#'  \item{\code{get_user_stats_for_game}}{A dataframe containing the
#'  name of the stat/achievement (\code{name}), its value (\code{value}) and
#'  whether it is a stat or an achievement (\code{type}).}
#' }
#'
#' @inheritParams get_game_achievements
#' @param language Language for descriptions and display names.
#'
#' @references
#' \url{https://wiki.teamfortress.com/wiki/WebAPI/GetGlobalStatsForGame}
#'
#' \url{https://partner.steamgames.com/doc/features/achievements#global_stats}
#'
#' @examples
#' \dontrun{
#' # get schema for all achievements and stats in the game Payday 2
#' get_game_schema(218620)
#'
#' # retrieve the number of players with more than 1000h and a level of high
#' # than 100 in Payday 2
#' get_game_stats(218620, c("player_time_1000h", "player_level_100"))
#'
#' # get the stats for a specific user in CS:GO
#' get_user_stats_for_game("76561197984981409", 730)
#' }
#'
get_game_schema <- function(appid, language = "english") {
  check_number(appid)
  check_string(language)

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUserStats",
    method = "GetSchemaForGame",
    version = "v2",
    params = params
  )

  if (!length(res$game)) {
    return(list())
  }

  game <- res$game$gameName
  version <- res$game$gameVersion

  stats <- res$game$availableGameStats$stats
  achievements <- res$game$availableGameStats$achievements
  res <- bind_rows(stats = stats, achievements = achievements, .id = "type")
  attr(res, "game") <- game
  attr(res, "version") <- version
  res
}


#' @rdname get_game_schema
#' @export
#' @param name A vector of stat names to return data for. The stat needs to
#' be aggregated in order to allow for making global stats.
#' @param startdate Start date for daily totals.
#' @param enddate End date for daily totals.
get_game_stats <- function(appid, name, startdate = NULL, enddate = NULL) {
  check_number(appid)
  check_string(name)
  check_date(startdate, null = TRUE)
  check_date(enddate, null = TRUE)

  count <- length(name)
  name <- box(name)
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUserStats",
    method = "GetGlobalStatsForGame",
    version = "v1",
    params = params
  )$response$globalstats
  pivot_longer_list(res, names_to = "stat")
}


#' @rdname get_game_schema
#' @export
#' @inheritParams get_player_summary
get_user_stats_for_game <- function(steamid, appid, language = "english") {
  check_string(steamid)
  check_string(language)
  steamid <- convert_steamid(steamid, to = "steam64")

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
