get_game_schema <- function(appid, language = "english") {
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUserStats",
    method = "GetSchemaForGame",
    version = "v2",
    params = params
  )
  game <- res$game$gameName
  version <- res$game$gameVersion

  stats <- res$game$availableGameStats$stats
  achievements <- res$game$availableGameStats$achievements
  res <- bind_rows(stats = stats, achievements = achievements, .id = "type")
  attr(res, "game") <- game
  attr(res, "version") <- version
  res
}


get_game_stats <- function(appid, count = NULL, name = NULL, startdate = NULL, enddate = NULL) {
  name <- box(name)
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUserStats",
    method = "GetGlobalStatsForGame",
    version = "v1",
    params = params
  )
  res
}


