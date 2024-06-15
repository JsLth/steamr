get_app_list <- function() {
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamApps",
    method = "GetAppList",
    version = "v2",
    params = params
  )

  as_data_frame(res$applist$apps)
}


get_games_by_players <- function() {

}


get_most_played_games <- function(steam_deck = FALSE) {

}


get_top_releases <- function() {

}
