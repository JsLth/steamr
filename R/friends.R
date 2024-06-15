get_friend_list <- function(steamid, relationship = "all") {
  check_string(steamid)
  check_string(relationship)
  convert_steamid(steamid, to = "steam64")

  params <- .make_params(access_token = FALSE)
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUser",
    method = "GetFriendList",
    version = "v1",
    params = params
  )

  as_data_frame(res$friendslist$friends)
}


get_friends_playtimes <- function(appid) {
  check_authenticated()
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetFriendsGameplayInfo",
    version = "v1",
    params = params
  )$response
  as_data_frame(rbind(res$played_ever, res$owns))
}


get_friends_recommendations <- function(appid) {
  check_authenticated()
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IUserReviewsService",
    method = "GetFriendsRecommendedApp",
    version = "v1",
    params = params
  )$response
}
