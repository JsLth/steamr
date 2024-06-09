get_userdata <- function() {
  check_authenticated()
  request_internal(
    api = store_api(),
    interface = "dynamicstore",
    method = "userdata"
  )
}


get_app_user_details <- function(appids) {
  check_authenticated()
  params <- .make_params(key = FALSE)
  request_internal(
    api = store_api(),
    interface = "api",
    method = "appuserdetails",
    params = params
  )
}


get_library_stats <- function() {
  check_authenticated()
  request_internal(
    api = store_api(),
    interface = "contenthub",
    method = "ajaxgetdlcstatsforuser"
  )
}


get_owned_apps <- function() {
  check_authenticated()
  cookiejar <- get("session", envir = globst)
  cookies <- read_cookies(cookiejar)
  sessionid <- cookies[cookies$name %in% "sessionid", ]
  sessionid <- cookies[cookies$domain %in% "steamcommunity.com", ]
  sessionid <- sessionid$value
  res <- request_internal(
    api = comm_api(),
    interface = "actions",
    method = "GetOwnedApps",
    params = list(sessionid = sessionid)
  )
  as_data_frame(res)
}


get_friends <- function() {
  check_authenticated()
  request_internal(
    api = comm_api(),
    interface = "actions",
    method = "ajaxlistfriends"
  )
}


get_friend_data <- function(steamid) {
  check_authenticated()
  params <- .make_params(u = steamid, key = FALSE)
  res <- request_internal(
    api = store_api(),
    interface = "friends",
    method = "frienddata"
  )$friends
  as_data_frame(res)
}


get_badge <- function(appid, badgeid) {
  check_authenticated()
  params <- .make_params(key = FALSE)
  res <- request_internal(
    api = comm_api(),
    interface = "actions",
    method = "ajaxgetbadgeinfo",
    params = params
  )

  res$success <- NULL
  res$completion_time <- as.POSIXct(res$completion_time)
  as_data_frame(res)
}


get_badge_info <- function(appid) {
  check_authenticated()
  params <- .make_params(key = FALSE)
  request_internal(
    api = comm_api(),
    interface = "my",
    method = "ajaxgetbadgeinfo",
    params = params,
    params_as_query = FALSE
  )$badgedata
}


get_store_relevance <- function(appid) {
  check_authenticated()
  params <- .make_params(key = FALSE)
  res <- request_internal(
    api = store_api(),
    interface = "explore",
    method = "ajaxgetstorerelevancedata",
    params = params
  )$results
  res$last_playtime <- as.POSIXct(res$last_playtime)
  res$last_playtime_after_gap <- as.POSIXct(res$last_playtime_after_gap)
  res$first_playtime <- as.POSIXct(res$first_playtime)
  res$first_windows_playtime <- as.POSIXct(res$first_windows_playtime)
  res$first_mac_playtime <- as.POSIXct(res$first_mac_playtime)
  res$first_linux_playtime <- as.POSIXct(res$first_linux_playtime)
  as_data_frame(res)
}


get_price_history <- function(appid, hash_name) {
  check_authenticated()
  params <- .make_params(key = FALSE)
  res <- request_internal(
    api = comm_api(),
    interface = "market",
    method = "pricehistory"
  )

  prefix <- res$price_prefix
  suffix <- res$price_suffix
  res <- res$prices
  attr(res, "prefix") <- prefix
  attr(res, "suffix") <- suffix
  as_data_frame(res)
}
