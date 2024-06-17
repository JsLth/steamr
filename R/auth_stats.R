#' User data
#' @description
#' Authenticated functions to fetch information about the authenticated user.
#'
#' @export
get_userdata <- function() {
  check_authenticated()
  res <- request_storefront(
    api = store_api(),
    interface = "dynamicstore",
    method = "userdata"
  )

  # format curations
  res$rgCurations <- lapply(
    res$rgCurations,
    pivot_longer_list,
    names_to = "clanid",
    values_to = "rating"
  )
  res$rgCurations <- pivot_longer_list(
    res$rgCurations,
    names_to = "appid"
  )

  ratings <- list(
    `0` = "recommended",
    `1` = "Not recommended",
    `2` = "Informational"
  )
  res$rgCurations$rating <- unlist(ratings[as.character(res$rgCurations$rating)])

  # format ignored apps
  res$rgIgnoredApps <- pivot_longer_list(
    res$rgIgnoredApps,
    names_to = "appid",
    values_to = "status"
  )
  status <- list(
    `0` = "Ignored",
    `1` = "Unknown",
    `2` = "Played on another platform"
  )
  res$rgIgnoredApps <- unlist(status[as.character(res$rgIgnoredApps)])
  res$rgCurators <- do.call(rbind.data.frame, unname(res$rgCurators))

  fields_as_data_frame(res)
}


#' @rdname get_userdata
#' @export
get_app_user_details <- function(appids) {
  check_authenticated()
  appids <- paste(appids, collapse = ",")
  params <- .make_params(key = FALSE)
  res <- request_storefront(
    api = store_api(),
    interface = "api",
    method = "appuserdetails",
    params = params
  )

  res <- lapply(names(res), function(k) {
    x <- res[[k]]
    if (isFALSE(x$success)) {
      return(NULL)
    }

    x <- x$data
    out <- x$friendsown
    names(out) <- paste0("friend_", names(out))
    cbind(
      appid = k,
      out,
      recommendations = x$recommendations$totalfriends,
      wishlisted = x$added_to_wishlist
    )
  })
  res <- do.call(rbind.data.frame, res)
  fields_as_data_frame(res)
}


#' @rdname get_userdata
#' @export
get_library_stats <- function() {
  check_authenticated()
  res <- request_storefront(
    api = store_api(),
    interface = "contenthub",
    method = "ajaxgetdlcstatsforuser"
  )
  res[!names(res) %in% "success"]
}


#' @rdname get_userdata
#' @export
get_owned_apps <- function() {
  check_authenticated()
  sessionid <- get_sessionid()
  params <- .make_params()
  res <- request_storefront(
    api = comm_api(),
    interface = "actions",
    method = "GetOwnedApps",
    params = params
  )
  as_data_frame(res)
}


#' @rdname get_userdata
#' @export
get_friends <- function() {
  check_authenticated()
  res <- request_storefront(
    api = comm_api(),
    interface = "actions",
    method = "ajaxlistfriends"
  )$friends

  as_data_frame(res)
}


#' @rdname get_userdata
#' @export
get_friend_data <- function(steamid) {
  check_authenticated()
  params <- .make_params(u = steamid, key = FALSE)
  res <- request_storefront(
    api = store_api(),
    interface = "friends",
    method = "frienddata",
    params = params
  )
  res <- pivot_longer_list(res, names_to = "appid")
  as_data_frame(res)
}


#' @rdname get_userdata
#' @export
get_badge <- function(appid, badgeid) {
  check_authenticated()
  params <- .make_params(key = FALSE)
  res <- request_storefront(
    api = comm_api(),
    interface = "actions",
    method = "ajaxgetbadgeinfo",
    params = params
  )

  res$success <- NULL
  res$completion_time <- as.POSIXct(res$completion_time)
  as_data_frame(res)
}


#' @rdname get_userdata
#' @export
get_badge_info <- function(appid) {
  check_authenticated()
  params <- .make_params(key = FALSE)
  res <- request_storefront(
    api = comm_api(),
    interface = "my",
    method = "ajaxgetbadgeinfo",
    params = params,
    params_as_query = FALSE
  )$badgedata

  fields_as_data_frame(res)
}


#' @rdname get_userdata
#' @export
recommend_apps <- function(appid) {
  check_authenticated()
  params <- .make_params(key = FALSE)
  res <- request_storefront(
    api = store_api(),
    interface = "explore",
    method = "ajaxgetstorerelevancedata",
    params = params
  )$results$similar_played_apps
  time_cols <- c(
    "last_playtime", "last_playtime_after_gap", "first_playtime",
    "first_windows_playtime", "first_mac_playtime", "first_linux_playtime",
    "first_deck_playtime", "last_deck_playtime",
    "last_windows_playtime", "last_mac_playtime", "last_linux_playtime"
  )

  for (k in time_cols) {
    res[[k]] <- as.POSIXct(res[[k]])
  }

  as_data_frame(res)
}


get_price_history <- function(appid, hash_name) {
  check_authenticated()
  params <- .make_params(key = FALSE)
  res <- request_storefront(
    api = comm_api(),
    interface = "market",
    method = "pricehistory",
    params = params
  )

  prefix <- res$price_prefix
  suffix <- res$price_suffix
  res <- res$prices
  attr(res, "prefix") <- prefix
  attr(res, "suffix") <- suffix
  as_data_frame(res)
}
