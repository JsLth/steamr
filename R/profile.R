#' Profiles
#' @description
#' Fetch cosmetic information from a user profile.
#'
#' @inheritParams common
#'
#' @export
#'
get_profile_items <- function(steamid, language = "english") {
  check_string(steamid)
  check_string(language, null = TRUE)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "ILoyaltyRewardsService",
    method = "GetEquippedProfileItems",
    version = "v1",
    params = params
  )$response
}


#' @rdname get_profile_items
#' @export
get_profile_reactions <- function(steamid) {
  check_string(steamid)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "ILoyaltyRewardsService",
    method = "GetReactionsSummaryForUser",
    version = "v1",
    params = params
  )$response
}


#' @rdname get_profile_items
#' @export
get_profile_avatar <- function(steamid, language = "english") {
  check_string(steamid)
  check_string(language, null = TRUE)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetAnimatedAvatar",
    version = "v1",
    params = params
  )$response$avatar
}


#' @rdname get_profile_items
#' @export
get_profile_frame <- function(steamid, language = "english") {
  check_string(steamid)
  check_string(language, null = TRUE)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetAvatarFrame",
    version = "v1",
    params = params
  )$response$avatar_frame
}


#' @rdname get_profile_items
#' @export
get_profile_background <- function(steamid, language = "english") {
  check_string(steamid)
  check_string(language, null = TRUE)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetProfileBackground",
    version = "v1",
    params = params
  )$response$profile_background
}


#' @rdname get_profile_items
#' @export
get_profile_mini_background <- function(steamid, language = "english") {
  check_string(steamid)
  check_string(language, null = TRUE)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetMiniProfileBackground",
    version = "v1",
    params = params
  )$response$profile_background
}


#' @rdname get_profile_items
#' @export
get_profile_badges <- function(steamid) {
  check_string(steamid)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetBadges",
    version = "v1",
    params = params
  )$response$badges
  as_data_frame(res)
}


#' @rdname get_profile_items
#' @export
get_profile_items <- function(steamid,
                              language = "english",
                              status = c("owned", "equipped")) {
  check_authenticated()
  check_string(steamid)
  check_string(language, null = TRUE)
  status <- match.arg(status)
  steamid <- convert_steamid(steamid, to = "steam64")

  method <- paste0("GetProfileItems", to_title(status))
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = method,
    version = "v1",
    params = params
  )$response
  lapply(res, as_data_frame)
}


#' @rdname get_profile_items
#' @export
get_profile_themes <- function() {
  check_authenticated()
  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetProfileThemesAvailable",
    version = "v1",
    params = params
  )$response
}


#' @rdname get_profile_items
#' @export
get_profile_customization <- function(steamid,
                                      include_inactive = FALSE,
                                      include_purchased = FALSE) {
  check_string(steamid)
  check_bool(include_inactive)
  check_bool(include_purchased)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetProfileCustomization",
    version = "v1",
    params = params
  )$response
  res$customizations <- as_data_frame(res$customizations)
  res
}
