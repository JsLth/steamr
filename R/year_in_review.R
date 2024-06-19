#' Year in review
#' @description
#' Get retrospective user-related summaries about past years.
#'
#' @inheritParams common
#' @param year Review year to return data for. Must be a year smaller than
#' the current year, otherwise the response will be empty.
#' @param force_regenerate Whether to regenerate the year in review or re-use
#' a previously generated summary.
#' @param access_source Unknown.
#'
#' @returns \describe{
#'  \item{\code{user_in_review}}{A nested list containing lots of information
#'  about playtime, performance, and game acquisitions.}
#'
#'  \item{\code{friends_in_review}}{A dataframe containing all newly befriended
#'  Steam friends. The columns\code{privacy_state} and
#'  \code{rt_privacy_updated} tell about the friends' privacy state.}
#'
#'  \item{\code{achievements_in_review}}{A dataframe with one row per appID.
#'  \code{all_time_unlocked_achievements} and \code{unlocked_more_in_future}
#'  tell about past and future achievements. \code{achievements} is a nested
#'  dataframe that contains all achievement for a specific game in \code{year}}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' steamid <- "76561197960435530"
#'
#' # get all sorts of summary data from 2023
#' user_in_review(steamid, year = 2023)
#'
#' # all new friends in 2023
#' # authentication necessary
#' friends_in_review(steamid, year = 2023)
#'
#' # get a list of all achievements in 2023
#' achievements_in_review(steamid, appids = c(440, 730), year = 2023)
#'
#' # return a summary of total achievements in 2023
#' achievements_in_review(
#'   steamid,
#'   appids = c(440, 730),
#'   year = 2023,
#'   total_only = TRUE
#' )
#'
#' # return a summary of screenshots in 2023
#' # actually, this returns an empty tibble because I couldn't
#' # find an example of a working user/app combination
#' screenshots_in_review(steamid, year = 2023)
#' }
user_in_review <- function(steamid,
                           year = 2023,
                           force_regenerate = FALSE,
                           access_source = FALSE) {
  check_string(steamid)
  check_number(year)
  check_bool(force_regenerate)
  check_bool(access_source)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface =  "ISaleFeatureService",
    method = "GetUserYearInReview",
    version = "v1",
    params = params
  )$response
}


#' @rdname user_in_review
#' @export
#' @param return_private Unknown.
friends_in_review <- function(steamid, year = 2023, return_private = FALSE) {
  check_authenticated()
  check_string(steamid)
  check_number(year)
  check_bool(return_private)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISaleFeatureService",
    method = "GetFriendsSharedYearInReview",
    version = "v1",
    params = params
  )$response$friend_shares
  as_data_frame(res)
}


#' @rdname user_in_review
#' @export
#' @param total_only If \code{TRUE}, returns only a summary list of
#' achievements in \code{year}.
achievements_in_review <- function(steamid,
                                   appids,
                                   year = 2023,
                                   total_only = FALSE) {
  check_string(steamid)
  check_number(appids)
  check_number(year)
  check_bool(total_only)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISaleFeatureService",
    method = "GetUserYearAchievements",
    version = "v1",
    params = params
  )$response

  if (!total_only) {
    res <- as_data_frame(res$game_achievements)
  }

  res
}


#' @rdname user_in_review
#' @export
#' @inheritParams common
screenshots_in_review <- function(steamid, appids, year = 2023) {
  check_string(steamid)
  check_number(appids)
  check_number(year)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISaleFeatureService",
    method = "GetUserYearScreenshots",
    version = "v1",
    params = params
  )$response$apps
  as_data_frame(res)
}
