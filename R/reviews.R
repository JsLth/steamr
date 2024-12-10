#' Get reviews
#' @description
#' Retrieve user reviews for a given Steam application.
#'
#' \itemize{
#'  \item{\code{get_app_reviews} retrieves up to 100 reviews.}
#'  \item{\code{get_all_app_reviews} has the ability to return much more reviews
#' depending on input parameters.}
#' }
#'
#' @param appid Application ID referencing a Steam application. There are
#' generally two ways of retrieving an application ID. The first one is to
#' inspect the URL of a Steam game. For example the application ID of
#' Counter-Strike is 10:
#' \code{https://store.steampowered.com/app/10/CounterStrike/}.
#'
#' It is also possible to retrieve application IDs programmatically by
#' exploiting Steam's search API. For details see \code{\link{search_apps}}.
#' @param day_range Number of days from now to \code{n} days ago to look for
#' reviews.
#' @param start_date Date-time object describing the earliest time to look for
#' reviews. Ignored if \code{date_range_type = "all"}.
#' @param end_date Date-time object describing the latest time to look for
#' reviews. Ignored if \code{date_range_type = "all"}.
#' @param date_range_type Specifies what to do with reviews within the
#' date range set by \code{start_date} and \code{end_date}. Options include
#' \code{include}, \code{exclude}, and \code{all}. If \code{all}, ignores
#' \code{end_date} and \code{start_date}.
#' @param filter Specifies the sort order. Can be one of \code{summary},
#' \code{recent}, \code{updated}, and \code{all}. \code{summary} returns
#' a summary of 10 most helpful comments. \code{all} sorts by helpfulness
#' and applies moving windows in order to always return reviews. \code{recent}
#' sorts by creation time and \code{updated} sorts by update time. For
#' \code{get_all_app_reviews}, \code{filter} must be either \code{updated} or
#' \code{recent}.
#' @param language Which review language to include in the output. A full
#' list of platform supported languages can be found in the
#' \href{https://partner.steamgames.com/doc/store/localization/languages}{Steamworks documentation}.
#' Defaults to \code{"all"}, which removes the language filter.
#' @param review_type Filters by the type of review. Can be either
#' \code{positive}, \code{negative}, or \code{all}.
#' @param purchase_type Filters by the type of purchase. Can be either
#' \code{steam}, \code{non_steam_purchase}, or \code{all}.
#' @param playtime Length-2 numeric vector specifying the minimum and maximum
#' playtime in hours that the reviewer should have played the game. If
#' \code{0}, no required (minimum or maximum) play time is set.
#' @param filter_offtopic_activity Whether to filter offtopic activity. This
#' especially includes so-called "review bombs", i.e. many reviews written
#' by users in a very short time to artificially alter the review score of a
#' game. See the Steam
#' \href{https://steamcommunity.com/games/593110/announcements/detail/1808664240333155775}{blog entry}
#' for more information.
#' @param num_per_page By default, Steam only shows 20 reviews per query.
#' By setting \code{num_per_page}, this number can go up to a maximum of 100.
#' For larger queries, consider iterating using \code{get_all_app_reviews}.
#' @param cursor For each review page query, \code{get_app_reviews} returns a
#' cursor ID. This ID can be used to paginate and iterate through many review pages
#' at once. This argument is probably useless for most use cases and is
#' extensively used for iteration purposes by \code{get_all_app_reviews}.
#'
#' @returns A dataframe containing IDs, texts, and metadata of application
#' reviews on Steam.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # reviews of counter-strike
#' get_app_reviews(10)
#'
#' # get all negative bulgarian reviews
#' get_app_reviews(10, language = "bulgarian", review_type = "negative")
#'
#' # get all recent reviews up to page 10
#' get_all_app_reviews(10, filter = "recent", max_tries = 10)
#' }
get_app_reviews <- function(appid,
                            day_range = NULL,
                            start_date = NULL,
                            end_date = NULL,
                            date_range_type = NULL,
                            filter = "all",
                            language = "all",
                            review_type = "all",
                            purchase_type = "steam",
                            playtime = c(0, 0),
                            filter_offtopic_activity = TRUE,
                            num_per_page = 20,
                            cursor = NULL) {
  check_length(playtime, ge = 2, le = 2)
  params <- list(
    day_range = day_range,
    start_date = start_date,
    end_date = end_date,
    date_range_type = date_range_type,
    filter = filter,
    language = language,
    review_type = review_type,
    purchase_type = purchase_type,
    playtime_filter_min = playtime[1],
    playtime_filter_max = playtime[2],
    filter_offtopic_activity = filter_offtopic_activity,
    num_per_page = num_per_page,
    summary_num_positive_reviews = summary_num_positive_reviews,
    summary_num_reviews = summary_num_reviews,
    json = TRUE,
    cursor = cursor
  )
  res <- request_storefront(
    api = store_api(),
    interface = "appreviews",
    method = appid,
    params = params
  )
  summary <- res$query_summary
  cursor <- res$cursor
  res <- as_data_frame(res$reviews)
  res$timestamp_created <- as.POSIXct(res$timestamp_created)
  res$timestamp_updated <- as.POSIXct(res$timestamp_updated)
  res$author.last_played <- as.POSIXct(res$author.last_played)
  res$weighted_vote_score <- as.numeric(res$weighted_vote_score)
  attr(res, "summary") <- summary
  attr(res, "cursor") <- cursor
  res
}


#' @rdname get_app_reviews
#' @export
#' @param max_pages Maximum number of pages until breaking the iteration
#' and returning the result up to this point. You may set this to \code{Inf}
#' to retrieve all reviews for a given query.
get_all_app_reviews <- function(appid,
                                day_range = NULL,
                                start_date = NULL,
                                end_date = NULL,
                                date_range_type = NULL,
                                filter = "recent",
                                language = "all",
                                review_type = "all",
                                purchase_type = "steam",
                                playtime = c(0, 0),
                                filter_offtopic_activity = TRUE,
                                max_tries = 10) {
  if (any(c("all", "summary") %in% filter)) {
    stop("When batch querying reviews, filter cannot be \"all\" or \"summary\".")
  }

  res <- list()
  cursor <- "*"
  i <- 1
  while (!any(duplicated(cursor)) && i < max_tries) {
    res[[i]] <- get_app_reviews(
      appid,
      day_range = day_range,
      start_date = start_date,
      end_date = end_date,
      date_range_type = date_range_type,
      filter = filter,
      language = language,
      review_type = review_type,
      purchase_type = purchase_type,
      playtime = playtime,
      filter_offtopic_activity = filter_offtopic_activity,
      num_per_page = 100,
      cursor = cursor[length(cursor)]
    )
    cursor[i] <- attr(res[[i]], "cursor")
    i <- i + 1
  }
  res <- rbind_list(res)
  res[!duplicated(res), ]
}


get_app_review <- function(steamid, appid) {
  check_string(steamid)
  check_number(appid)
  steamid <- convert_steamid(steamid, to = "steam64")

  requests <- list(steamid = steamid, appid = appid)
  input_json <- .make_input_json(requests = list(requests))
  params <- .make_params(input_json = input_json)
  res <- request_webapi(
    api = public_api(),
    interface = "IUserReviewsService",
    method = "GetIndividualRecommendations",
    version = "v1",
    params = params
  )$response$recommendations
  as_data_frame(res)
}


#' @rdname get_app_reviews
#' @export
get_review_histogram <- function(appid,
                                 language = "english",
                                 review_score_preference = 2) {
  check_string(language, null = TRUE)
  check_integerish(review_score_preference)

  params <- list(
    l = language,
    review_score_preference = review_score_preference
  )
  res <- request_storefront(
    api = store_api(),
    interface = "appreviewhistogram",
    method = appid,
    params = params
  )

  count_all <- res$count_all_reviews
  expand <- res$expand_graph
  type <- res$results$rollup_type

  res <- list(rollups = res$results$rollups, recent = res$results$recent)
  res$rollups$date <- as.POSIXct(res$rollups$date)
  res$recent$date <- as.POSIXct(res$recent$date)
  res
}
