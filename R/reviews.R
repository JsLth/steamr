#' Reviews
#' @description
#' Retrieve user reviews for a given Steam application.
#'
#' \itemize{
#'  \item{\code{get_app_reviews} retrieves up to 100 reviews.}
#'  \item{\code{get_all_app_reviews} has the ability to return much more reviews
#' depending on input parameters.}
#' }
#'
#' @param day_range Number of days from now to \code{n} days ago to look for
#' reviews, e.g. \code{day_range = 5} looks for reviews in the last 5 days.
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
#' and applies moving windows in order to always return reviews (see the Steam
#' \href{https://steamcommunity.com/games/593110/announcements/detail/2666556941788470579}{blog article}).
#' \code{recent} sorts by creation time and \code{updated} sorts by update time.
#' For \code{get_all_app_reviews}, \code{filter} must be either \code{updated} or
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
#' @inheritParams common
#'
#' @returns \code{stf_app_reviews} and \code{wba_app_review} return a dataframe
#' containing IDs, texts, and metadata of application reviews on Steam. See the
#' \href{https://github.com/Revadike/InternalSteamWebAPI/wiki/Get-App-Reviews#response}{community docs}
#' for more info on the output of \code{stf_app_reviews}.
#'
#' \code{stf_review_histogram} returns a named list containing rollups aggregated
#' by month (\code{rollups}), and rollups of the last 30 days aggregated by
#' day (\code{recent}). Each list element is a dataframe containing the
#' corresponding date and the aggregated thumps up/down.
#'
#' @evalRd auth_table(
#'   list("stf_app_reviews", key = FALSE, login = FALSE, note = NULL),
#'   list("wba_app_review", key = FALSE, login = TRUE, note = "Can only retrieve reviews of the authenticated user"),
#'   list("stf_review_histogram", key = FALSE, login = FALSE)
#' )
#'
#' @export
#'
#' @examples
#' \donttest{# reviews of counter-strike
#' stf_app_reviews(10)
#'
#' # get all negative german reviews
#' stf_app_reviews(
#'   10,
#'   language = "german",
#'   review_type = "negative",
#'   paginate = TRUE,
#'   max_pages = Inf
#' )
#'
#' # get all recent reviews up to page 10
#' stf_app_reviews(10, filter = "recent", max_pages = 10)
#'
#' (hist <- stf_review_histogram(10)$rollups)
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'
#'   ggplot(hist) +
#'     geom_bar(
#'       aes(x = date, y = recommendations_up),
#'       stat = "identity",
#'       fill = "#66c0f4",
#'       color = NA,
#'       width = resolution(as.double(hist$date) * 0.5)
#'     ) +
#'     geom_bar(
#'       aes(x = date, y = recommendations_down * - 1),
#'       stat = "identity",
#'       fill = "#a34c25",
#'       color = NA,
#'       width = resolution(as.double(hist$date) * 0.5)
#'     ) +
#'     geom_hline(yintercept = 0, color = "#66c0f4") +
#'     labs(title = "Overall reviews: Counter-Strike", x = NULL, y = NULL) +
#'     theme_minimal() +
#'     theme(
#'       plot.background = element_rect(fill = "#2a475e"),
#'       text = element_text(color = "white"),
#'       panel.grid = element_blank(),
#'       axis.line = element_blank(),
#'       axis.text.y = element_text(color = "white"),
#'       axis.text.x = element_text(color = "#66c0f4")
#'     )
#' }}
stf_app_reviews <- function(appid,
                            day_range = NULL,
                            start_date = NULL,
                            end_date = NULL,
                            date_range_type = "all",
                            filter = "recent",
                            language = "all",
                            review_type = "all",
                            purchase_type = "steam",
                            playtime = c(0, 0),
                            filter_offtopic_activity = TRUE,
                            paginate = FALSE,
                            max_pages = 10,
                            num_per_page = 20,
                            cursor = NULL) {
  if (paginate) {
    args <- as.list(environment())
    res <- do.call(stf_all_app_reviews, args)
    return(res)
  }

  assert_number(appid)
  assert_number(day_range, null.ok = TRUE)
  assert_posixct(start_date, null.ok = TRUE)
  assert_posixct(end_date, null.ok = TRUE)
  assert_string(date_range_type)
  assert_string(filter)
  assert_string(language)
  assert_string(review_type)
  assert_string(purchase_type)
  assert_numeric(playtime)
  assert_vector(playtime, len = 2)
  assert_flag(filter_offtopic_activity)
  assert_flag(paginate)
  assert_integerish(max_pages)
  assert_integerish(num_per_page)
  assert_string(cursor, null.ok = TRUE)

  if (!is.null(start_date)) {
    start_date <- as.numeric(start_date)
  }

  if (!is.null(end_date)) {
    end_date <- as.numeric(end_date)
  }

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
  res <- res$reviews
  res$timestamp_created <- as.POSIXct(res$timestamp_created)
  res$timestamp_updated <- as.POSIXct(res$timestamp_updated)
  res$author.last_played <- as.POSIXct(res$author.last_played)
  res$weighted_vote_score <- as.numeric(res$weighted_vote_score)
  res <- as_data_frame(res)
  attr(res, "summary") <- summary
  attr(res, "cursor") <- cursor
  res
}


stf_all_app_reviews <- function(appid,
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
                                max_pages = 10,
                                num_per_page = 20,
                                ...) {
  if (any(c("all", "summary") %in% filter)) {
    stop("When batch querying reviews, filter cannot be \"all\" or \"summary\".")
  }

  res <- list()
  cursor <- "*"
  i <- 1
  while (!any(duplicated(cursor)) && i < max_pages) {
    res[[i]] <- stf_app_reviews(
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
      num_per_page = num_per_page,
      cursor = cursor[length(cursor)]
    )
    cursor[i] <- attr(res[[i]], "cursor")
    i <- i + 1
  }
  res <- rbind_list(res)
  res[!duplicated(res), ]
}


#' @rdname stf_app_reviews
#' @export
wba_app_review <- function(steamid, appid) {
  assert_string(steamid)
  assert_integerish(appid)
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


#' @rdname stf_app_reviews
#' @param review_score_preference Unknown. In my tests, changing this argument
#' did not yield a measurable difference in the results.
#' @export
stf_review_histogram <- function(appid,
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
  lapply(res, as_data_frame)
}
