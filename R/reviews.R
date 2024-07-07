#' Get reviews
#' @description
#' Retrieve user reviews for a given Steam application or from a specific user.
#' Generate review histograms.
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
#' @param cursor For each review page query, \code{get_app_reviews} returns a
#' cursor ID. This ID can be used to paginate and iterate through many review pages
#' at once. This argument is probably useless for most use cases and is
#' extensively used for iteration purposes by \code{get_all_app_reviews}.
#' @inheritParams common
#'
#' @returns \describe{
#'  \item{get_app_reviews}{A dataframe containing information about the review.
#'  \code{recommendationid} contains the unique recommendationID. \code{language}
#'  contains the language of the review. \code{review} contains the review
#'  text. The \code{weighted_vote_score} represents Steams helpfulness weighting
#'  as described \href{https://steamcommunity.com/games/593110/announcements/detail/2666556941788470579}{here}.
#'  Weights closer to one indicate a higher helpfulness.}
#'
#'  \item{get_app_review}{A dataframe similar to the one returned by
#'  \code{get_app_reviews}, but with more detailed and undocumented metadata.}
#'
#'  \item{get_review_histogram}{A list containing two dataframes \code{rollups}
#'  and \code{recent}. Both dataframes contain the date and the corresponding
#'  upvotes and downvotes. \code{rollups} is for all reviews, \code{recent} only
#'  for the last month.}
#' }
#'
#' @export
#'
#' @examples
#' # reviews of counter-strike
#' get_app_reviews(10)
#'
#' # get all negative bulgarian reviews
#' get_app_reviews(10, language = "bulgarian", review_type = "negative")
#'
#' # replicate steam histogram
#' has_pkgs <- require(tidyr) &&
#'   require(dplyr) &&
#'   require(ggplot2) &&
#'   require(ggtext)
#'
#' if (has_pkgs) {
#'   histo <- get_review_histogram("440", language = NULL, review_score_preference = 2)
#'
#'   rollups <- histo$rollups %>%
#'     mutate(recommendations_down = recommendations_down * -1) %>%
#'     pivot_longer(
#'       cols = starts_with("rec"),
#'       names_prefix = "recommendations_",
#'       names_to = "vote"
#'     )
#'
#'   ggplot(rollups) +
#'     aes(x = date, y = value, fill = vote) +
#'     geom_bar(
#'       stat = "identity",
#'       show.legend = FALSE,
#'       width = resolution(as.double(rollups$date) * 0.5)
#'     ) +
#'     geom_hline(yintercept = 0, color = "#66c0f4") +
#'     scale_x_datetime(date_breaks = "2 years", date_labels = "%b %Y", expand = c(0, 0)) +
#'     scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
#'     scale_fill_manual(values = c(up = "#66c0f4", down = "#a34c25")) +
#'     labs(
#'       title = "Overall reviews:",
#'       subtitle = sprintf(
#'         paste(
#'           "<b style='color:#66c0f4;font-size:20px;text-shadow:8px 8px 16px",
#'           "#000000aa;font-weight:bold'>Very Positive</b> <span style='color:#8ba6b6;",
#'           "font-size:15px'>(%s reviews)</span>"
#'         ),
#'         format(sum(abs(rollups$value)), big.mark = ",")
#'       ),
#'       x = NULL,
#'       y = NULL
#'     ) +
#'     theme_minimal() +
#'     annotate(
#'       geom = "segment",
#'       y = as.POSIXct(Inf), yend = as.POSIXct(Inf),
#'       x = as.POSIXct(-Inf), xend = as.POSIXct(Inf)
#'     ) +
#'     theme(
#'       plot.background = element_rect(fill = "#2a475e"),
#'       text = element_text(color = "white"),
#'       panel.grid = element_blank(),
#'       axis.line = element_blank(),
#'       title = element_markdown(),
#'       axis.text.y = element_text(color = "white"),
#'       axis.text.x = element_text(color = "#66c0f4")
#'     )
#' }
#'
#' \dontrun{
#' # requires authentication
#' # get reviews for a single steamID
#' get_app_review("76561198092541763", c(730, 220200))
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
                            summary_num_positive_reviews = NULL,
                            summary_num_reviews = NULL,
                            paginate = FALSE,
                            max_pages = Inf) {
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
    num_per_page = 100,
    summary_num_positive_reviews = summary_num_positive_reviews,
    summary_num_reviews = summary_num_reviews,
    json = TRUE,
    cursor = "*"
  )
  res <- request_storefront(
    api = store_api(),
    interface = "appreviews",
    method = appid,
    params = params,
    paginate = if (paginate) list(
      method = "cursor",
      limit = max_pages,
      next_cursor = "cursor"
    )
  )

  if (paginate) {
    summary <- lapply(res, "[[", "summary")
    res <- rbind_list(lapply(res, "[[", "reviews"))
  } else {
    summary <- res$query_summary
    res <- res$reviews
  }


  res$timestamp_created <- as.POSIXct(res$timestamp_created)
  res$timestamp_updated <- as.POSIXct(res$timestamp_updated)
  res$author.last_played <- as.POSIXct(res$author.last_played)
  res$weighted_vote_score <- as.numeric(res$weighted_vote_score)
  res <- as_data_frame(res)
  res <- res[!duplicated(res), ]
  attr(res, "summary") <- summary
  res
}


#' @rdname get_app_reviews
#' @export
#'
#' @param steamids A vector of steamIDs corresponding to the appIDs in
#' \code{appids}. SteamIDs are recycled to match the length of \code{appids}.
#' @param appIDs A vector of appIDs written by the steamIDs in \code{steamids}.
#' AppIDs are recycled to match the length of \code{steamids}.
get_app_review <- function(steamids, appids) {
  check_string(steamids)
  check_number(appids)
  if (length(steamids) == 1) {
    steamids <- rep(steamids, length(appids))
  }
  if (length(appids) == 1) {
    appids <- rep(appids, length(steamids))
  }
  steamids <- convert_steamid(steamids, to = "steam64")

  requests <- Map(list, steamid =  steamids, appid = appids, USE.NAMES = FALSE)
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
#' @param review_score_preference Unknown.
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
