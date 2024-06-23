#' Events
#' @description
#' Fetches data about news and events from the Steam News Hub.
#'
#' \code{query_events} queries all events based on arbitrary filters.
#' \code{get_best_events} fetches only the best events, whatever that means.
#' \code{get_event_details} returns details about a number of events within
#' a number of groups.
#'
#' @param min_time,max_time Time range for which to include events.
#' @param ascending Whether to sort event dates in an ascending order.
#' @param max_results Maximum number of events to return. A minimum of 10
#' documents will always be returned. The maximum number of documents is
#' restricted to 500.
#' @param populate Number of events to populate with additional event data.
#' Adds a a field \code{events} to the data that essentially contains the
#' output of \code{get_event_details} for the specified number of events.
#' @param app_types Vector of app types to be returned. Can include
#' \code{library}, \code{wishlist}, \code{following}, \code{recommended},
#' \code{steam}, \code{curator} and \code{featured}.
#' @param event_types Vector of event types. Can include a combination of
#' numbers representing event types. That's all I know.
#' @param collection_id Unknown.
#' @param sale_id Unknown.
#'
#' @returns \code{query_events} returns a list of dataframes. \code{documents}
#' contains information on the event documents. \code{apps} contains information
#' about the event apps. If authenticated, also shows information about last
#' playtime if applicable. \code{clans} shows information about the group of
#' each event. If \code{populate} is larger than 0, also shows an \code{events}
#' field, that shows resolved information about each document including their
#' text body.
#'
#' \code{get_best_events} and \code{get_event_details} essentially return a
#' list in the same format as the \code{events} field in \code{query_events}.
#'
#' @rdname events
#' @export
#'
#' @examples
#' \dontrun{
#' # set a time filter
#' query_events(
#'   min_time = as.POSIXct("2023-01-01"),
#'   max_time = as.POSIXct("2023-01-31")
#' )
#'
#' # get event details about first 20 events
#' query_events(populate = 20)
#'
#' # show only events of games in library
#' query_events(app_types = "library")
#' }
query_events <- function(min_time = NULL,
                         max_time = Sys.time(),
                         ascending = FALSE,
                         max_results = NULL,
                         populate = FALSE,
                         app_types = NULL,
                         event_types = NULL,
                         collection_id = NULL,
                         sale_id = NULL) {
  params <- .make_params(
    minTime = as.numeric(min_time),
    maxTime = as.numeric(max_time),
    ascending = ascending,
    maxResults = max_results,
    populateEvents = populate,
    appTypes = app_types,
    eventTypes = event_types,
    collectionID = collection_id,
    saleID = sale_id,
    key = FALSE
  )
  res <- request_storefront(
    api = store_api(),
    interface = "events",
    method = "ajaxgetusereventcalendarrange",
    params = params
  )

  res <- drop_null(res[c("documents", "apps", "clans", "events")])
  res$documents$start_time <- as.POSIXct(res$documents$start_time)
  res <- lapply(res, as_data_frame)
  res
}


#' @param language Language in which event data should be returned.
#' @param include_steam_blog Whether events from the Steam blog should be
#' included.
#' @param recency Specifies the number of days that game must have been last
#' played to be included in the query.
#' @param only_game_updates Whether to filter out all events that are not
#' game updates.
#'
#' @rdname events
#' @export
get_best_events <- function(language = "english",
                            include_steam_blog = TRUE,
                            recency = 0,
                            only_game_updates = FALSE) {
  check_authenticated()
  params <- .make_params(
    l = language,
    include_steam_blog = include_steam_blog,
    filter_to_played_within_days = recency,
    include_only_game_updates = only_game_updates,
    key = FALSE
  )
  request_storefront(
    api = store_api(),
    interface = "events",
    method = "ajaxgetbesteventsforuser",
    params = params
  )
}


#' @param gid Vector of event IDs for which to provide details.
#' @param clanid List of group IDs for which the events in \code{gid} were
#' created.
#'
#' @rdname events
#' @export
get_event_details <- function(gid, clanid) {
  check_string(gid)
  check_number(clanid)
  gid <- convert_steamid(gid, to = "steam64", vanity_type = "group")

  params <- .make_params(
    uniqueid_list = gid,
    clanid_list = clanid,
    key = FALSE
  )
  request_storefront(
    api = store_api(),
    interface = "events",
    method = "ajaxgeteventdetails",
    params = params
  )
}
