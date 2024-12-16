#' Charts
#' @description
#' Retrieve Steam (weekly) charts in terms of concurrent players (CCU), total
#' most played games, and top new releases of the last three months.
#'
#' @inheritParams wba_store_items
#' @inheritParams store_context
#'
#' @param country ISO 3166 country code representing the country from
#' which to view the Steam store. A list of Steam countries can be retrieved
#' with \code{\link{get_country_list}}.
#' @param start_date Date-time object giving the earliest time for which to
#' retrieve top sellers. Results will be returned for the time between
#' the start date and the start date plus seven days.
#'
#' @returns
#' \itemize{
#'  \item{\code{wba_charts_weekly}: A dataframe containing the first 100 ranks,
#'  the appID as well as detailed information about the ranking history. Also
#'  includes item info as returned by \code{\link{wba_store_items}}. The
#'  actual start date of the weekly count is included in the \code{start_date}
#'  attribute.}
#'  \item{\code{wba_charts_ccu}: A dataframe containing the first 100 ranks,
#'  their appID, concurrent players and peak players.}
#'  \item{\code{wba_charts_total}: A dataframe containing the first 100 ranks,
#'  their appID, rank change, and peak players.}
#'  \item{\code{wba_charts_releases}: A dataframe containing three rows
#'  representing the last three months. Each row contains the appIDs of the
#'  top 88 new releases of that month.}
#' }
#'
#' @evalRd auth_table(
#'   list("wba_charts_ccu", key = FALSE, login = FALSE),
#'   list("wba_charts_ccu", key = FALSE, login = FALSE),
#'   list("wba_charts_total", key = FALSE, login = FALSE),
#'   list("wba_charts_releases", key = FALSE, login = FALSE)
#' )
#'
#' @details
#' Although a store context can be passed to \code{wba_charts_ccu}, it does not
#' seem to be taken into account. To return a regionalized version of the
#' Steam charts, you may use \code{wba_charts_weekly}.
#'
#' @seealso
#' \code{\link{wba_apps}} to return a list of all apps
#'
#' \code{\link{stf_frontpage}} to
#' retrieve (genre) charts as shown on the store front page
#'
#' @export
#' @name wba_charts
#'
#' @examples
#' \donttest{# get most played games
#' wba_charts_total()
#'
#' # worldwide weekly top sellers
#' get_weekly_top_sellers()
#'
#' # weeky top sellers of South Korea
#' get_weekly_top_sellers("KR")
#'
#' # top sellers of last month
#' get_weekly_top_sellers(start_date = Sys.time() - 2629800)}
wba_charts_weekly <- function(country = NULL,
                              context = store_context(),
                              data_request = store_data_request(),
                              start_date = NULL) {
  check_string(country, null = TRUE)
  check_class(context, "StoreBrowseContext")
  check_class(data_request, "StoreBrowseItemDataRequest")
  check_date(start_date, null = TRUE)

  input_json <- .make_input_json(
    country_code = country,
    context = context,
    data_request = data_request,
    start_date = as.numeric(start_date),
    page_count = 100
  )

  params <- .make_params(input_json = input_json)
  res <- request_webapi(
    api = public_api(),
    interface = "IStoreTopSellersService",
    method = "GetWeeklyTopSellers",
    version = "v1",
    params = params
  )
  ts <- res$response$start_date
  res <- as_data_frame(res$response$ranks)
  attr(res, "start_date") <- ts
  res
}


#' @rdname wba_charts
#' @export
wba_charts_ccu <- function(context = store_context(),
                           data_request = store_data_request()) {
  context <- context %||% store_context()
  data_request <- data_request %||% store_data_request()
  input_json <- .make_input_json(
    context = context,
    data_request = data_request
  )

  params <- .make_params(input_json = input_json)
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamChartsService",
    method = "GetGamesByConcurrentPlayers",
    params = params
  )$response$ranks
  as_data_frame(res)
}


#' @rdname wba_charts
#' @export
wba_charts_total <- function() {
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamChartsService",
    method = "GetMostPlayedGames"
  )$response$ranks
  as_data_frame(res)
}


#' @rdname wba_charts
#' @export
wba_charts_releases <- function() {
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamChartsService",
    method = "GetTopReleasesPages",
    version = "v1"
  )$response$pages
  res$item_ids <- list(unlist(res$item_ids, use.names = FALSE))
  res$start_of_month <- as.POSIXct(res$start_of_month)
  as_data_frame(res)
}
