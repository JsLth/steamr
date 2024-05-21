#' Country info
#' @description
#' Retrieves information on Steam's country, region, and city codes.
#'
#' @param country ISO-2 country code. If specified, returns information
#' on regions, otherwise countries.
#' @param region Region code within a country specified in \code{country}.
#' Country must not be \code{NULL} if \code{region} is specified. If specified,
#' returns information on cities, otherwise on regions.
#'
#' @returns A dataframe with information on country code, region code, city
#' code and geographical name.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # get country codes
#' query_locations()
#'
#' # get region codes of Germany
#' query_locations(country = "DE")
#'
#' # get city codes of Baden-Wurttemberg, Germany
#' query_locations(country = "DE", region = "01")
#'
#' # get localized country names
#' get_country_list(language = "german")
#' }
query_locations <- function(country = NULL, region = NULL) {
  check_string(country, null = TRUE)
  check_string(region, null = TRUE)
  params <- .make_params(key = FALSE)
  request_internal(
    comm_api(),
    interface = "actions",
    method = "QueryLocations",
    params = params
  )
}



#' @rdname query_locations
#' @inheritParams get_items
#' @returns A dataframe containing country codes and names
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
get_country_list <- function(language = "english") {
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IStoreTopSellersService",
    method = "GetCountryList",
    version = "v1",
    params = params
  )
  as_data_frame(res$response$countries)
}



#' Weekly top sellers
#' @description
#' Get a list of weekly top sellers by country.
#'
#' @param country ISO-2 code of the country for which to return top sellers.
#' @param start_date Date-time object giving the earliest time for which to
#' retrieve top sellers. Results will be returned for the time between
#' the start date and the start date plus seven days.
#' @param page_start On which part to start looking. Defaults to page 1.
#' @param page_count Maximum pages to return. Defaults to one page.
#' @inheritParams get_items
#'
#' @returns A dataframe containing information on weekly top sellers.
#' Metadata is stored in attributes: \code{timestamp} holds the earliest
#' time of the week, \code{next_page_start} specifies where the next page
#' would start.
get_weekly_top_sellers <- function(country,
                                   language = "english",
                                   elanguage = NULL,
                                   country_code = "US",
                                   steam_realm = 1L,
                                   include = NULL,
                                   apply_user_filters = FALSE,
                                   start_date = NULL,
                                   page_start = NULL,
                                   page_count = NULL) {
  check_integerish(start_date, null = TRUE)
  check_integerish(page_start, null = TRUE)
  check_integerish(page_count, null = TRUE)

  context <- store_browse_context(
    language = language,
    elanguage = elanguage,
    country_code = country_code,
    steam_realm = steam_realm
  )
  data_request <- store_browse_item_data_request(
    include = include,
    apply_user_filters = apply_user_filters
  )
  input_json <- jsonlite::toJSON(
    list(context = context, data_request = data_request),
    auto_unbox = TRUE,
    force = TRUE
  )

  params <- .make_params(
    country_code = country,
    input_json = input_json,
    start_date = start_date,
    page_start = page_start,
    page_count = page_count
  )
  res <- request_webapi(
    api = public_api(),
    interface = "IStoreTopSellersService",
    method = "GetWeeklyTopSellers",
    version = "v1",
    params = params
  )
  timestamp <- as.POSIXct(res$response$start_date)
  next_page <- res$response$next_page_start
  res <- as_data_frame(res$response$ranks)
  attr(res, "timestamp") <- timestamp
  attr(res, "next_page_start") <- next_page
  res
}
