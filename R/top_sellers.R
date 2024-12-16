#' Weekly top sellers
#' @description
#' Retrieve the top 100 weekly top sellers of a given country.
#'
#' @param country ISO-2 code of the country for which to return top sellers.
#' A list of Steam countries can be retrieved with
#' \code{\link{get_country_list}}.
#' @param start_date Date-time object giving the earliest time for which to
#' retrieve top sellers. Results will be returned for the time between
#' the start date and the start date plus seven days.
#' @param page_start On which part to start looking. Defaults to page 1.
#' @param page_count Maximum pages to return. Defaults to one page.
#' @inheritParams common
#'
#' @returns A dataframe containing information on weekly top sellers.
#' Metadata is stored in attributes: \code{timestamp} holds the earliest
#' time of the week, \code{next_page_start} specifies where the next page
#' would start.
#'
#' @export
#' @seealso \code{\link{get_country_list}}
#'
#' @examples

get_weekly_top_sellers <- function(country = NULL,
                                   language = "english",
                                   elanguage = NULL,
                                   country_code = "US",
                                   steam_realm = 1L,
                                   include = NULL,
                                   apply_user_filters = FALSE,
                                   start_date = NULL) {
  check_string(country, null = TRUE)
  check_date(start_date, null = TRUE)

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
  as_data_frame(res$response$ranks)
}
