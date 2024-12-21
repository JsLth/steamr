#' Location
#' @description
#' Retrieves information on Steam's country, region, and city codes.
#'
#' \code{stf_locations} queries the storefront API and can drill down all
#' Steam location levels, while \code{wba_countries} can only provide
#' information on the country level.
#'
#' @param country ISO-2 country code. If specified, returns information
#' on regions, otherwise countries.
#' @param region Region code within a country specified in \code{country}.
#' Country must not be \code{NULL} if \code{region} is specified. If specified,
#' returns information on cities, otherwise on regions.
#'
#' @returns \describe{
#'  \item{\code{query_locations}}{A dataframe with country code, name,
#'  and whether the location can be further subdivided into states or cities.}
#'
#'  \item{\code{get_country_list}}{A dataframe containing country code
#'  and country name.}
#' }
#'
#' @export
#'
#' @evalRd auth_table(
#'   list("stf_locations", key = FALSE, login = FALSE),
#'   list("wba_countries", key = FALSE, login = FALSE)
#' )
#'
#' @examples
#' \donttest{# get country codes
#' stf_locations()
#'
#' # get region codes of Germany
#' stf_locations(country = "DE")
#'
#' # get city codes of Baden-Wurttemberg, Germany
#' stf__locations(country = "DE", region = "01")
#'
#' # get localized country names
#' wba_countries(language = "german")}
stf_locations <- function(country = NULL, region = NULL) {
  assert_string(country, null.ok = TRUE)
  assert_string(region, null.ok = TRUE)
  params <- .make_params(key = FALSE)
  request_storefront(
    comm_api(),
    interface = "actions",
    method = "QueryLocations",
    params = params
  )
}



#' @rdname stf_locations
#' @export
#' @inheritParams store_context
wba_countries <- function(language = "english") {
  assert_string(language, null.ok = TRUE)
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
