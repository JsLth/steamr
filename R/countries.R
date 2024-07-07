#' Location
#' @description
#' Retrieves information on Steam's country, region, and city codes.
#'
#' \code{query_locations} queries the storefront API and can drill down all
#' Steam location levels, while \code{get_country_list} can only provide
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
  request_storefront(
    comm_api(),
    interface = "actions",
    method = "QueryLocations",
    params = params
  )
}



#' @rdname query_locations
#' @export
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
