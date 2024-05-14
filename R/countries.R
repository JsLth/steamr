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
