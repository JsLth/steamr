#' Storefront search
#' @description
#' Simple Steam store queries that return the best matches to a given search
#' query. Uses Steam's storefront API.
#'
#' Steam provides different endpoints that essentially do the same, but
#' in a slightly different manner.
#' \itemize{
#'  \item{\code{stf_search_apps} has less filtering capabilities, provides no
#'  context and returns a slim output.}
#'  \item{\code{stf_suggest} provides many filtering capabilities, provides context
#'  and returns a slightly more detailed output.}
#'  \item{\code{stf_search_store} has no filtering capabilities, provides context
#'  and returns a detailed output.}
#' }
#'
#' @param term Search term used to query the Steam store.
#' @param links Whether to include links to icon and logo in the output.
#' Defaults to \code{FALSE} to avoid littering the output.
#'
#' @returns A dataframe containing at least the appid and application name.
#' \code{stf_suggest} returns additional information on app type and price.
#' \code{stf_search_store} returns additional info on metascore, current video
#' streams, hardware support, and price details. If \code{links = TRUE}, also
#' includes links to application icon and logo.
#'
#' @evalRd auth_table(
#'   list("stf_search_apps", key = FALSE, login = FALSE),
#'   list("stf_suggest", key = FALSE, login = FALSE),
#'   list("stf_search_store", key = FALSE, login = FALSE)
#' )
#'
#' @export
#'
#' @seealso
#' \code{\link{get_app_list}} and \code{\link{steamspy}} for ways
#' to retrieve all applications
#'
#' \code{\link{query}} to query all apps using arbitrary filters.
#'
#' @examples
#' \donttest{# `stf_search_apps` is suitable for simple queries with low overhead.
#' stf_search_apps("fortress")
#'
#' # Multiple terms can be searched in a single call
#' stf_search_apps(c("team", "fortress"))
#'
#' # `stf_suggest` is suitable for suggestion systems (e.g. in a search bar)
#' # as it returns only the first results, allows more sophisticated filtering
#' # and provides basic metadata.
#' stf_suggest("fortress")
#' stf_suggest("team", require_type = "dlc")
#' stf_suggest("team", language = "german", country_code = "DE")
#'
#' # `stf_search_store` provides detailed metadata
#' stf_search_store("fortress")}
stf_search_apps <- function(term, links = FALSE) {
  assert_character(term, min.len = 1)
  assert_flag(links)

  res <- lapply(term, function(x) {
    request_storefront(
      api = comm_api(),
      interface = "actions",
      method = file.path("SearchApps", x)
    )
  })
  names(res) <- term
  res <- bind_rows(res, .id = "term")

  if (!links) {
    res <- res[c("term", "appid", "name")]
  }

  res
}


#' @inheritParams store_context
#' @param require_type Which application types should be returned? Can be
#' one of \code{bundle}, \code{game}, \code{demo}, \code{mod} \code{dlc},
#' \code{software}, \code{video}, \code{hardware}, \code{series},
#' and \code{music}.
#' @param excluded_content_descriptors A vector of content descriptors to
#' exclude in the response. Can be one or multiple codes as returned by
#' \code{\link{content_descriptors}}. Useful to filter explicit contents from
#' the search results. Defaults to \code{NULL}.
#'
#' @export
#' @rdname stf_search_apps
stf_suggest <- function(term,
                        country_code = "US",
                        language = "english",
                        steam_realm = 1L,
                        require_type = NULL,
                        excluded_content_descriptors = NULL,
                        links = FALSE) {
  assert_character(term, min.len = 1)
  assert_string(country_code)
  assert_string(language)
  assert_integerish(steam_realm)
  assert_character(require_type, null.ok = TRUE)
  assert_integerish(excluded_content_descriptors, lower = 1, upper = 5, null.ok = TRUE)
  assert_flag(links)

  excluded_content_descriptors <- webapi_params(list(
    excluded_content_descriptors = as.list(excluded_content_descriptors)
  ))

  params <- list(
    term = term,
    cc = country_code,
    l = language,
    realm = steam_realm,
    require_type = require_type,
    f = "jsonfull"
  )
  params <- c(params, excluded_content_descriptors)
  res <- request_storefront(
    api = store_api(),
    interface = "search",
    method = "suggest",
    params = params
  )

  if (!links) {
    res <- res[!lvapply(res, function(col) all(is_url(col)))]
  }

  as_data_frame(res)
}


#' @rdname stf_search_apps
#' @export
stf_search_store <- function(term,
                             country_code = "US",
                             language = "english",
                             links = FALSE) {
  assert_character(term, min.len = 1)
  assert_string(country_code)
  assert_string(language)
  assert_flag(links)

  params <- list(term = term, cc = country_code, l = language)
  res <- request_storefront(
    api = store_api(),
    interface = "api",
    method = "storesearch",
    params = params
  )$items

  if (!links) {
    res <- res[!lvapply(res, function(col) all(is_url(col)))]
  }

  as_data_frame(res)
}
