#' Search apps
#' @description
#' Simple Steam store queries that return the best matches to a given search
#' query. Uses Steam's unofficial API.
#'
#' Steam provides different endpoints that essentially do the same, but
#' in a slightly different manner.
#' \itemize{
#'  \item{\code{search_apps} has less filtering capabilities, provides no
#'  context and returns a slim output.}
#'  \item{\code{suggest} provides many filtering capabilities, provides context
#'  and returns a slightly more detailed output.}
#'  \item{\code{store_search} has no filtering capabilities, provides context
#'  and returns a detailed output.}
#' }
#'
#' @param term Search term used to query the Steam store.
#' @param links Whether to include links to icon and logo in the output.
#' Defaults to \code{FALSE} to avoid littering the output.
#'
#' @returns A dataframe containing the appid and application name and
#' (optionally) links to application icon and logo.
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
#' \dontrun{
#' # returns a single result
#' search_apps("team fortress")
#'
#' # returns multiple results
#' search_apps("team")
#'
#' # returns the first 10 results
#' suggest("team")
#'
#' # returns the first 10 most fitting DLCs
#' suggest("team", require_type = "dlc")
#'
#' # translates results to German and converts dollar to euro
#' suggest("team", language = "german", country_code = "DE")
#'
#' # provides detailed data
#' store_search("team")
#' }
search_apps <- function(term, links = FALSE) {
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


#' @inheritParams get_items
#' @param require_type Which application types should be returned? Can be
#' one of \code{bundle}, \code{game}, \code{demo}, \code{mod} \code{dlc},
#' \code{software}, \code{video}, \code{hardware}, \code{series},
#' and \code{music}.
#' @param use_store_query Unknown parameter.
#' @param use_search_spellcheck Whether search terms should be spellchecked.
#' @param search_creators_and_tags Whether to search for the search term in
#' creator names and tags.
#'
#' @export
#' @rdname search_apps
suggest <- function(term,
                    country_code = "US",
                    language = "english",
                    steam_realm = 1L,
                    require_type = NULL,
                    use_store_query = TRUE,
                    use_search_spellcheck = TRUE,
                    search_creators_and_tags = TRUE,
                    links = FALSE) {
  params <- list(
    term = term,
    cc = country_code,
    l = language,
    realm = steam_realm,
    require_type = require_type,
    use_store_query = use_store_query,
    use_search_spellcheck = use_search_spellcheck,
    search_creators_and_tags = search_creators_and_tags,
    f = "jsonfull"
  )
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


#' @inheritParams get_items
#' @rdname search_apps
#' @export
store_search <- function(term, country_code = "US", language = "english") {
  params <- list(term = term, cc = country_code, l = language)
  res <- request_storefront(
    api = store_api(),
    interface = "api",
    method = "storesearch",
    params = params
  )
  as_data_frame(res$items)
}
