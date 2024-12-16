#' Categories
#' @description
#' Retrieve a list of available store categories and genres. Note that
#' categories and store categories are inherently different, although their
#' precise definition is unknown. Categories seem to be aimed at marketing
#' while store categories are used for search filtering purposes.
#'
#' @inheritParams common
#' @param extra  Unknown.
#' @param trailer Unknown.
#'
#' @returns \itemize{
#'  \item{\code{get_categories}: A named vector where the names are the
#'  categories and the values are their human-readable descriptions.}
#'
#'  \item{\code{get_store_categories}: A dataframe containing the internal
#'  and display names, category type, categoryID, image URL, and whether
#'  the categories are shown in the search.}
#'
#'  \item{\code{get_genres}: An unnamed vector of genre names.}
#' }
#'
#' @export
#'
#' @seealso
#'
#' \code{\link{get_apps_in_category}}
#'
#' \code{\link{get_apps_in_genre}}
#'
#' \code{\link{wba_store_items}}
#'
#' @examples
#' \dontrun{
#' # categories in swedish
#' get_categories(language = "swedish")
#'
#' # these are also categories, but very different ones!
#' get_store_categories()
#'
#' # get all genres
#' get_genres()
#' }
get_categories <- function(language = "english",
                           country_code = "US",
                           extra = NULL,
                           trailer = NULL) {
  check_string(language)
  check_string(country_code)
  check_string(extra, null = TRUE)
  check_string(trailer, null = TRUE)

  params <- .make_params(
    l = language,
    cc = country_code,
    extra = extra,
    trailer = trailer
  )
  res <- request_webapi(
    api = store_api(),
    interface = "api",
    method = "featuredcategories",
    params = params
  )
  res$status <- NULL
  cat <- cvapply(res, "[[", "id", use_names = FALSE)
  nm <- cvapply(res, "[[", "name", use_names = FALSE)
  names(nm) <- cat
  nm[!duplicated(nm)]
}


#' @rdname get_categories
#' @export
get_genres <- function(language = "english", country_code = "US") {
  check_string(language)
  check_string(country_code)

  params <- .make_params(l = language, cc = country_code)
  res <- request_webapi(
    api = store_api(),
    interface = "api",
    method = "getgenrelist",
    params = params
  )$genres
  res$name
}


#' @rdname get_categories
#' @export
get_store_categories <- function(language = "english", elanguage = NULL) {
  check_string(language)
  check_integerish(elanguage, null = TRUE)

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IStoreBrowseService",
    method = "GetStoreCategories",
    version = "v1",
    params = params
  )
  as_data_frame(res$response$categories)
}
