#' Frontpage apps
#' @description
#' These functions return apps as they are currently shown on the (genre)
#' dashboard of the store front page (\url{https://store.steampowered.com/}).
#'
#' \itemize{
#'  \item{\code{stf_frontpage} returns the featured apps on the main store
#'  front page.}
#'  \item{\code{stf_frontpage_category} returns the featured apps on a
#'  category-specific front page (e.g. new releases or top sellers).}
#'  \item{\code{stf_frontpage_genre} returns the featured apps on a
#'  genre-specific front page (e.g. action or racing).}
#' }
#'
#' @param category A category of store items as shown on the front page of the
#' store. Sensible values can be retrieved using \code{\link{get_categories}}.
#' @param genre A genre of store items as shown on the front page of the store.
#' Sensible values can be retrieved using \code{\link{get_genres}}.
#' @inheritParams store_context
#'
#' @returns \code{stf_frontpage} returns a grouped dataframe where the
#' \code{category} column represents the category of apps on the frontpage,
#' e.g. \code{"cat_spotlight"} contains the current special offers.
#'
#' \code{stf_frontpage_genre} and \code{stf_frontpage_category} return a
#' dataframe containing the tab category, an unknown type, and the ID of
#' the featured store item.
#'
#' @evalRd auth_table(
#'   list("stf_frontpage", key = FALSE, login = FALSE),
#'   list("stf_frontpage_category", key = FALSE, login = FALSE),
#'   list("stf_frontpage_genre", key = FALSE, login = FALSE)
#' )
#'
#' @export
#'
#' @examples
#' \donttest{# get details about the daily deal in the swedish store
#' fp <- stf_frontpage(country_code = "SE")
#' fp[fp$category %in% "cat_dailydeal", ]
#'
#' # get details about the top racing games
#' fp <- stf_frontpage_genre("racing")
#' fp <- fp[fp$tab %in% "topsellers", ]
#' wba_store_items(fp$id)}
stf_frontpage <- function(language = "english", country_code = "US", trailer = NULL, extra = NULL) {
  assert_string(language)
  assert_string(country_code)

  params <- .make_params(
    cc = country_code,
    l = language,
    trailer = trailer,
    extra = extra,
    key = FALSE
  )

  res <- request_storefront(
    api = store_api(),
    interface = "api",
    method = "featuredcategories",
    params = params
  )

  res <- lapply(res, function(x) {
    if (is.list(x) && !is.null(x$items)) {
      cbind(category = x$id, desc = x$name, x$items)
    }
  })
  as_data_frame(rbind_list(drop_null(res)))
}


#' @rdname stf_frontpage
#' @export
stf_frontpage_category <- function(category,
                                   language = "english",
                                   country_code = "US") {
  assert_string(category)
  assert_string(language)
  assert_string(country_code)

  params <- .make_params(
    category = category,
    cc = country_code,
    l = language,
    key = FALSE
  )
  res <- request_storefront(
    api = store_api(),
    interface = "api",
    method = "getappsincategory",
    params = params
  )

  if (!identical(res$status, 1L)) {
    stop(sprintf("Request failed with status code %s", res$status %||% "Unknown error"))
  }

  if (is.null(res$tabs)) {
    stop(sprintf("Response is empty. Category \"%s\" does not seem to exist", category))
  }

  res <- bind_rows(lapply(res$tabs, "[[", "items"), .id = "tab")
  as_data_frame(res)
}


#' @rdname stf_frontpage
#' @export
stf_frontpage_genre <- function(genre, language = "english", country_code = "US") {
  assert_string(genre)
  assert_string(language)
  assert_string(country_code)

  params <- .make_params(key = FALSE)
  res <- request_storefront(
    api = store_api(),
    interface = "api",
    method = "getappsingenre",
    params = params
  )$tabs
  res <- bind_rows(lapply(res, "[[", "items"), .id = "tab")
  as_data_frame(res)
}
