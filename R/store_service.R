#' Get tags
#' @description
#' Get a list of whitelisted and localized tags sorted by their popularity.
#'
#' @inheritParams get_items
#'
#' @returns A dataframe containing all tags sorted by their popularity
#'
#' @export
#' @family store
#'
#' @examples
#' \dontrun{
#' get_most_popular_tags(language = "DE")
#' }
#'
get_most_popular_tags <- function(language = "english") {
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IStoreService",
    method = "GetMostPopularTags",
    version = "v1",
    params = params
  )
  as_data_frame(res$response$tags)
}
