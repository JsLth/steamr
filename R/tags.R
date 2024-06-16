#' Tags
#' @description
#' Most generally, retrieves a list of tags. \code{get_most_popular_tags}
#' fetches the most popular tags on Steam at the time. \code{get_frequent_tags}
#' is an authenticated function that fetches the authenticated user's most
#' popular tags. \code{get_recommended_tags} returns a list of tags recommended
#' by the Steam algorithm.
#'
#' @inheritParams get_items
#'
#' @returns A dataframe containing all tags sorted by their popularity
#'
#' @rdname tags
#' @export
#'
#' @examples
#' \dontrun{
#' get_most_popular_tags(language = "DE")
#'
#' # requires authentication
#' auth_credentials("username")
#' get_frequent_tags()
#' get_recommended_tags()
#' }
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


#' @rdname tags
#' @export
get_frequent_tags <- function() {
  check_authenticated()
  res <- request_storefront(
    api = store_api(),
    interface = "tagdata",
    method = "myfrequenttags"
  )
  as_data_frame(res)
}


#' @rdname tags
#' @export
get_recommended_tags <- function() {
  check_authenticated()
  res <- request_storefront(
    api = store_api(),
    interface = "tagdata",
    method = "recommendedtags"
  )
  as_data_frame(res)
}
