#' Tags
#' @description
#' Retrieves user-provided tags in the Steam store with different emphases
#' and orders. \code{get_tags} and \code{get_most_popular_tags} operate on tags
#' in the entire Steam store. \code{get_frequent_tags} and
#' \code{get_recommended_tags} are specifically based on the authenticated
#' user.
#'
#' @inheritParams get_items
#' @param hash Hash included in the \code{hash} attribute of a previous
#' function call. If \code{hash} matches with the hash of the requested tags,
#' then a \code{DuplicateRequest} error is thrown.
#'
#' @returns All functions return a dataframe containing the tagIDs and tag
#' names. \code{get_tags} returns all known tags sorted by their tagID.
#' \code{get_most_popular_tags} returns the most popular tags sorted by
#' their popularity. \code{get_frequent_tags} returns the most frequent tags
#' by the authenticated user sorted by the count in the \code{count} column.
#' \code{get_recommended_tags} returns tags recommended by the Steam algorithm
#' with a secret sorting.
#'
#' @rdname tags
#' @export
#'
#' @examples
#' \dontrun{
#' # get all tags
#' get_tags()
#'
#' # get the most popular tags translated to German
#' get_most_popular_tags(language = "DE")
#'
#' # requires authentication
#' auth_credentials("username")
#' get_frequent_tags()
#' get_recommended_tags()
#' }
get_tags <- function(language = "english", hash = NULL) {
  check_string(language)
  check_string(hash, null = TRUE)

  params <- .make_params(language = language, have_version_hash = hash)
  res <- request_webapi(
    api = public_api(),
    interface = "IStoreService",
    method = "GetTagList",
    version = "v1",
    params = params
  )$response
  hash <- res$version_hash
  res <- as_data_frame(res$tags)
  attr(res, "hash") <- hash
  res
}


#' @rdname tags
#' @export
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
