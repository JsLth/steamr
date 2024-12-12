#' Wishlist
#' @description
#' Get wishlist details of a user.
#'
#' @inheritParams common
#'
#' @export
#'
#' @returns A list of apps where each field contains information about
#' name, reviews, release dates, platforms, screenshots, discounts,
#' tags, and compatibility.
#'
#' @examples
#' get_wishlist("76561198092541763")
#'
get_wishlist <- function(steamid) {
  check_string(steamid)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params()
  request_storefront(
    api = store_api(),
    interface = "wishlist",
    method = sprintf("profiles/%s/wishlistdata", steamid),
    params = params
  )
}
