#' Bundles and packages
#' @description
#' Resolve details of bundles and packages.
#'
#' @param bundleids A vector of up to 200 bundleIDs.
#' @param packageids A vector of up to 200 packageIDs. For
#' \code{package_details}, this vector can be of any length.
#' @inheritParams common
#'
#' @returns Both \code{resolve_bundles} and \code{resolve_packages} return
#' detailed information on a bundle or a package including prices,
#' release dates, compatibility information, discounts, and localization.
#'
#' @export
#'
#' @examples
#' # resolve Graveyard Keeper bundle
#' resolve_bundles(8077)
#'
#' # resolve OST from the Graveyard Keeper bundle
#' resolve_packages(295588)
#'
#' # get more details on OST
#' package_details(295588)
resolve_bundles <- function(bundleids,
                            country_code = "US",
                            language = "english") {
  check_number(bundleids)
  check_length(bundleids, ge = 1, le = 200)
  check_string(country_code)
  check_string(language)

  params <- .make_params(
    bundleids = bundleids,
    cc = country_code,
    l = language,
    key = FALSE,
    access_token = FALSE
  )
  res <- request_storefront(
    api = store_api(),
    interface = "actions",
    method = "ajaxresolvebundles",
    params = params
  )
  as_data_frame(res)
}


#' @rdname resolve_bundles
#' @export
resolve_packages <- function(packageids,
                             country_code = "US",
                             language = "english") {
  check_number(packageids)
  check_length(packageids, ge = 0, le = 200)
  check_string(country_code)
  check_string(language)

  params <- .make_params(
    packageids = packageids,
    cc = country_code,
    l = language,
    key = FALSE,
    access_token = FALSE
  )
  res <- request_storefront(
    api = store_api(),
    interface = "actions",
    method = "ajaxresolvepackages",
    params = params
  )
  as_data_frame(res)
}


package_details <- function(packageids,
                            country_code = "US",
                            language = "english") {
  check_number(packageids)
  check_string(country_code)
  check_string(language)

  res <- lapply(packageids, function(pid) {
    params <- .make_params(
      packageids = pid,
      cc = country_code,
      l = language,
      key = FALSE,
      access_token = FALSE
    )
    res <- request_storefront(
      api = store_api(),
      interface = "api",
      method = "packagedetails",
      params = params
    )
    res <- lapply(res, "[[", "data")
    rbind_list(res)
  })
  names(res) <- packageids
  bind_rows(res, .id = "packageid")
}
