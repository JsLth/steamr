#' Get store items
#' @description
#' Get information about items in the store. \code{get_hardware_items} more
#' specifically requests special information about items that require physical
#' shipping.
#'
#' @param items A dataframe, list, or vector containing item IDs. If a vector,
#' interprets it as appIDs. If a list or character vector is passed, each
#' column can refer to a different type of item ID. Available types are
#' \code{appid}, \code{packageid}, \code{bundleid}, \code{tagid},
#' \code{creatorid}, and \code{hubcategoryid}. In this way, multiple items
#' with multiple ID types can be specified. For \code{get_hardware_items},
#' only vectors are allowed and they are always interpreted as packageIDs.
#' @param context Object of class
#' \code{\link[=store_context]{StoreBrowseContext}} that specifies the
#' geographic context from which to access the store. Defaults to a global
#' Steam realm (\code{steam_realm = 1}), English as the store language
#' (\code{language = "english"}) and the USA as the access country
#' (\code{country_code = "US"}).
#' @param data_request Object of class
#' \code{\link[=store_data_request]{StoreBrowseDataRequest}} that specifies
#' the additional information to be included. Defaults to basic information.
#'
#' @returns A dataframe containing the requested information on the input
#' IDs.
#'
#' @export
#' @family store
#'
#' @examples
#' # if just an ID is passed, it is assumed to be an appID
#' get_items(10)
#'
#' # by passing a dataframe, you can pass multiple types of ID at a time
#' ids <- data.frame(
#'   appid = c(10, NA, NA),
#'   creatorid = c(NA, 10, NA),
#'   packageid = c(NA, NA, 354231)
#' )
#'
#' # request basic item info
#' get_items(ids)
#'
#' # request info in german language
#' get_items(ids, context = store_context(language = "german"))
#'
#' # request info for the swedish store
#' get_items(ids, context = store_context(country_code = "SE"))
#'
#' # request info on operating systems
#' get_items(ids, data_request = store_data_request(include = "platforms"))
#'
#' # request special info on hardware
#' get_items(ids, data_request = store_data_request(include = "all"))
get_items <- function(items, context = NULL, data_request = NULL) {
  check_class(context, "StoreBrowseContext", null = TRUE)
  check_class(data_request, "StoreBrowseDataRequest", null = TRUE)

  if (!is.list(items)) {
    items <- data.frame(appid = items)
  }

  items <- .mapply(store_item, items, NULL)
  context <- context %||% store_context()
  data_request <- data_request %||% store_data_request()
  input_json <- .make_input_json(
    ids = items,
    context = context,
    data_request = data_request
  )

  params <- .make_params(input_json = input_json)
  res <- request_webapi(
    api = public_api(),
    interface = "IStoreBrowseService",
    method = "GetItems",
    version = "v1",
    params = params
  )
  as_data_frame(fix_steam_bool(res$response$store_items))
}


#' @rdname get_items
#' @export
#'
#' @examples
#' get_hardware_items(c(354231, 1628580))
get_hardware_items <- function(items, context = NULL) {
  if (is.list(items)) items <- unlist(items, use.names = FALSE)
  context <- context %||% store_context()

  params <- .make_params(packageid = as.list(items), context = context)
  res <- request_webapi(
    api = public_api(),
    interface = "IStoreBrowseService",
    method = "GetHardwareItems",
    version = "v1",
    params = params
  )
  as_data_frame(res$response$details)
}


store_item <- function(appid = NULL,
                       packageid = NULL,
                       bundleid = NULL,
                       tagid = NULL,
                       creatorid = NULL,
                       hubcategoryid = NULL) {
  item <- drop_na(drop_null(as.list(environment())))

  if (inherits(item[[1]], "StoreItemID")) {
    return(item[[1]])
  }

  check_integerish(unlist(item), null = TRUE)
  class(item) <- "StoreItemID"
  item
}


#' Steam utility objects
#' @description
#' Create utility objects commonly used in the Steam web API.
#'
#' \itemize{
#'  \item{\code{store_data_request} specifies which extra information should
#'  be included in the API response. It is usually supplied over an argument
#'  \code{data_request}.}
#'  \item{\code{store_context} specifies the geographic and language context
#'  from which to access the Steam store. It is usually supplied over an
#'  argument \code{context}.}
#' }
#'
#' @param include List of extra information to include. Can be one or several
#' of the following: \code{release}, \code{platforms},
#' \code{all_purchase_options}, \code{screenshots}, \code{trailers},
#' \code{ratings}, \code{tag_count}, \code{reviews}, \code{basic_info},
#' \code{supported_languages}, \code{full_description}, \code{included_items}
#' and \code{assets_without_overrides}. If \code{all}, returns all information.
#' @param apply_user_filters Unknown.
#' @returns \code{store_data_request()} returns a list of class
#' \code{StoreBrowseItemDataRequest}. \code{store_context()} returns a list of
#' class \code{StoreBrowseContext}.
store_data_request <- function(include = NULL, apply_user_filters = FALSE) {
  info <- c(
    "assets", "release", "platforms", "all_purchase_options",
    "screenshots", "trailers", "ratings", "tag_count", "reviews",
    "basic_info", "supported_languages", "full_description",
    "included_items", "assets_without_overrides"
  )

  if (identical(include, "all")) {
    include <- info
  }

  obj <- lapply(info, "%in%", include)
  names(obj) <- paste0("include_", info)

  obj <- c(obj, apply_user_filters = apply_user_filters)
  class(obj) <- "StoreBrowseItemDataRequest"
  drop_false(drop_null(obj)) %empty% NULL
}


#' @rdname store_data_request
#' @param language ISO 639-1 language code all tokenized strings should be
#' returned in. Not all tokenized strings have a translation
#' for all languages. If no translation is available, defaults to English.
#' @param elanguage Numeric code representing the store language. A list
#' of language codes and their corresponding languages is defined in
#' \code{\link{ELanguage}}.
#' @param country_code ISO 3166 country code representing the country from
#' which to view the Steam store.
#' @param steam_realm Number describing the Steam realm. A value of 1
#' indicates Steam Global, 2 indicates Steam China, and 0 indicates Unknown.
#' @export
store_context <- function(language = "english",
                          elanguage = NULL,
                          country_code = "US",
                          steam_realm = 1L) {
  check_string(language)
  check_integerish(elanguage, null = TRUE)
  check_string(country_code)
  check_integerish(steam_realm)

  obj <- as.list(environment())
  class(obj) <- "StoreBrowseContext"
  drop_null(obj) %empty% NULL
}
