#' Get store items
#' @description
#' Get information about items in the store
#'
#' @param items Store item or list of store items as returned by
#' \code{\link{store_item}}.
#' @param language Language string of the returned information.
#' @param elanguage Language identifier of the returned information.
#' @param country_code ISO-2 country code as returned by
#' \code{\link{get_countries}}. The Steam store differs depending on the
#' country it is accessed from.
#' @param steam_realm Number describing the Steam realm. A value of 1
#' indicates Steam Global, 2 indicates Steam China, and 0 indicates Unknown.
#' @param include List of extra information to include. Can be one or several
#' of the following: \code{release}, \code{platforms},
#' \code{all_purchase_options}, \code{screenshots}, \code{trailers},
#' \code{ratings}, \code{tag_count}, \code{reviews}, \code{basic_info},
#' \code{supported_languages}, \code{full_description}, \code{included_items}
#' and \code{assets_without_overrides}. If \code{all}, returns all information.
#' @param apply_user_filters Unknown.
#'
#' @returns A dataframe containing the requested information on the input
#' IDs.
#'
#' @export
#' @family store
#'
#' @examples
#' \dontrun{
#' ids <- list(
#'   store_item(268770),
#'   store_item(1182620),
#'   store_item(creatorid = 10)
#' )
#'
#' # request basic item info
#' get_items(ids)
#'
#' # request info in german language
#' get_items(ids, language = "german")
#'
#' # request info for the swedish store
#' get_items(ids, country_code = "SE")
#'
#' # request info on operating systems
#' get_items(ids, include = "platforms")
#'
#' # request all info
#' get_items(ids, include = "all")
#' }
#'
get_items <- function(items,
                      language = "english",
                      elanguage = NULL,
                      country_code = "US",
                      steam_realm = 1L,
                      include = NULL,
                      apply_user_filters = FALSE) {
  items <- lapply(items, store_item)
  context <- store_browse_context(
    language = language,
    elanguage = elanguage,
    country_code = country_code,
    steam_realm = steam_realm
  )
  data_request <- store_browse_item_data_request(
    include = include,
    apply_user_filters = apply_user_filters
  )
  input_json <- jsonlite::toJSON(
    list(
      ids = items,
      context = context,
      data_request = data_request
    ),
    auto_unbox = TRUE,
    force = TRUE
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


#' Get store categories
#' @description
#' Get a list of Steam store categories.
#'
#' @inheritParams get_items
#' @returns A dataframe containing names and information on all
#' store categories.
#'
#' @export
#' @family store
#'
#' @examples
#' \dontrun{
#' get_store_categories(language = "swedish")
#' }
get_store_categories <- function(language = "english", elanguage = NULL) {
  check_string(language, null = TRUE)
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


#' Get hardware items
#' @description
#' Get information about items that require physical shipping.
#'
#' @param items Numeric vector of package IDs or a list of store item objects
#' as returned by \code{\link{store_items}(..., type = "packageid")}.
#' @inheritParams get_items
#'
#' @returns A dataframe containing information on hardware items.
#'
#' @export
#' @family store
#'
#' @examples
#' \dontrun{
#'   get_hardware_items(c(354231, 1628580))
#'   get_hardware_items(store_items(c(354231, 1628580), type = "packageid"))
#' }
#'
get_hardware_items <- function(items,
                               language = "english",
                               elanguage = NULL,
                               country_code = "US",
                               steam_realm = 1L) {
  if (is.list(items)) items <- unlist(items, use.names = FALSE)
  context <- store_browse_context(
    language = language,
    elanguage = elanguage,
    country_code = country_code,
    steam_realm = steam_realm
  )

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


#' Define a store item
#' @description
#' Create an object describing a store item by ID. Can be an application,
#' package, bundle, tag, creator or hub category.
#'
#' \code{store_item} returns a single ID object, but allows the passing
#' of varying ID types. \code{store_items} returns a list of ID objects, but
#' is restricted to a single ID type.
#'
#' @param appid Application ID
#' @param packageid Package ID
#' @param bundleid Bundle ID
#' @param tagid Tag ID
#' @param creatorid Creator ID
#' @param hubcategoryid Hub category ID
#'
#' @returns An object of class \code{StoreItemID}.
#'
#' @export
#' @family store
#'
#' @examples
#' store_item(10)
#'
#' \dontrun{
#' ids <- list(
#'   store_item(268770),
#'   store_item(1182620),
#'   store_item(creatorid = 10)
#' )
#'
#' get_items(ids)
#' }
store_item <- function(appid = NULL,
                       packageid = NULL,
                       bundleid = NULL,
                       tagid = NULL,
                       creatorid = NULL,
                       hubcategoryid = NULL) {
  check_specified(exact = TRUE)
  item <- drop_null(as.list(environment()))

  if (is_store_item(item[[1]])) {
    return(item[[1]])
  }

  check_integerish(unlist(item), null = TRUE)
  class(item) <- "StoreItemID"
  item
}


#' @param items Numeric vector of Steam IDs. Will be converted to
#' \code{store_item}s. If a list is passed, will return \code{items} as-is.
#' @param type ID type of all items to be returned. Must be one of the
#' argument names of \code{store_item}.
#' @rdname store_item
#' @export
store_items <- function(items, type = NULL) {
  if (is_store_item(items)) {
    return(list(items))
  } else if (is.list(items)) {
    return(items)
  }

  type <- type %||% "appid"
  type <- match.arg(type, names(formals(store_item)))
  lapply(items, function(x) {
    arg <- list(x)
    names(arg) <- type
    box(do.call(store_item, arg))
  })
}


is_store_item <- function(x) {
  inherits(x, "StoreItemID")
}


#' @export
print.StoreItemID <- function(x, ...) {
  cat(sprintf("<StoreItem>\n  %s: %s", names(x), x))
  invisible(x)
}


store_browse_item_data_request <- function(include,
                                           apply_user_filters = FALSE,
                                           ...) {
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


store_browse_context <- function(language = NULL,
                                 elanguage = NULL,
                                 country_code = NULL,
                                 steam_realm = NULL,
                                 ...) {
  check_string(language)
  check_integerish(elanguage, null = TRUE)
  check_string(country_code)
  check_integerish(steam_realm)

  obj <- as.list(environment())
  class(obj) <- "StoreBrowseContext"
  drop_null(obj) %empty% NULL
}
