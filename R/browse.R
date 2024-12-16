#' Get store items
#' @description
#' Get information about items in the store. \code{get_hardware_items} more
#' specifically requests special information about items that require physical
#' shipping.
#'
#' @param items A dataframe, list, or character vector containing item IDs. If
#' a vector, interprets it as appIDs. If a list or dataframe is passed, each
#' column or list element can refer to a different type of item ID. Available
#' types are \code{appid}, \code{packageid}, \code{bundleid}, \code{tagid},
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
#' the additional information to be included. Defaults to no extra info.
#'
#' @returns A dataframe containing the requested information on the input
#' IDs.
#'
#' @export
#'
#' @evalRd auth_table(
#'   list("wba_store_items", key = FALSE, login = FALSE),
#'   list("wba_hardware_items", key = FALSE, login = FALSE)
#' )
#'
#' @examples
#' \donttest{# if just an ID is passed, it is assumed to be an appID
#' wba_store_items(10)
#'
#' # by passing a dataframe, you can pass multiple types of ID at a time
#' ids <- data.frame(
#'   appid = c(10, NA, NA),
#'   creatorid = c(NA, 10, NA),
#'   packageid = c(NA, NA, 354231)
#' )
#'
#' # request basic item info
#' wba_store_items(ids)
#'
#' # request info in german language
#' wba_store_items(ids, context = store_context(language = "german"))
#'
#' # request info for the swedish store
#' wba_store_items(ids, context = store_context(country_code = "SE"))
#'
#' # request info on operating systems
#' wba_store_items(ids, data_request = store_data_request("platforms"))
#'
#' # request special info on hardware
#' wba_hardware_items(c(354231, 1628580))}
wba_store_items <- function(items,
                            context = store_context(),
                            data_request = store_data_request()) {
  assert_class(context, "StoreBrowseContext")
  assert_class(data_request, "StoreBrowseItemDataRequest")

  if (!is.list(items)) {
    items <- list(appid = items)
  }

  items <- .mapply(store_item, items, NULL)
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


#' @rdname wba_store_items
#' @export
wba_hardware_items <- function(items, context = store_context()) {
  assert_class(context, "StoreBrowseContext", null = TRUE)
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
                       hubcategoryid = NULL,
                       ...) {
  if (...length()) {
    abort("Unknown store item type: {.val {...names()}}", call = NULL)
  }

  item <- drop_na(drop_null(as.list(environment())))
  item <- as.data.frame(item)

  if (!length(item)) {
    abort(c(
      "Missing or empty store items provided.",
      "i" = "Please make sure that each store item contains exactly one ID."
    ), call = NULL)
  }

  mpr <- !one_per_row(item)
  if (all(mpr)) {
    abort(c(
      "Argument `items` must contain exactly one ID per row.",
      "i" = "The problem occured in the following {cli::qty(sum(mpr))} row{?s}: {which(mpr)}"
    ), call = NULL)
  }

  if (inherits(item[[1]], "StoreItemID")) {
    return(item[[1]])
  }

  assert_integerish(unlist(item), null = TRUE)
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
#'
#' @seealso \code{\link{wba_store_items}}, \code{\link{wba_hardware_items}},
#' \code{\link{wba_charts_weekly}}, \code{\link{wba_query}},
#' \code{\link{wba_marketing_message}}
#'
#' @export
#'
#' @examples
#' \donttest{# Specify the geographic context
#' wba_store_items(440, context = store_context(language = "german", country_code = "DE"))
#'
#' # Specify the data to be returned
#' wba_store_items(440, data_request = store_data_request(c("screenshots", "trailers")))}
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

  obj <- drop_false(drop_null(obj))
  class(obj) <- c("StoreBrowseItemDataRequest", "steam_object")
  obj
}


#' @rdname store_data_request
#' @param language ISO 639-1 language code all tokenized strings should be
#' returned in. Not all tokenized strings have a translation
#' for all languages. If no translation is available, defaults to English.
#' @param elanguage Numeric code representing the store language. A list
#' of language codes and their corresponding languages is defined in
#' \code{\link{ELanguage}}.
#' @param country_code ISO 3166 country code representing the country from
#' which to view the Steam store. A list of Steam countries can be retrieved
#' with \code{\link{get_country_list}}.
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
  obj <- drop_null(obj)
  class(obj) <- c("StoreBrowseContext", "steam_object")
  obj
}


#' @export
print.steam_object <- function(x, ...) {
  header <- sprintf("<%s>", class(x)[[1]])
  if (length(x)) {
    body <- paste0("  ", names(x), ": ", x)
    cat(header, body, sep = "\n")
  } else {
    cat(header, sep = "\n")
  }
  invisible(x)
}
