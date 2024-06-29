#' Assets
#' @description
#' Query market assets and their prices. An asset is an item
#' from the Steam economy that can be traded. For more information about
#' assets, see the \href{https://partner.steamgames.com/doc/store/assets}{Steamworks documentation}.
#'
#' @inheritParams common
#' @param search_term Term to search for in the market item name.
#' @param sort_column Column to sort the response by.
#' @param sort_dir Direction of sorting if \code{sort_column} is not
#' \code{NULL}. Can be \code{asc} or \code{desc}.
#' @param search_descriptions Unknown.
#' @param currency ISO 4217 code to filter by currency. Defaults to all
#' available currencies.
#'
#' @returns
#' \describe{
#'  \item{\code{query_market_assets}A dataframe with each row representing
#'  one asset. The dataframe provides data on name, hash names, prices,
#'  the corresponding app, asset classIDs, and the output of
#'  \code{get_asset_info}.}
#'
#'  \item{\code{get_asset_prices}}{A dataframe containg information on assert
#'  prices. \code{name}, \code{date}, \code{class}, and \code{classid} provide
#'  metadata about assets. Optionally, tags are provided in \code{tags} and
#'  \code{tag_ids}. \code{class}, \code{tags}, and \code{tag_ids} are nested
#'  as dataframes or lists as they can contain values of arbitrary length.
#'  \code{prices.\*} contain the current prices for
#'  the currencies specified in \code{currency}. If an asset is on sale,
#'  \code{original_prices.\*} show the original prices.}
#'
#'  \item{\code{get_asset_info}}{A dataframe with each row representing
#'  asset info on each provided classID. \code{\*_url} columns show
#'  extensions to the URL \url{http://cdn.steamcommunity.com/economy/image/}.
#'  \code{name}, \code{market_hash_name} and \code{market_name} show various
#'  types of asset names. If an asset has received a fraud warning, the
#'  column \code{fraudwarnings} can contain their warning strings. The
#'  \code{descriptions} columns contain information about asset attributes,
#'  \code{actions} about action URLs, and \code{tags} about asset tags.}
#' }
#'
#' @export
#'
#' @references
#' \url{https://wiki.teamfortress.com/wiki/WebAPI/GetAssetPrices}
#'
#' \url{https://wiki.teamfortress.com/wiki/WebAPI/GetAssetClassInfo}
#'
#' @seealso
#' \code{\link{steamkit_enum}} to retrieve all currency codes
#'
#' \code{\link{query_loyalty_rewards}} for loyalty assets
#'
#' @examples
#' # show asset prices for Team Fortress and all available currencies
#' get_asset_prices(440)
#'
#' # show the same assets, but return only EUR
#' get_asset_prices(440, currency = "EUR")
#'
#' # a list of valid currency codes can be retrieved using
#' # steamkit_enum
#' steamkit_enum("CurrencyCode", type = "SteamLanguage")
#'
#' # get_asset_prices returns classIDs for each asset. These classIDs can
#' # be used to retrieve detailed information on an asset.
#' get_asset_class(730, c("5710093913", "5189384166"))
query_market_assets <- function(appid,
                                search_term = NULL,
                                sort_column = NULL,
                                sort_dir = NULL,
                                search_description = NULL,
                                paginate = TRUE) {
  check_number(appid)
  check_string(search_term, null = TRUE)
  check_string(sort_column, null = TRUE)
  check_string(sort_dir, null = TRUE)
  check_string(search_description, null = TRUE)

  params <- .make_params(
    appid = appid,
    query = search_term,
    sort_column = sort_column,
    sort_dir = sort_dir,
    search_description = search_description,
    start = 0,
    norender = 1,
    count = 100
  )

  res <- request_storefront(
    api = comm_api(),
    interface = "market",
    method = "search/render",
    params = params,
    paginate = if (paginate) "start"
  )

  if (paginate) {
    res <- lapply(res, "[[", "results")
    res <- rbind_list(res)
  } else {
    res <- res$results
  }

  as_data_frame(res)
}


#' @rdname query_market_assets
#' @export
get_asset_prices <- function(appid, currency = NULL, language = "english") {
  check_steam_key()
  check_number(appid)
  check_string(currency, null = TRUE)
  check_string(language)

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamEconomy",
    method = "GetAssetPrices",
    version = "v1",
    params = params
  )$result$assets
  res$date <- as.Date(gsub("/", "-", res$date))
  as_data_frame(res)
}


#' @rdname query_market_assets
#' @export
#' @param classids A vector of classIDs of the assets to retrieve.
#' @param instanceids A vector of instanceIDs corresponding to the
get_asset_info <- function(appid,
                           classids,
                           class_count = 1L,
                           language = "english",
                           instanceids = NULL) {
  check_steam_key()
  check_number(appid)
  check_number(classids)
  check_string(language)
  check_number(instanceids, null = TRUE)

  n <- length(classids)
  check_length(instanceids, ge = 0, le = n)
  class_count <- n

  params <- c(
    appid = appid,
    format_idN(classids, name = "classid"),
    class_count = class_count,
    language = language,
    format_idN(instanceids, name = "instanceid")
  )
  params <- do.call(.make_params, params)
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamEconomy",
    method = "GetAssetClassInfo",
    version = "v1",
    params = params
  )$result

  if ("error" %in% names(res)) {
    stop(sprintf("GetAssetClassInfo error: %s", res$error))
  }

  res$success <- NULL
  res <- rbind_list(lapply(res, bind_rows))
  res
}


#' @rdname query_market_assets
#' @export
get_price_history <- function(appid, market_hash_name) {
  check_authenticated()
  params <- .make_params(key = FALSE)
  res <- request_storefront(
    api = comm_api(),
    interface = "market",
    method = "pricehistory",
    params = params
  )

  prefix <- res$price_prefix
  suffix <- res$price_suffix
  res <- as.data.frame(res$prices)
  names(res) <- c("date", "price", "stock")
  res$date <- as.Date(res$date, tryFormats = "%b %d %Y 01: +0")
  res$price <- as.numeric(res$price)
  res$stock <- as.numeric(res$stock)
  attr(res, "prefix") <- prefix
  attr(res, "suffix") <- suffix
  as_data_frame(res)
}


get_item_histogram <- function(listingid, currency = 3L, language = "english", country_code = "US") {
  check_number(listingid)
  check_integerish(currency)
  check_string(language)
  check_string(country_code)

  params <- .make_params(
    item_nameid = listingid,
    currency = currency,
    language = language,
    country_code = country_code,
    norender = 1
  )

  res <- request_storefront(
    api = comm_api(),
    interface = "market",
    method = "itemordershistogram",
    params = params
  )
  res <- drop_empty(drop_null(res))
  as_data_frame(do.call(cbind.data.frame, res))
}


format_idN <- function(ids, name) {
  if (is.null(ids)) return()
  ids <- unname(as.list(ids))
  names(ids) <- paste0(name, seq_along(ids) - 1)
  ids
}
