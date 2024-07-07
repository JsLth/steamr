#' Discovery queue
#' @description
#' Retrieve suggested and skipped apps on the discovery queue and its
#' settings.
#'
#' @inheritParams common
#' @param queue_type Integer indicating the queue type. Possible values can
#' be retrieved using \code{node_enum("StoreDiscoveryQueueType")}.
#' @param rebuild_queue If \code{TRUE}, rebuilds the queue. Otherwise,
#' takes the previously generated one. Rebuilding the queue can lead to
#' error codes 2. In this case, retrying helps.
#' @param settings_changed Whether to overwrite the settings in
#' \code{get_discovery_settings} with the settings passed to
#' \code{get_discovery_queue}.
#' @param os_win If \code{TRUE}, only includes apps for Windows.
#' @param os_mac If \code{TRUE}, only includes apps for Mac OS.
#' @param os_linux If \code{TRUE}, only includes apps for Linux.
#' @param full_controller_support If \code{TRUE}, only includes apps with
#' full controller support.
#' @param native_steam_controller If \code{TRUE}, only includes apps with
#' native Steam controller support.
#' @param include_coming_soon If \code{TRUE}, includes apps that are coming
#' soon.
#' @param excluded_tagids A vector of tagIDs to exclude.
#' @param exclude_early_access,exclude_videos,exclude_software,exclude_dlc,exclude_soundtracks
#' If \code{TRUE}, excludes early access apps, videos, software, DLCs, or
#' soundtracks.
#' @param featured_tagids A vector of tagIDs that must be present in the
#' output appIDs.
#' @param rebuild_queue_if_stale If \code{TRUE}, rebuilds the discovery queue
#' if it is stale. It is a mystery what stale means.
#' @param ignore_user_preferences If \code{TRUE}, ignores user preferences.
#' @param no_experimental_results Unknown.
#'
#' @returns \describe{
#'  \item{\code{get_discovery_queue}}{A vector of appIDs in the requested
#'  discovery queue.}
#'  \item{\code{get_discovery_settings}}{A list of current discovery queue
#'  settings.}
#'  \item{\code{get_discovery_skipped}}{A vector of appIDs skipped in the
#'  discovery queue.}
#' }
#'
#' @export
#'
#' @examples
#' # return discovery queue only containing DLCs on sale
#' get_discovery_queue(queue_type = 8) # 8 - DLCOnSale
#'
#' # return the best DLCs on sale in Germany
#' get_discovery_queue(queue_type = 8, country_code = "DE")
#'
#' \dontrun{
#' # overwrite discovery queue settings
#' # NOTE: this modifies account settings and should be used with care
#' get_discovery_queue(settings_changed = TRUE, exclude_dlc = TRUE)
#' }
get_discovery_queue <- function(queue_type = NULL,
                                country_code = "US",
                                rebuild_queue = FALSE,
                                settings_changed = FALSE,
                                os_win = FALSE,
                                os_mac = FALSE,
                                os_linux = FALSE,
                                full_controller_support = FALSE,
                                native_steam_controller = FALSE,
                                include_coming_soon = FALSE,
                                excluded_tagids = NULL,
                                exclude_early_access = FALSE,
                                exclude_videos = FALSE,
                                exclude_software = FALSE,
                                exclude_dlc = FALSE,
                                exclude_soundtracks = FALSE,
                                featured_tagids = NULL,
                                rebuild_queue_if_stale = FALSE,
                                ignore_user_preferences = FALSE,
                                no_experimental_results = FALSE) {
  check_authenticated()
  check_integerish(queue_type, null = TRUE)
  check_string(country_code)
  check_bool(rebuild_queue)
  check_bool(settings_changed)
  check_bool(rebuild_queue_if_stale)
  check_bool(ignore_user_preferences)
  check_bool(no_experimental_results)

  settings <- store_discovery_queue_settings(
    os_win = os_win,
    os_mac = os_mac,
    os_linux = os_linux,
    full_controller_support = full_controller_support,
    native_steam_controller = native_steam_controller,
    include_coming_soon = include_coming_soon,
    excluded_tagids = excluded_tagids,
    exclude_early_access = exclude_early_access,
    exclude_videos = exclude_videos,
    exclude_software = exclude_software,
    exclude_dlc = exclude_dlc,
    exclude_soundtracks = exclude_soundtracks,
    featured_tagids = featured_tagids
  )

  params <- .make_params(
    queue_type = queue_type,
    country_code = country_code,
    rebuild_queue = rebuild_queue,
    settings_changed = settings_changed,
    settings = do.call(.make_input_json, list(settings)),
    rebuild_queue_if_stale = rebuild_queue_if_stale,
    ignore_user_preferences = ignore_user_preferences,
    no_experimental_results = no_experimental_results
  )
  request_webapi(
    api = public_api(),
    interface = "IStoreService",
    method = "GetDiscoveryQueue",
    version = "v1",
    params = params
  )$response$appids
}


#' @rdname get_discovery_queue
#' @export
get_discovery_settings <- function(queue_type = NULL) {
  check_authenticated()
  check_integerish(queue_type, null = TRUE)

  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IStoreService",
    method = "GetDiscoveryQueueSettings",
    version = "v1",
    params = params
  )$response
}


#' @rdname get_discovery_queue
#' @export
get_discovery_skipped <- function(queue_type = NULL) {
  check_integerish(queue_type, null = TRUE)
  steamid <- get_auth("steamid")

  params <- .make_params(steamid = steamid, queue_type = queue_type)
  request_webapi(
    api = public_api(),
    interface = "IStoreService",
    method = "GetDiscoveryQueueSkippedApps",
    version = "v1",
    params = params
  )$response$appids
}


store_discovery_queue_settings <- function(os_win = FALSE,
                                           os_mac = FALSE,
                                           os_linux = FALSE,
                                           full_controller_support = FALSE,
                                           native_steam_controller = FALSE,
                                           include_coming_soon = FALSE,
                                           excluded_tagids = NULL,
                                           exclude_early_access = FALSE,
                                           exclude_videos = FALSE,
                                           exclude_software = FALSE,
                                           exclude_dlc = FALSE,
                                           exclude_soundtracks = FALSE,
                                           featured_tagids = NULL) {
  check_bool(os_win)
  check_bool(os_mac)
  check_bool(os_linux)
  check_bool(full_controller_support)
  check_bool(native_steam_controller)
  check_bool(include_coming_soon)
  check_number(excluded_tagids, null = TRUE)
  check_bool(exclude_early_access)
  check_bool(exclude_videos)
  check_bool(exclude_software)
  check_bool(exclude_dlc)
  check_bool(exclude_soundtracks)
  check_number(featured_tagids, null = TRUE)

  obj <- as.list(environment())
  class(obj) <- "StoreDiscoveryQueueSettings"
  drop_null(drop_false(obj)) %empty% NULL
}
