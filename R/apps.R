#' App list
#' @description
#' Get a list of applications in the Steam store, either a complete list
#' or the top games by concurrent players, all-time players, or top releases.
#'
#' @returns A dataframe containing the appID and name of each application in
#' the Steam store. The app list is cached on disk for a short while until
#' going stale.
#'
#' @evalRd auth_table(list("wba_apps", key = FALSE, login = FALSE))
#'
#' @export
#'
#' @seealso
#'
#' \code{\link{steamspy}} for a similar approach by the SteamSpy API
#'
#' \code{\link{stf_search_apps}} to filter apps using search strings
#'
#' \code{\link{wba_query}} to filter apps using a filter query
#'
#' @examples
#' \donttest{# return all apps on Steam
#' wba_apps()}
wba_apps <- function() {
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamApps",
    method = "GetAppList",
    version = "v2",
    cache = TRUE
  )$applist$apps

  as_data_frame(res)
}
