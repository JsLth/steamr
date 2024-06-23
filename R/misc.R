#' Game servers
#' @description
#' Retrieve a list of Steam's game servers.
#'
#' @export
#'
get_servers <- function(filter = NULL, limit = NULL) {
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IGameServersService",
    method = "GetServerList",
    version = "v1",
    params = params
  )$response$servers
  as_data_frame(res)
}


#' Game groups
#' @description
#' Retrieve details about the game groups (or community hubs) associated with an
#' appID.
#'
#' @inheritParams common
#'
#' @returns A dataframe containing information about the game group
#' associated with an application. Notably, the columns \code{clanAccountID}
#' and \code{clanSteamIDString} contain the clanID and gid, respectively.
#' Each row represents one appID.
#'
#' @export
#'
#' @seealso
#' \code{\link{resolve_vanity_url}}
#'
#' \code{\link{parse_steam64}}
#'
#' @examples
#' \dontrun{
#' # get the community hubs of Counter Strike and Team Fortress
#' get_game_group(c(10, 440))
#' }
get_game_group <- function(appids) {
  check_number(appids)

  res <- lapply(appids, function(appid) {
    res <- request_storefront(
      api = comm_api(),
      interface = paste0("ogg/", appid),
      method = "ajaxgetvanityandclanid"
    )
    as_data_frame(do.call(cbind.data.frame, res))
  })
  bind_rows(res)
}
