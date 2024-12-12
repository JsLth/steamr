#' Session info
#' @description
#' Retrieve login and client info of the authenticated session.
#'
#' @param client_instanceid InstanceID of an authenticated session. Can be
#' retrieved with \code{get_login_info}.
#'
#' @returns \describe{
#'  \item{\code{get_logon_info}}{A dataframe where each row is an authenticated
#'  session. Contains columns on the client instanceID, protocol version,
#'  operating system, machine name, device type, and Steam realm.}
#'  \item{\code{get_client_info}}{A dataframe with a single row containing
#'  information on package version, operating system and client details.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # authentication needed
#' auth_qr()
#'
#' # get info about the session
#' info <- get_logon_info()
#' client <- info$client_instanceid
#'
#' # get details on an instanceid
#' get_client_info(client)
#' }
get_logon_info <- function() {
  check_authenticated()
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IClientCommService",
    method = "GetAllClientLogonInfo",
    version = "v1",
    params = params
  )$response
  as_data_frame(res$sessions)
}


#' @rdname get_logon_info
#' @export
get_client_info <- function(client_instanceid) {
  check_authenticated()
  check_number(client_instanceid)
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IClientCommService",
    method = "GetClientInfo",
    version = "v1",
    params = params
  )$response
  as_data_frame(res$client_info)
}
