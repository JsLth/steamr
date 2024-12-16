#' @section Global options:
#'
#' \code{steamr} defines a couple of global options that can be used to control
#' the behavior of Steam requests and authentication.
#'
#' \describe{
#'  \item{\code{steamr_persistent}}{Specifies the default value of the
#'  \code{persistent} argument of \code{\link{auth_credentials}} and
#'  \code{\link{auth_qr}}. If \code{TRUE}, caches authentication information
#'  on disk to stay authenticated even between sessions. Note that storing the
#'  access token on disk may result in security risks. Defaults to
#'  \code{FALSE}.}
#'  \item{\code{steamr_debug}}{If \code{TRUE}, prints the request method and
#'  URL with each request. Defaults to \code{FALSE}.}
#'  \item{\code{steamr_do_auth}}{If \code{FALSE}, does not use authentication
#'  information, even if available (relating to both key and login).}
#'  \item{\code{steamr_throttle}}{If \code{TRUE}, throttles storefront requests
#'  to 300 requests per 5 minutes. The Web API and steamcmd.net do not need
#'  rate limits (to my knowledge). SteamSpy is always rate-limited at 60 requests
#'  per minute which cannot be turned off. Defaults to \code{TRUE}.}
#'  \item{\code{steamr_max_tries}}{Specifies the number of times a failing
#'  request is retried before throwing an error. Defaults to 3.}
#'  \item{\code{steamr_max_reqs}}{Specifies the number of allowed subsequent
#'  paginated requests. If \code{steamr_max_reqs = 3}, paginated functions
#'  can iterate through a maximum of 3 pages before returning the results.
#'  Defaults to \code{Inf}.}
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
