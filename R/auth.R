#' Session authentication
#' @description
#' Initializes an authorized Steam session. Some functions in both the
#' Web API and storefront API require the session to be authenticated. This
#' concerns all API methods that access personal account information.
#' \code{auth_*} functions essentially sign in a user programmatically.
#'
#' @param username Account name of the user to be authenticated.
#' @param password A function that safely prompts the Steam password. Defaults
#' to \code{\link[askpass]{askpass}} which obfuscates the input. The password
#' is RSA encrypted before transmission to the Steam servers.
#' @param shared_secret A 5-character string representing the one-time Steam
#' Guard code needed to confirm the login. Note that the Steam Guard code
#' is regenerated every 30 seconds. If \code{shared_secret} is \code{NULL},
#' authenticates using two-factor authentication. Two-factor confirmation
#' requires the manual confirmation of the login request in the Steam Guard
#' mobile app.
#' @param persistent Whether to start a persistent or ephemeral session.
#' If the session is ephemeral (default), then it is reset when the R session
#' ends. If it is persistent, then the necessary cookies are stored in a
#' persistent cache in the file system and recovered when the package is loaded.
#' \code{auth_credentials} additionally requests a persistent session from the
#' Steam API indicating that the requested access token is persistent.
#' \code{auth_credentials} is thus potentially more persistent than
#' \code{auth_qr}.
#' @param friendly_name Name of the session. Used to track authorized devices
#' in Steam Guard.
#' @param details Arbitrary details about the device attempting to
#' authenticate. Will be shown in Steam Guard.
#' @returns An object of class \code{steam_auth_session} holding information
#' about vanity ID, Steam64 ID, client ID, request ID, and access token. The
#' object is also attached to the session. \code{auth_logout} returns
#' \code{NULL}, invisibly. \code{auth_history} returns a dataframe where each
#' row is a login attempt.
#'
#' @details
#' Internally, \code{auth_credentials} requests a public RSA key using the
#' \code{GetPasswordRSAPublicKey} method from the \code{IAuthenticationService}
#' interface. The public key is used to encrypt the entered password to safely
#' transfer it to Steam. An authenticated session is requested by querying
#' \code{BeginAuthSessionViaCredentials}. If a captcha is required, the
#' function aborts. If no Steam Guard code is provided, the function will
#' attempt to authenticate using 2 factor authentication. After confirming
#' the authentication, \code{auth_credentials} will redirect to all Steam
#' websites requiring authentication to set the necessary cookies.
#'
#' \code{auth_qr} follows a very similar process, but requests an authenticated
#' session using \code{BeginAuthSessionViaQR}. The resulting challenge URL
#' is converted to a QR code using \code{\link[qrcode]{qrcode}}. Scanning the
#' QR code with the Steam Guard mobile app automatically authenticates the
#' session.
#'
#' @evalRd auth_table(
#'   list("auth_credentials", key = FALSE, login = FALSE),
#'   list("auth_qr", key = FALSE, login = FALSE),
#'   list("auth_logout", key = FALSE, login = TRUE, note = "Ends login status"),
#'   list("auth_history", key = FALSE, login = TRUE)
#' )
#'
#' @note
#' Session authentication is only possible in interactive mode because it
#' requires the manual insertion of Steam guard codes, confirmation of
#' login requests in the mobile app, or the scanning of QR codes. All of these
#' actions are not suitable for batch processing.
#'
#' @name auth
#' @export
#'
#' @examples
#' \dontrun{
#' user <- "gabelogannewell"
#' auth_credentials(user)
#'
#' # use a different password method
#' auth_credentials(user, password = rstudioapi::askForPassword)
#'
#' # set a friendly name to identify this session
#' auth_credentials(user, friendly_name = "R session")
#'
#' # skip two-factor authentication by providing a Steam Guard code
#' auth_credentials(user, shared_secret = "XXXXX")
#'
#' # authenticating a persistent session survives restarts of R
#' auth_credentials(user, persistent = TRUE)
#' .rs.restartR() # restart R
#' is_authenticated() # returns TRUE
#'
#' # sign in using a QR code
#' auth_qr()
#'
#' # the new login attempts can be retrieved using `auth_history()`
#' auth_history()
#'
#' # end the authenticated session by revoking the refresh token
#' auth_logout()}
auth_credentials <- function(username,
                             password = openssl::askpass,
                             shared_secret = NULL,
                             persistent = getOption("steamr_persistent", FALSE),
                             friendly_name = "steamr",
                             details = "Login using credentials") {
  stopifnot(is.character(username))
  stopifnot(is.function(password))
  if (!interactive()) {
    abort("Session authentication is only possible in interactive mode.")
  }

  password <- password()
  if (is.null(password)) {
    return()
  }

  # request public key from web api
  params <- get_password_rsa_public_key(username)

  # encrypt password using public key
  epass <- encrypt_password(password, params$publickey_mod, params$publickey_exp)

  # request an authenticated session
  login <- begin_auth_session(
    device_friendly_name = friendly_name,
    account_name = username,
    encrypted_password = epass,
    encryption_timestamp = params$timestamp,
    device_details = details,
    persistence = as.logical(persistent)
  )

  # abort if captcha is needed
  check_captcha(login)

  # confirm authenticated session using 2FA
  poll <- handle_steam_guard(login, code = shared_secret)

  # get redirection urls
  transfer <- finalize_login(poll$refresh_token)

  # tell various steam pages they are now authenticated
  set_tokens(transfer)

  auth <- list(
    vanity = poll$account_name,
    steamid = login$steamid,
    client_id = login$client_id,
    request_id = login$request_id,
    token = get_access_token()
  )
  class(auth) <- "steam_auth_session"
  assign("auth", auth, envir = globst)

  if (!is_authenticated()) {
    abort("Authentication was unsuccessful. Session is not authenticated.")
  }

  if (persistent) {
    memorize_session(auth)
  }

  auth
}


#' @rdname auth
#' @description
#' \code{auth_qr} authenticates by showing a QR code that can be
#' scanned using the Steam mobile app. The QR code is shown as an R plot
#' and refreshes every 5 seconds.
#'
#' @export
auth_qr <- function(friendly_name = "steamr",
                    device_details = "Login using QR code",
                    persistent = getOption("steamr_persistent", FALSE)) {
  check_interactive()

  if (!loadable("qrcode")) {
    abort("The {.pkg qrcode} package must be installed to generate QR codes.")
  }

  refresh <- TRUE
  cli::cli_inform(c(
    "In the next seconds, a QR code will be plotted.",
    "Scan the QR code using the Steam mobile app to authenticate.",
    "The QR code will refresh every 5 seconds.",
    "You can press <Esc> to exit."
  ))

  status <- FALSE
  while (!status) {
    if (refresh) {
      base_time <- Sys.time()
      qr <- get_steam_qr()
      code <- qrcode::qr_code(qr$challenge_url)
      plot(code)
    }

    poll <- poll_auth_session_status(qr$client_id, qr$request_id)
    status <- !is.null(poll$access_token)

    diff <- difftime(Sys.time(), base_time, units = "secs")
    refresh <- diff >= 20
    cat(countdown(ceiling(diff), max = 30, step = 5), "\r")
    Sys.sleep(1)
  }

  transfer <- finalize_login(poll$refresh_token)
  set_tokens(transfer)

  auth <- list(
    vanity = poll$account_name,
    steamid = transfer$steamID,
    client_id = qr$client_id,
    request_id = qr$request_id,
    token = get_access_token()
  )
  class(auth) <- "steam_auth_session"
  assign("auth", auth, envir = globst)

  if (!is_authenticated()) {
    abort("Authentication was unsuccessful. Session is not authenticated.")
  }

  if (persistent) {
    memorize_session(auth)
  }

  auth
}


#' @rdname auth
#' @export
#' @description
#' \code{auth_logout} ends the active authenticated session by formally logging
#' out of Steam and removing all cookies and authentication information from
#' the session and the cache.
auth_logout <- function() {
  session <- globst$session

  if (is.null(session)) {
    abort("Cannot log out. Session is not authenticated.")
  }

  url <- file.path(store_api(), "logout")
  params <- list(sessionid = get_sessionid())
  request_generic(
    url,
    params = params,
    http_method = "POST",
    format = "html",
    headers = list(
      `Content-Length` = nchar(params$sessionid) + 10, # necessary to prevent HTTP411
      Connection = "keep-alive",
      Origin = store_api() # necessary for TLS communication
    )
  )

  if (is_authenticated()) {
    abort("Logout was unsuccessful. Session is still authenticated.")
  }

  clear_auth()
}


#' @rdname auth
#' @export
auth_history <- function() {
  check_authenticated()
  res <- request_webapi(
    api = public_api(),
    interface = "IAuthenticationService",
    method = "EnumerateTokens",
    http_method = "POST",
    version = "v1"
  )
  res <- as_data_frame(res$response$refresh_tokens)
  res$time_updated <- as.POSIXct(res$time_updated)
  res
}


clear_auth <- function() {
  unlink(cache_path(), recursive = TRUE)
  rm(session, auth, envir = globst)
}


countdown <- function(n, max = 5, step = 1) {
  n <- as.numeric(n)
  count <- seq(0, n, by = step)
  empty <- rep(strrep(" ", 4), (max - count[length(count)]) / step)
  count <- count[count != 0]
  count <- if (length(count)) paste0(count, "...")
  paste(c(count, empty), collapse = " ")
}


get_steam_qr <- function(device_friendly_name = NULL,
                         platform_type = 0,
                         device_details = NULL,
                         website_id = NULL) {
  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IAuthenticationService",
    method = "BeginAuthSessionViaQR",
    version = "v1",
    params = params,
    http_method = "POST"
  )$response
}


get_password_rsa_public_key <- function(username) {
  params <- .make_params(account_name = username)
  request_webapi(
    api = public_api(),
    interface = "IAuthenticationService",
    method = "GetPasswordRSAPublicKey",
    version = "v1",
    params = params
  )$response
}


encrypt_password <- function(password, mod, exp) {
  mod <- hex_to_raw(mod)
  exp <- hex_to_raw(exp)
  key <- PKI::PKI.mkRSApubkey(mod, exp, format = "DER")
  key <- openssl::read_pubkey(key, der = TRUE)
  enc <- openssl::rsa_encrypt(charToRaw(password), key)
  openssl::base64_encode(enc)
}


begin_auth_session <- function(device_friendly_name,
                               account_name,
                               encrypted_password,
                               encryption_timestamp,
                               platform_type = NULL,
                               persistence = NULL,
                               website_id = NULL,
                               device_details = NULL,
                               guard_data = NULL,
                               language = "english",
                               qos_level = NULL) {
  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IAuthenticationService",
    method = "BeginAuthSessionViaCredentials",
    version = "v1",
    http_method = "POST",
    params = params
  )$response
}


poll_auth_session_status <- function(client_id, request_id) {
  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IAuthenticationService",
    method = "PollAuthSessionStatus",
    version = "v1",
    params = params,
    http_method = "POST",
    serror = FALSE
  )$response
}


update_with_mobile_confirmation <- function(version,
                                            client_id,
                                            steamid,
                                            signature,
                                            confirm = TRUE,
                                            persistence = TRUE) {
  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IAuthenticationService",
    method = "UpdateAuthSessionWithMobileConfirmation",
    version = "v1",
    params = params,
    http_method = "POST",
    serror = FALSE
  )$response
}


update_with_steam_guard_code <- function(client_id,
                                         steamid,
                                         code,
                                         code_type = 3) {
  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IAuthenticationService",
    method = "UpdateAuthSessionWithSteamGuardCode",
    version = "v1",
    params = params,
    http_method = "POST",
    serror = FALSE
  )$response
}


finalize_login <- function(refresh_token, sessionid = NULL) {
  url <- "https://login.steampowered.com/jwt/finalizelogin"
  params <- list(
    nonce = refresh_token,
    sessionid = sessionid,
    redir = "https://steamcommunity.com/login/home/?goto="
  )
  request_generic(url, params, http_method = "POST")
}


set_tokens <- function(transfer) {
  params <- transfer$transfer_info

  if (is.null(params)) {
    abort("Redirects after authentication failed, no parameters fetched.")
  }

  for (redir in params) {
    redir$params$steamID <- transfer$steamID
    request_generic(redir$url, redir$params, method = "POST", format = "html")
  }
}


get_access_token <- function() {
  request_storefront(
    api = store_api(),
    interface = "pointssummary",
    method = "ajaxgetasyncconfig",
    cache = FALSE
  )$data$webapi_token
}


get_sessionid <- function() {
  cookiejar <- get("session", envir = globst)
  cookies <- read_cookies(cookiejar)
  sessionid <- cookies[cookies$name %in% "sessionid", ]
  sessionid$value[1]
}


check_captcha <- function(login) {
  if (isTRUE(login$captcha_needed)) {
    abort("Captcha test needed to authenticate.")
  }
}


handle_steam_guard <- function(login, code) {
  if (isFALSE(login$had_remote_interaction)) {
    return(login)
  }

  is_twofactor <- is.null(code)
  if (is_twofactor) {
    cat("Steam Guard authentication required.\n")
    cat("Confirm the login request using your Steam Guard mobile app.\n\n")

    poll <- poll_auth_session_status(login$client_id, login$request_id)
    while (is.null(poll$access_token)) {
      Sys.sleep(2)
      poll <- poll_auth_session_status(login$client_id, login$request_id)
    }
  } else {
    update <- update_with_steam_guard_code(
      client_id = login$client_id,
      steamid = login$steamid,
      code_type = 3,
      code = code
    )
    poll <- poll_auth_session_status(login$client_id, login$request_id)
  }

  poll
}


is_authenticated <- function() {
  # check if session is initialized
  auth <- get0("auth", envir = globst)
  if (is.null(auth)) {
    return(FALSE)
  }

  # check if session is fully authenticated
  token <- get_access_token()
  if (is.null(token)) {
    return(FALSE)
  }

  TRUE
}


#' Get authentication
#' @description
#' Retrieve global authentication object. When authenticating a session
#' using \code{\link{auth_credentials}} or \code{\link{auth_qr}}, an auth
#' object is attached to the R session. This object can be useful when
#' manually creating requests to the Steam API. Use this function to retrieve
#' these informations. Only for advanced usage.
#'
#' @param auth Field to extract. Can be one of \code{vanity}, \code{steamid},
#' \code{client_id}, \code{request_id} and \code{token}.
#'
#' @returns An auth object as returned by \code{auth_credentials} and
#' \code{auth_qr}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' auth_credentials("name")
#'
#' get_auth()
#' }
get_auth <- function(field = NULL) {
  check_authenticated()
  auth <- get("auth", envir = globst)
  if (!is.null(field)) {
    auth <- auth[[field]]
  }
  auth
}


set_auth <- function(auth, session) {
  assign("auth", auth, envir = globst)
  assign("session", session, envir = globst)
}


memorize_session <- function(auth) {
  session <- get("session", envir = globst)
  session_cache <- cache_path(basename(session))
  file.copy(session, session_cache)
  assign("session", session_cache, envir = globst)
  cache <- list(auth = auth, session = session_cache)
  saveRDS(cache, cache_path("auth_cache.rds"))
}


hex_to_raw <- function(x) {
  digits <- strtoi(strsplit(x, "")[[1]], base = 16L)
  as.raw(bitwShiftL(digits[c(TRUE, FALSE)], 4) + digits[c(FALSE, TRUE)])
}


#' @export
print.steam_auth_session <- function(x, ...) {
  cat(
    "<steam_auth_session>\n",
    " Vanity     :", x$vanity, "\n",
    " Steam64    :", x$steamid, "\n",
    " Client ID  :", x$client_id, "\n",
    " Request ID :", x$request_id, "\n"
  )
}
