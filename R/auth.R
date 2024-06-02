#' Session authentication
#' @description
#' Initializes an authorized Steam session. Some functions in both the
#' Web API and internal API require the session to be authenticated. This
#' concerns all API methods that access personal account information.
#' \code{steam_login} essentially signs in a user programmatically.
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
#' @param friendly_name Name of the session. Used to track authorized devices
#' in Steam Guard.
#' @param details Arbitrary details about the device attempting to
#' authenticate. Will be shown in Steam Guard.
#' @returns Information about the authenticated session.
#'
#' @note
#' Session authentication is only possible in interactive mode because it
#' requires the manual insertion of Steam guard codes, confirmation of
#' login requests in the mobile app, or the scanning of QR codes. All of these
#' actions are not suitable for batch processing.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' user <- "gabelogannewell"
#' steam_login(user)
#'
#' # set a friendly name to identify this session
#' steam_login(user, friendly_name = "R session")
#'
#' # sign in using a QR code
#' show_steam_qr()
#' }
steam_login <- function(username,
                        password = openssl::askpass,
                        shared_secret = NULL,
                        persistent = FALSE,
                        friendly_name = NULL,
                        details = NULL) {
  if (!interactive()) {
    stop("Session authentication is only possible in interactive mode.")
  }

  password <- password()
  params <- get_password_rsa_public_key(username)
  epass <- encrypt_password(password, params$publickey_mod, params$publickey_exp)
  login <- begin_auth_session(
    device_friendly_name = friendly_name,
    account_name = username,
    encrypted_password = epass,
    encryption_timestamp = params$timestamp,
    device_details = details
  )
  check_captcha(login)
  login <- handle_steam_guard(login)
  session <- list(
    steamid = login$steamid,
    client_id = login$client_id,
    request_id = login$request_id,
    token = login$weak_token
  )
  class(session) <- "steam_auth_session"
  assign("session", session, envir = globst)
  session
}


#' @rdname steam_login
#' @description
#' \code{show_steam_qr} authenticates by showing a QR code that can be
#' scanned using the Steam mobile app. The QR code is shown as an R plot
#' and refreshes every 5 seconds.
#'
#' @export
show_steam_qr <- function(friendly_name = NULL, device_details = NULL) {
  if (!loadable("qrcode")) {
    stop("The qrcode package must be installed to generate QR codes.")
  }

  refresh <- TRUE
  cat(paste(
    "In the next seconds, a QR code will be plotted.",
    "Scan the QR code using the Steam mobile app to authenticate.",
    "The QR code will refresh every 5 seconds.",
    "You can press <Esc> to exit.\n",
    sep = "\n"
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
    refresh <- diff >= (qr$interval - 0.5)
    cat(countdown(ceiling(diff), max = 6), "\r")
    Sys.sleep(1)
  }

  get_auth_session_info(qr$client_id, poll$access_token)
}


countdown <- function(n, max = 5) {
  count <- seq(1, n)
  count <- paste0(count, "...")
  empty <- rep(strrep(" ", 4), max - n)
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


get_auth_session_info <- function(client_id, token) {
  params <- .make_params(client_id = client_id)
  request_webapi(
    api = public_api(),
    interface = "IAuthenticationService",
    method = "GetAuthSessionInfo",
    version = "v1",
    params = params,
    http_method = "POST",
    access_token = token
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
                                            confirm = FALSE,
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


check_captcha <- function(login) {
  if (isTRUE(login$captcha_needed)) {
    stop("Captcha test needed to authenticate.")
  }
}


handle_steam_guard <- function(login, code) {
  if (isFALSE(login$had_remote_interaction)) {
    return(login)
  }

  is_twofactor <- is.null(code)
  cat("Steam Guard authentication required.\n")
  if (is_twofactor) {
    cat("Confirm the login request using your Steam Guard mobile app.\n")

    poll <- poll_auth_session_status(login$client_id, login$request_id)
    if (is.null(poll$access_token)) {
      Sys.sleep(2)
      poll <- handle_steam_guard(login)
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


hex_to_raw <- function(x) {
  digits <- strtoi(strsplit(x, "")[[1]], base = 16L)
  as.raw(bitwShiftL(digits[c(TRUE, FALSE)], 4) + digits[c(FALSE, TRUE)])
}


#' @export
print.steam_auth_session <- function(x, ...) {
  cat(
    "<steam_auth_session>\n",
    " Steam ID   :", x$steamid, "\n",
    " Client ID  :", x$client_id, "\n",
    " Request ID :", x$request_id, "\n",
    " Token      : <redacted>\n"
  )
}
