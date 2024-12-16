globst <- new.env(parent = emptyenv())


is_steam_key <- function(x) {
  isTRUE(grepl("^[A-Z0-9]+$", x))
}


check_steam_key <- function() {
  key <- api_key()
  if (!is_steam_key(key)) {
    abort(c(
      "No valid Steam key stored in the {.envvar STEAM_API_KEY} environment variable.",
      "i" = "You can request an API key from here: {.url https://steamcommunity.com/dev/apikey}"
    ))
  }
}


api_key <- function() {
  Sys.getenv("STEAM_API_KEY")
}


#' Meta info
#' @description
#' Fetch metadata about the Steam server.
#'
#' \itemize{
#'  \item{\code{wba_methods}: returns information about all supported
#' API endpoints of the web API. A complete list including undocumented
#' endpoints can be found on \url{https://steamapi.xpaw.me/}}
#'  \item{\code{wba_servertime}: returns the current Steam server time}
#'  \item{\code{steam_stats} returns the current users online and ingame}
#'  \item{\code{wba_servers} returns details about running game servers}
#' }
#'
#' @returns \itemize{
#'  \item{\code{wba_methods}: An named list where each
#'  index contains a named list with information on the respective API
#'  endpoint.}
#'
#'  \item{\code{wba_servertime}: A \code{POSIXct} value.}
#'
#'  \item{\code{steam_stats}: A named list of length 2}
#'
#'  \item{\code{wba_servers}: A dataframe.}
#'
#' }
#'
#' @evalRd auth_table(
#'   list("wba_methods", key = FALSE, login = FALSE, note = "Providing a publisher key returns additional publisher-only methods"),
#'   list("wba_servertime", key = FALSE, login = FALSE),
#'   list("steam_stats", key = FALSE, login = FALSE),
#'   list("wba_servers", key = FALSE, login = FALSE)
#' )
#'
#' @name meta
#' @export
#'
#' @examples
#' \donttest{# details on low-level method for `wba_news`
#' methods <- wba_methods()
#' methods$ISteamNews$GetNewsForApp
#'
#' # get the current server time
#' wba_servertime()
#'
#' # get the current user number
#' steam_stats()
#'
#' # get details about game servers
#' get_servers()}
steam_stats <- function() {
  res <- request_storefront(
    api = valve_api(),
    interface = "about",
    method = "stats"
  )

  lapply(res, function(x) as.numeric(gsub(",", "", x, fixed = TRUE)))
}


#' @rdname meta
#' @export
wba_methods <- function() {
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamWebAPIUtil",
    method = "GetSupportedAPIList",
    version = "v1",
    params = params,
    simplify = FALSE
  )$apilist$interfaces
  nms <- cvapply(res, "[[", "name")
  methods <- lapply(res, function(x) {
    x <- x$methods
    nms1 <- cvapply(x, "[[", "name")
    x <- lapply(x, function(y) {
      y$name <- NULL
      y$parameters <- as_data_frame(rbind_list(y$parameters))
      y
    })
    names(x) <- nms1
    x
  })
  names(methods) <- nms
  methods
}


#' @rdname meta
#' @export
wba_servertime <- function() {
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamWebAPIUtil",
    method = "GetServerInfo",
    version = "v1",
    params = params,
    simplify = FALSE
  )

  as.POSIXct(res$servertime)
}


#' @rdname meta
#' @export
wba_servers <- function(filter = NULL, limit = NULL) {
  check_steam_key()
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


# readLines("https://raw.githubusercontent.com/SteamRE/SteamKit/master/Resources/SteamLanguage/eresult.steamd") %>%
#   .[-1:-2] %>%
#   .[1:length(.) - 1] %>%
#   trimws() %>%
#   gsub(";", "", .) %>%
#   strsplit(" = ") %>%
#   do.call(rbind.data.frame, .) %>%
#   setNames(c("msg", "code")) %>%
#   constructive::construct()
eresult <- data.frame(
  msg = c(
    "Invalid", "OK", "Fail", "NoConnection", "InvalidPassword",
    "LoggedInElsewhere", "InvalidProtocolVer", "InvalidParam", "FileNotFound",
    "Busy", "InvalidState", "InvalidName", "InvalidEmail", "DuplicateName",
    "AccessDenied", "Timeout", "Banned", "AccountNotFound", "InvalidSteamID",
    "ServiceUnavailable", "NotLoggedOn", "Pending", "EncryptionFailure",
    "InsufficientPrivilege", "LimitExceeded", "Revoked", "Expired",
    "AlreadyRedeemed", "DuplicateRequest", "AlreadyOwned", "IPNotFound",
    "PersistFailed", "LockingFailed", "LogonSessionReplaced", "ConnectFailed",
    "HandshakeFailed", "IOFailure", "RemoteDisconnect", "ShoppingCartNotFound",
    "Blocked", "Ignored", "NoMatch", "AccountDisabled", "ServiceReadOnly",
    "AccountNotFeatured", "AdministratorOK", "ContentVersion", "TryAnotherCM",
    "PasswordRequiredToKickSession", "AlreadyLoggedInElsewhere", "Suspended",
    "Cancelled", "DataCorruption", "DiskFull", "RemoteCallFailed",
    "PasswordUnset", "ExternalAccountUnlinked", "PSNTicketInvalid",
    "ExternalAccountAlreadyLinked", "RemoteFileConflict", "IllegalPassword",
    "SameAsPreviousValue", "AccountLogonDenied", "CannotUseOldPassword",
    "InvalidLoginAuthCode", "AccountLogonDeniedNoMail", "HardwareNotCapableOfIPT",
    "IPTInitError", "ParentalControlRestricted", "FacebookQueryError",
    "ExpiredLoginAuthCode", "IPLoginRestrictionFailed", "AccountLockedDown",
    "AccountLogonDeniedVerifiedEmailRequired", "NoMatchingURL", "BadResponse",
    "RequirePasswordReEntry", "ValueOutOfRange", "UnexpectedError", "Disabled",
    "InvalidCEGSubmission", "RestrictedDevice", "RegionLocked",
    "RateLimitExceeded", "AccountLoginDeniedNeedTwoFactor", "ItemDeleted",
    "AccountLoginDeniedThrottle", "TwoFactorCodeMismatch",
    "TwoFactorActivationCodeMismatch", "AccountAssociatedToMultiplePartners",
    "NotModified", "NoMobileDevice", "TimeNotSynced", "SMSCodeFailed",
    "AccountLimitExceeded", "AccountActivityLimitExceeded",
    "PhoneActivityLimitExceeded", "RefundToWallet", "EmailSendFailure",
    "NotSettled", "NeedCaptcha", "GSLTDenied", "GSOwnerDenied", "InvalidItemType",
    "IPBanned", "GSLTExpired", "InsufficientFunds", "TooManyPending",
    "NoSiteLicensesFound", "WGNetworkSendExceeded", "AccountNotFriends",
    "LimitedUserAccount", "CantRemoveItem", "AccountDeleted",
    "ExistingUserCancelledLicense", "CommunityCooldown", "NoLauncherSpecified",
    "MustAgreeToSSA", "LauncherMigrated", "SteamRealmMismatch",
    "InvalidSignature", "ParseFailure", "NoVerifiedPhone", "InsufficientBattery",
    "ChargerRequired", "CachedCredentialInvalid", "PhoneNumberIsVOIP",
    "NotSupported", "FamilySizeLimitExceeded"
  ),
  code = c(
    "0", "1", "2", "3", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14",
    "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27",
    "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
    "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53",
    "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66",
    "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
    "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92",
    "93", "94", "95", "96", "97", "98", "99", "100", "101", "102", "103", "104",
    "105", "106", "107", "108", "109", "110", "111", "112", "113", "114", "115",
    "116", "117", "118", "119", "120", "121", "122", "123", "124", "125", "126",
    "127", "128", "129"
  )
)
