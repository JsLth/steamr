globst <- new.env(parent = emptyenv())


is_steam_key <- function(x) {
  isTRUE(grepl("^[A-Z0-9]+$", x))
}


api_key <- function() {
  key <- Sys.getenv("STEAM_API_KEY")
  if (!is_steam_key(key)) {
    stop("No valid Steam key stored in STEAM_API_KEY environment variable.")
  }
  key
}


#' Meta info
#' @description
#' Fetch metadata about the Steam server.
#'
#' \code{get_supported_api_list()} returns information about all supported
#' API endpoints of the web API. A complete list including undocumented
#' endpoints can be found on \url{https://steamapi.xpaw.me/}.
#'
#' \code{get_servertime()} returns the current Steam server time.
#'
#' \code{steam_stats()} returns the current users online and ingame.
#'
#' @returns \code{get_supported_api_list()} returns an unnamed list where each
#' index contains a named list with information on the respective API endpoint.
#' \code{get_servertime()} returns a \code{POSIXct} value. \code{steam_stats()}
#' contains a named list of length 2.
#' @rdname steam_stats
#' @export
#'
#' @examples
#' \dontrun{
#' get_supported_api_list()
#'
#' get_servertime()
#'
#' steam_stats()
#' }
steam_stats <- function() {
  res <- request_storefront(
    api = valve_api(),
    interface = "about",
    method = "stats"
  )

  lapply(res, function(x) as.numeric(gsub(",", "", x, fixed = TRUE)))
}


#' @rdname steam_stats
#' @export
get_supported_api_list <- function() {
  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "ISteamWebAPIUtil",
    method = "GetSupportedAPIList",
    version = "v1",
    params = params,
    simplify = FALSE
  )$apilist$interfaces
}


#' @rdname steam_stats
#' @export
get_servertime <- function() {
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
