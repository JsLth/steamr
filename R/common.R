#' Common arguments
#' @description
#' Common arguments that are reused in various functions of the Web and
#' storefront API.
#'
#' @param appid A single appID of an application in the Steam store.
#' @param appids A vector of multiple appIDs of applications in the Steam store.
#' @param language ISO 639-1 language code all tokenized strings should be
#' returned in. Formally, a tokenized string is a string that is prefixed
#' with a \code{#} in the VDF file. Not all tokenized strings have a translation
#' for all languages. If no translation is available, defaults to English.
#' @param elanguage Numeric code representing the store language. A list
#' of language codes and their corresponding languages is defined in
#' \code{\link{ELanguage}}.
#' @param country_code ISO 3166 country code representing the country from
#' which to view the Steam store.
#' @param steam_realm Number describing the Steam realm. A value of 1
#' indicates Steam Global, 2 indicates Steam China, and 0 indicates Unknown.
#' @param steamid SteamID of a user. The SteamID must be in a format that can
#' be converted by \code{\link{convert_steamid}}. This includes vanity,
#' Steam64, Steam2, and Steam3 IDs.
#' @param steamids A vector of multiple SteamIDs of a user. The SteamIDs must
#' be in a format that can be converted by \code{\link{convert_steamid}}.
#' This includes vanity, Steam64, Steam2, and Steam3 IDs.
#'
#' @usage NULL
#' @format NULL
#' @export
common <- NULL
