#' Common arguments
#' @description
#' Common arguments that are reused in various functions of the Web and
#' storefront API.
#'
#' @param appid A single appID of an application in the Steam store.
#' @param appids A vector of multiple appIDs of applications in the Steam store.
#' @param steamid SteamID of a user. The SteamID must be in a format that can
#' be converted by \code{\link{convert_steamid}}. This includes vanity,
#' Steam64, Steam2, and Steam3 IDs.
#' @param steamids A vector of multiple SteamIDs of a user. The SteamIDs must
#' be in a format that can be converted by \code{\link{convert_steamid}}.
#' This includes vanity, Steam64, Steam2, and Steam3 IDs.
#' @param paginate If \code{TRUE}, paginates through the results. Some methods
#' provide data access in digestible chunks and do not return all available
#' information at once. The \code{paginate} automatically retrieves all pages
#' until \code{max_pages} is reached.
#' @param max_pages Maximum number of pages to paginate. Ignored if
#' \code{paginate} is \code{FALSE}. Defaults to \code{Inf} such that all
#' available data are paginated.
#'
#' @usage NULL
#' @format NULL
#' @export
common <- NULL
