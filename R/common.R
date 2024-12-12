#' Common arguments
#' @description
#' Common arguments that are reused in various functions of the Web and
#' storefront API.
#'
#' @param appid Application ID referencing a Steam application. There are
#' generally two ways of retrieving an appID:
#' \itemize{
#'  \item{Inspect or parse the store URL of an application, e.g. \code{https://store.steampowered.com/app/10/CounterStrike/}}
#'  \item{Query appIDs programmatically, e.g. using \code{\link{search_apps}}}
#' }
#' @param appids A vector of multiple appIDs of applications in the Steam store.
#' There are generally two ways of retrieving an appID:
#' \itemize{
#'  \item{Inspect or parse the store URL of an application, e.g. \code{https://store.steampowered.com/app/10/CounterStrike/}}
#'  \item{Query applications programmatically, e.g. using \code{\link{search_apps}}}
#' }
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
#' @keywords internal
#' @name common
#' @import checkmate
NULL


auth_table <- function(...) {
  args <- lapply(list(...), \(x) {names(x)[1] <- "function"; x})
  x <- rbind_list(args)
  x[["function"]] <- NULL
  names(x)[1] <- "function"

  needs_key <- any(x$key)
  needs_login <- any(x$login)
  needs_auth <- any(needs_key, needs_login)

  x$key <- ifelse(x$key, "yes", "no")
  x$login <- ifelse(x$login, "yes", "no")
  if (!is.null(x$note)) x$note <- ifelse(is.na(x$note), "", x$note)
  if (all(!nzchar(x$note))) x$note <- NULL
  x[["function"]] <- paste0("\\code{", x[["function"]], "}")
  names(x) <- cvapply(gsub("_", " ", names(x)), to_title)
  fmt <- sinew::tabular(x)
  fmt <- gsub("#' ?", "", fmt)

  intro <- if (needs_auth) {
    "The functions of this reference page are subject to the following authentication requirements (Key = API key needed, Login = user login needed):"
  } else {
    "The functions of this reference page do not need any kind of authentication to be used (Key = API key needed, Login = user login needed)."
  }

  paste(
    "\\section{Authentication}{",
    intro,
    fmt,
    if (needs_key) "API keys can be set by storing the API key in the \\code{STEAM_API_KEY} environment variable.",
    "",
    if (needs_login) "To learn more about user authentication, see \\code{\\link{auth_credentials}}.",
    "}",
    sep = "\n"
  )
}
