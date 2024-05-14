#' Get app details
#' @description
#' Retrieve detailed information on an application in the steam store. Uses
#' Steam's unofficial API.
#'
#' \code{appdetails} returns details on a single app or price information
#' on up to 100 apps. \code{appdetails} is a simple wrapper that iterates
#' through \code{appids} and returns a list of details on all apps.
#'
#' @param appids One or up to 100 application IDs. If more than one app IDs is
#' passed, the \code{filters} argument must only contain the \code{price_overview}
#' filter.
#' @param client Unknown parameter.
#' @param filters Fields to include in the output. Can be any field name that
#' is returned by this function. If \code{basic}, includes a number of basic
#' fields.
#' @inheritParams get_items
#'
#' @returns A nested list containing detailed information on a single app.
#' If multiple app IDs are passed and \code{filters = "price_overview"},
#' contains price information on up to 100 applications.
#'
#' \code{appdetails_all} returns a list where each index corresponds to the
#' app details of an app ID in \code{appids}.
#'
#' @export
#'
#' @note
#' This endpoint is rate-limited at 200 requests per 5 minutes.
#'
#' @examples
#' \dontrun{
#' appdetails(10, filters = c("recommendations", "metacritic"))
#' }
appdetails <- function(appids,
                       client = NULL,
                       filters = NULL,
                       country_code = "US",
                       language = "english") {
  if (length(appids) > 1 && !identical(filters, "price_overview")) {
    stop("If multiple appids are passed, filters must contain only \"overview\".")
  }

  params <- list(
    appids = paste(appids, collapse = ","),
    client = client,
    filters = paste(filters, collapse = ","),
    cc = country_code,
    l = language
  )
  request_internal(
    api = store_api(),
    interface = "api",
    method = "appdetails",
    params = params,
    rate = 200 / 300
  )
}


#' @export
#' @rdname appdetails
appdetails_all <- function(appids,
                           client = NULL,
                           filters = NULL,
                           country_code = "US",
                           language = "english",
                           progress = TRUE) {
  pb <- utils::txtProgressBar(
    min = 1,
    max = length(appids),
    char = "\u25a0",
    style = 3
  )
  out <- lapply(appids, function(i) {
    utils::setTxtProgressBar(pb, i)
    appdetails(
      appids[i],
      client = client,
      filters = filters,
      country_code = country_code,
      language = language
    )[[1]]$data
  })

  df <- try(dplyr::bind_rows(out), silent = TRUE)
  if (is.data.frame(df)) {
    out <- df
  }

  out
}
