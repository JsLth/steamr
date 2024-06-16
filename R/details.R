#' Get app details
#' @description
#' Retrieve detailed information on an application in the steam store.
#'
#' \code{appdetails} uses Steam's unofficial API.
#'
#' \code{appdetails_steamcmd}
#' uses the unofficial \href{https://www.steamcmd.net/}{steamcmd API}, which
#' provides data from the Steam client network (powered by the
#' \href{https://steam.readthedocs.io/en/stable/api/steam.client.html}{\code{steam}}
#' Python library).
#'
#' \code{appdetails_steamspy} uses the unofficial
#' \href{https://steamspy.com/}{SteamSpy API}, which provides
#' custom estimates on owners, tags, genres and price dynamics. To get a full
#' app list from SteamSpy, see \code{\link{steamspy}}.
#'
#' @param appids,appid One or up to 100 application IDs. If more than one app
#' IDs is passed, the \code{filters} argument must only contain the
#' \code{price_overview} filter.
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
#' @export
#'
#' @note
#' \code{appdetails} is rate-limited at 200 requests per 5 minutes.
#'
#' \code{appdetails_steamcmd} is not rate-limited.
#'
#' \code{appdetails_steamspy} is rate-limited at 1 request per second.
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
  request_storefront(
    api = store_api(),
    interface = "api",
    method = "appdetails",
    params = params,
    rate = 200 / 300
  )
}


#' @rdname appdetails
#' @export
appdetails_steamcmd <- function(appid) {
  check_number(appid)
  check_length(appid, le = 1, ge = 1)
  url <- sprintf("https://api.steamcmd.net/v1/info/%s", appid)
  res <- jsonlite::read_json(url, simplifyVector = TRUE, flatten = TRUE)

  if ("detail" %in% names(res)) {
    msg <- res$detail[[1]]$msg
    stop(paste("steamcmd.net returned an error:", msg))
  }

  res$data[[1]]
}


#' @rdname appdetails
#' @export
appdetails_steamspy <- function(appid) {
  check_number(appid)
  params <- list(request = "appdetails", appid = appid)
  res <- request_steamspy(params)
  res$tag_names <- paste(names(res$tags), collapse = ", ")
  res$tags <- paste(res$tags, collapse = ", ")
  owners <- steamspy_estimate_to_range(res$owners)
  res$owners_low <- owners[1]
  res$owners_high <- owners[2]
  res$owners <- NULL
  as_data_frame(rbind_list(list(res)))
}


steamspy_estimate_to_range <- function(x) {
  est <- match_regex("(.+) .. (.+)", x)[[1]][c(2, 3)]
  as.numeric(gsub(",", "", est))
}
