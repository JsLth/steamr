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
#' @param client Unknown.
#' @param filters Keys to include in the output. Can be any key that
#' is returned by this function. If \code{basic}, includes the following
#' default keys: \code{type}, \code{name}, \code{steam_appid},
#' \code{required_age}, \code{dlc}, \code{detailed_description},
#' \code{about_the_game}, \code{short_description}, \code{supported_languages},
#' \code{header_image}, \code{website}, \code{pc_requirements},
#' \code{mac_requirements}, \code{linux_requirements}. \code{filters} does
#' not accept any of these keys as input.
#' @inheritParams common
#'
#' @returns \describe{
#'  \item{\code{appdetails}}{A nested list containing the following keys:
#'  \itemize{
#'    \item{\code{type}: Type of application (game, dlc, demo, advertising,
#'    mod, or video)}
#'    \item{\code{name}: Name of the application}
#'    \item{\code{steam_appid}: appID of the application}
#'    \item{\code{required_age}: Required age to view the application}
#'    \item{\code{controller_support}: Whether the app has controller support
#'    (partial or full). May be omitted.}
#'    \item{\code{is_free}: Whether the game is free to play}
#'    \item{\code{dlc}: Array of appIDs of DLCs associated with this application.}
#'    \item{\code{detailed_description}: A detailed description in HTML}
#'    \item{\code{about_the_game}: "About the game" description in HTML}
#'    \item{\code{short_description}: Short description in HTML, usually found
#'    in the application preview}
#'    \item{\code{fullgame}: Name and appID of the full game. Sometimes
#'    found in demos or movies.}
#'    \item{\code{supported_languages}: Languages that the application supports,
#'    in HTML.}
#'    \item{\code{header_image}: URL to the header image.}
#'    \item{\code{capsule_image}: URL to the capsule image.}
#'    \item{\code{capsule_imagev5}: URL to a different capsule image.}
#'    \item{\code{website}: URL to the application website.}
#'    \item{\code{pc_requirements}: Minimum and recommended requirements
#'    for Windows, in HTML}
#'    \item{\code{mac_requirements}: Minimum and recommended requirements
#'    for Mac OS, in HTML}
#'    \item{\code{linux_requirements}: Minimum and recommended requirements
#'    for Linux, in HTML}
#'    \item{\code{developers}: Names of the developers.}
#'    \item{\code{publishers}: Names of the publishers.}
#'    \item{\code{demos}: List of appIDs which are demos of this application.}
#'    \item{\code{price_overview}: List containing the initial and final
#'    price as well as price discounts. Omitted for free to play games.}
#'    \item{\code{packages}: List of packageIDs that include this game.}
#'    \item{\code{package_groups}: Dataframe specifying purchase options.}
#'    \item{\code{platforms}: Specifies whether the application is compatible
#'    with linux and mac.}
#'    \item{\code{metacritic}: Metacritic score and URL.}
#'    \item{\code{categories}: Store categories of the application. May be
#'    omitted.}
#'    \item{\code{genres}: Genres of the application. May be omitted.}
#'    \item{\code{screenshots}: A dataframe containing the paths to all
#'    showcase screenshots. Omitted if no screenshots exist.}
#'    \item{\code{movies}: A dataframe containing the paths to all
#'    showcase movies. Omitted if no movies exist.}
#'    \item{\code{recommendations}: A list containing the total number
#'    of recommendations.}
#'    \item{\code{recommendations}: A list containing the total and
#'    highlighted achievements.}
#'    \item{\code{release_date}: A list containing the release date and
#'    whether the application is coming soon.}
#'    \item{\code{support_info}: A list containing support URL and email.}
#'    \item{\code{background}: URL to the background image.}
#'    \item{\code{background_raw}: URL to the raw background image.}
#'    \item{\code{content_descriptors}: Content descriptor codes given to
#'    this application. Also includes a content descriptor description.
#'    A list of content descriptors is included in
#'    \code{\link{content_descriptors}}.}
#'    \item{\code{ratings}: A list containing maturity ratings.}
#'  }
#'  }
#'  \item{\code{appdetails_steamspy}}{A dataframe containing information
#'  about name, developer, publisher, reviews, concurrent players, genres,
#'  tags and estimated owners.}
#'
#'  \item{\code{appdetails_steamcmd}}{A nested list containing information
#'  listed \href{https://www.steamcmd.net/}{here}.}
#' }
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
#' # returns the detailed description about team fortress
#' appdetails(440, filters = "detailed_description")
#'
#' # returns the metacritic score and total number of reviews of Counter-Strike
#' appdetails(10, filters = c("recommendations", "metacritic"))
#'
#' # returns steamspy data
#' appdetails_steamspy(440)
#'
#' # returns steamcmd data
#' appdetails_steamcmd(440)
#' }
appdetails <- function(appid,
                       client = NULL,
                       filters = NULL,
                       country_code = "US",
                       language = "english") {
  params <- list(
    appids = appid,
    client = client,
    filters = paste(filters, collapse = ","),
    cc = country_code,
    l = language
  )
  res <- request_storefront(
    api = store_api(),
    interface = "api",
    method = "appdetails",
    params = params,
    rate = 200 / 300
  )[[1]]$data

  recurse(res, function(x) {
    if (is.data.frame(x)) {
      x <- as_data_frame(x)
    }
    x
  })
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
    abort("steamcmd.net returned an error", "x" = msg)
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
