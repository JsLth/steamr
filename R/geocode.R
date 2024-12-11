#' Geocode
#' @description
#' Geocode Steam country codes, state codes, and cityIDs. Can be used to
#' geocode the output of functions that return \code{loccountrycode},
#' \code{locstatecode}, or \code{loccityid}. This includes functions such
#' as \code{get_player_summary}.
#'
#' Since users are not obliged to specify any level of location, the geocoder
#' skips \code{NA} values or falls back to the next higher level, e.g. if
#' \code{city_id} is \code{NA}, then the state coordinates are returned. This
#' behavior can be suppressed by passing the \code{force_level} argument. This
#' can be useful, if the coarseness of the data should be consistent.
#'
#' This function is powered by
#' \href{https://github.com/quer/steam-friends-countries}{quer's Steam location}
#' dataset.
#'
#' @param country_code A vector of Steam users' states of residence. Must be
#' an ISO 3166 country code.
#' @param state_code A vector of Steam users' states of residence. Must be a
#' code as returned by the Steam API.
#' @param city_id A vector of Steam users' cities of residence. Must be a code
#' as returned by the Steam API.
#' @param force_level Geographic level that the output must return. Can be
#' one of \code{country}, \code{state}, and \code{city}. If an input location
#' is not available at the specified level, an empty geometry or \code{NA}
#' is returned. If \code{NULL}, falls back to the next higher level if an input
#' location is unavailable.
#' @param cache The download of Steam location geocodes can take a few seconds.
#' If \code{TRUE}, caches this data in a temporary file to speed up subsequent
#' function calls.
#'
#' @returns If \code{sf} is installed, an \code{sf} dataframe containing the
#' point geometries of a user location. Otherwise, a dataframe containing
#' \code{lng} and \code{lat} columns.
#'
#' @examples
#' \dontrun{
#' # get data containing user locations
#' user <- get_player_summary(c("76561197984981409", "76561197968282875"))
#' cc <- user$loccountrycode
#' sc <- user$locstatecode
#' ci <- user$loccityid
#'
#' # geocode the user location
#' geocode_steam(cc, sc, ci)
#'
#' # force level to state level -- although cityIDs are available
#' # this can be useful to keep geographic levels consistent
#' geocode_steam(cc, sc, ci, force_level = "state")
#'
#' # similarly, if location information is missing but a certain level
#' # is requested, geocoder returns NA to adhere to the level target
#' geocode_steam(cc, force_level = "state")
#' }
geocode_steam <- function(country_code,
                          state_code = NULL,
                          city_id = NULL,
                          force_level = NULL,
                          cache = TRUE) {
  check_string(country_code)
  check_string(state_code, null = TRUE)
  check_number(city_id, null = TRUE)
  check_string(force_level, null = TRUE)
  check_bool(cache)
  nas <- rep(NA, length(country_code))
  state_code <- as.character(state_code %||% nas)
  city_id <- as.character(city_id %||% nas)

  cache_file <- get0("geocode_cache", envir = globst, ifnotfound = "")
  if (file.exists(cache_file)) {
    geocodes <- readRDS(cache_file)
  } else {
    url <- paste0(
      "https://raw.githubusercontent.com/quer/steam-friends-countries/",
      "master/data/steam_countries.json"
    )
    geocodes <- jsonlite::read_json(url, flatten = TRUE)

    if (cache) {
      cache_file <- tempfile(pattern = "steamr_geocode_cache", fileext = ".rds")
      assign("geocode_cache", cache_file, envir = globst)
      saveRDS(geocodes, file = cache_file)
    }
  }

  geom <- Map(
    geocode_single,
    country_code, state_code, city_id,
    MoreArgs = list(force_level = force_level, geocodes = geocodes)
  )

  fun <- ifelse(loadable("sf"), c, rbind.data.frame)
  do.call(fun, geom)
}


geocode_single <- function(cc, sc, ci, geocodes, force_level = NULL) {
  gcd <- geocodes[[cc]]
  na_coords <- list(coordinates = list(lng = NA, lat = NA))
  force_level <- force_level %||% ""
  can_state <- !force_level %in% "country"
  can_city <- !force_level %in% c("country", "state")
  must_state <- force_level %in% c("state", "city")
  must_city <- force_level %in% "city"

  if (is.na(cc) && identical(force_level, "country")) {
    gcd <- na_coords
  }

  if (!is.na(sc) && can_state) {
    gcd <- gcd$states[[sc]]

    if (!is.na(ci) && can_city) {
      gcd <- gcd$cities[[ci]]
    } else if (must_city) {
      gcd <- na_coords
    }
  } else if (must_state) {
    gcd <- na_coords
  }

  coords <- gcd$coordinates[c("lng", "lat")]
  if (loadable("sf")) {
    coords <- as.numeric(unlist(coords))
    sf::st_sfc(sf::st_point(coords), crs = 4326)
  } else {
    as_data_frame(coords)
  }
}
