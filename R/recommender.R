recommender <- function(steamid,
                        include_played = FALSE,
                        algorithm = FALSE,
                        reinference = FALSE,
                        model_version = 0,
                        ignored = NULL) {
  check_authenticated()
  check_string(steamid)
  check_bool(include_played)
  check_bool(algorithm)
  check_bool(reinference)
  check_integerish(model_version, null = TRUE)
  check_number(ignored, null = TRUE)
  steamid <- convert_steamid(steamid, to = "steam64")
  sessionid <- get_sessionid()

  params <- .make_params(access_token = FALSE, key = FALSE)
  request_storefront(
    api = store_api(),
    interface = "recommender",
    method = sprintf("%s/results", steamid),
    params = params
  )
}


get_recommender_input <- function(steamid) {
  check_string(steamid)
  steamid <- convert_steamid(steamid, to = "steam64")
  sessionid <- get_sessionid()

  params <- .make_params(access_token = FALSE, key = FALSE)
  request_storefront(
    api = store_api(),
    interface = "recommender",
    method = sprintf("%s/inputs", steamid),
    params = params
  )
}
