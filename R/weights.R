#' App similarity
#' @description
#' Compute similarity weights and identify clusters of applications.
#'
#' App weights are undocumented and not defined in the Steam Web API. It can
#' be assumed that the weights represent some type of prioritization
#' measure for a given user, i.e. the higher the weight the more an app
#' fits to a user. Argument documentation is a good guess at best.
#'
#' @inheritParams common
#' @param tag_score_factor Factor indicating the weight given to an ominous
#' tag factor.
#' @param playtime_max_seconds A value indicating the maximum seconds of
#' playtime. Defaults to 100 hours or 360,000 seconds. The exact meaning
#' of this parameter is unknown.
#' @param playtime_max_games A value indicating the maximum games. Defaults
#' to 3 games. The exact meaning of this parameter is unknown.
#' @param playtime_score_factor Factor indicating the weight given to the
#' playtime score.
#' @param popularity_factor Factor indicating the weight given to app
#' popularity. The more popular the higher the app weight. Can be an integer
#' between 1 and 4. Every other value seems to be ignored.
#' @param popularity_reciprocal A value indicating a reciprocal weight for
#' popularity, i.e. the less popular the higher the score. Defaults to
#' \code{1e+04}.
#' @param popularity_base_score A value indicating the base popularity
#' score. Defaults to \code{5e+06}.
#'
#' @returns \describe{
#'  \item{\code{weight_app}}{A dataframe containing the appID and the weight
#'  given to this appID for the specified user.}
#'
#'  \item{\code{cluster_apps}}{A dataframe containing clusters of apps.
#'  \code{cluster_id} is an ID unique to the specific cluster call.
#'  \code{playtime_forever} and \code{playtime_2weeks} are the total
#'  playtimes of a cluster in minutes. \code{played_appids} is a vector
#'  of all apps in the cluster. \code{played_appids} is a vector of
#'  similar apps. \code{similar_item_popularity_score} is the popularity
#'  score of the similar app vector.}
#' }
#'
#' @export
#'
#' @examples
#' steamid <- "76561197960435530"
#'
#' # generate prioritization weight for team fortress 2 and portal 2
#' weight_app(steamid, c(440, 620))
#'
#' # generate 5 clusters for a given user
#' cluster_apps(steamid, n = 5)
#'
weight_app <- function(steamid,
                       appids,
                       country_code = NULL,
                       tag_score_factor = NULL,
                       playtime_max_seconds = NULL,
                       playtime_max_games = NULL,
                       playtime_score_factor = NULL,
                       popularity_factor = NULL,
                       popularity_reciprocal = NULL,
                       popularity_base_score = 0,
                       include_owned_games = FALSE) {
  check_number(steamid)
  check_number(appids)
  check_string(country_code, null = TRUE)
  check_bool(include_owned_games)
  steamid <- convert_steamid(steamid, to = "steam64")

  ids <- store_items(appids, type = "appid")
  options <- store_app_similarity_priority_options(
    tag_score_factor = tag_score_factor,
    playtime_max_seconds = playtime_max_seconds,
    playtime_max_games = playtime_max_games,
    playtime_score_factor = playtime_score_factor,
    popularity_factor = popularity_factor,
    popularity_reciprocal = popularity_reciprocal,
    popularity_base_score = popularity_base_score
  )

  params <- .make_params(
    steamid = steamid,
    country_code = country_code,
    input_json = do.call(.make_input_json, list(ids = ids, options = options)),
    include_owned_games = include_owned_games
  )

  res <- request_webapi(
    api = public_api(),
    interface = "IStoreAppSimilarityService",
    method = "PrioritizeAppsForUser",
    version = "v1",
    params = params
  )$response
  as_data_frame(res$items)
}


#' @rdname weight_app
#' @export
#'
#' @param n Number of clusters to generate. By default, generates 75 clusters.
#' @param cluster_index Unknown.
cluster_apps <- function(steamid,
                         n = NULL,
                         cluster_index = NULL) {
  check_string(steamid)
  check_integerish(n, null = TRUE)
  check_integerish(cluster_index, null = TRUE)
  check_integerish(sort, null = TRUE)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params(
    steamid = steamid,
    clusters_to_return = n,
    cluster_index = cluster_index,
    sort = sort
  )
  res <- request_webapi(
    api = public_api(),
    interface = "IStoreAppSimilarityService",
    method = "IdentifyClustersFromPlaytime",
    version = "v1",
    params = params,
    http_method = "POST"
  )$response$clusters
  as_data_frame(res)
}


store_app_similarity_priority_options <- function(tag_score_factor = NULL,
                                                  playtime_max_seconds = NULL,
                                                  playtime_max_games = NULL,
                                                  playtime_score_factor = NULL,
                                                  popularity_factor = NULL,
                                                  popularity_reciprocal = NULL,
                                                  popularity_base_score = 0) {
  check_numeric(tag_score_factor, null = TRUE)
  check_integerish(playtime_max_seconds, null = TRUE)
  check_integerish(playtime_max_games, null = TRUE)
  check_numeric(playtime_score_factor, null = TRUE)
  check_integerish(popularity_factor, null = TRUE)
  check_integerish(popularity_reciprocal, null = TRUE)
  check_integerish(popularity_base_score, null = TRUE)

  obj <- as.list(environment())
  class(obj) <- "StoreAppSimilarityPriorityOptions"
  drop_null(drop_false(obj)) %empty% NULL
}
