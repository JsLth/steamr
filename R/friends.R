#' Friends
#' @description
#' Get friend lists, retrieve friends' game playtimes, recommendations and
#' general user data.
#'
#' Of these functions \code{get_friend_list} is the only non-authenticated
#' function. \code{get_friends} is similar in concept, but requires
#' authentication and delivers more detailed results.
#'
#' @inheritParams common
#' @param relationship Type of relationship to filter for. If \code{all},
#' does not filter. A list of available relationships can be retrieved
#' using \code{\link{steamkit_enum}("FriendRelationship", type = "SteamLanguage")}.
#'
#' @returns \describe{
#'  \item{\code{get_friend_list}}{A dataframe of all friends of a user
#'  along with their SteamIDs, relationship types and the date of
#'  friendship.}
#'
#'  \item{\code{get_friends_playtimes}}{A dataframe of all friends that
#'  have played a specific game and their playtimes. If a playtime is
#'  \code{NA}, then the friend only owns the game but has not played it.}
#'
#'  \item{\code{get_friends_recommendations}}{A vector of friend SteamIDs
#'  that have written a recommendation about a game.}
#'
#'  \item{\code{get_friends}}{A dataframe containing the SteamIDs of friends,
#'  their vanity ID and the avatar URL.}
#'
#'  \item{\code{get_friend_data}}{A dataframe containing the appID, SteamID
#'  of the friend, type of relationship to the application, and the amount of
#'  time played. If a playtime is \code{NA}, then the friend only owns the game
#'  but has not played it. The \code{type} column can take three values:
#'
#'  1 -- Played the game
#'
#'  2 -- Recently bought the game (?)
#'
#'  3 -- Owns the game}
#' }
#'
#' @export
#'
#' @seealso \code{\link{friends_in_review}}
#'
#' @examples
#' \dontrun{
#' # get the friend list of any user
#' get_friend_list("76561197960435530")
#'
#' # need authentication
#' auth_credentials("username")
#'
#' # how much did user's friends play team fortress?
#' get_friends_playtimes(440)
#'
#' # which friends recommended team fortress?
#' get_friends_recommendations(440)
#'
#' # more detailed list of user's friends
#' get_friends()
#' }
get_friend_list <- function(steamid, relationship = "all") {
  check_steam_key()
  check_string(steamid)
  check_string(relationship)
  steamid <- convert_steamid(steamid, to = "steam64")

  params <- .make_params(access_token = FALSE)
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUser",
    method = "GetFriendList",
    version = "v1",
    params = params
  )

  as_data_frame(res$friendslist$friends)
}


#' @rdname get_friend_list
#' @export
get_friends_playtimes <- function(appid) {
  check_steam_key()
  check_authenticated()
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IPlayerService",
    method = "GetFriendsGameplayInfo",
    version = "v1",
    params = params
  )$response
  as_data_frame(bind_rows(played = res$played_ever, owns = res$owns, .id = "state"))
}


#' @rdname get_friend_list
#' @export
get_friends_recommendations <- function(appid) {
  check_steam_key()
  check_authenticated()
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IUserReviewsService",
    method = "GetFriendsRecommendedApp",
    version = "v1",
    params = params
  )$response$accountids_recommended
  as_data_frame(res)
}


#' @rdname get_friend_list
#' @export
get_friends <- function() {
  check_authenticated()
  res <- request_storefront(
    api = comm_api(),
    interface = "actions",
    method = "ajaxlistfriends"
  )$friends

  as_data_frame(res)
}


#' @rdname get_friend_list
#' @export
get_friend_data <- function() {
  check_authenticated()

  params <- .make_params(key = FALSE)
  res <- request_storefront(
    api = store_api(),
    interface = "friends",
    method = "frienddata",
    params = params
  )
  res <- pivot_longer_list(res, names_to = "appid")
  as_data_frame(res)
}
