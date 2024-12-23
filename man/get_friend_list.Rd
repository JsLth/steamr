% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/friends.R
\name{get_friend_list}
\alias{get_friend_list}
\alias{get_friends_playtimes}
\alias{get_friends_recommendations}
\alias{get_friends}
\alias{get_friend_data}
\title{Friends}
\usage{
get_friend_list(steamid, relationship = "all")

get_friends_playtimes(appid)

get_friends_recommendations(appid)

get_friends()

get_friend_data()
}
\arguments{
\item{steamid}{SteamID of a user. The SteamID must be in a format that can
be converted by \code{\link{convert_steamid}}. This includes vanity,
Steam64, Steam2, and Steam3 IDs.}

\item{relationship}{Type of relationship to filter for. If \code{all},
does not filter. A list of available relationships can be retrieved
using \code{\link{steamkit_enum}("FriendRelationship", type = "SteamLanguage")}.}

\item{appid}{Application ID referencing a Steam application. There are
generally two ways of retrieving an appID:
\itemize{
 \item{Inspect or parse the store URL of an application, e.g. \code{https://store.steampowered.com/app/10/CounterStrike/}}
 \item{Query appIDs programmatically, e.g. using \code{\link{search_apps}}}
}}
}
\value{
\describe{
 \item{\code{get_friend_list}}{A dataframe of all friends of a user
 along with their SteamIDs, relationship types and the date of
 friendship.}

 \item{\code{get_friends_playtimes}}{A dataframe of all friends that
 have played a specific game and their playtimes. If a playtime is
 \code{NA}, then the friend only owns the game but has not played it.}

 \item{\code{get_friends_recommendations}}{A vector of friend SteamIDs
 that have written a recommendation about a game.}

 \item{\code{get_friends}}{A dataframe containing the SteamIDs of friends,
 their vanity ID and the avatar URL.}

 \item{\code{get_friend_data}}{A dataframe containing the appID, SteamID
 of the friend, type of relationship to the application, and the amount of
 time played. If a playtime is \code{NA}, then the friend only owns the game
 but has not played it. The \code{type} column can take three values:

 1 -- Played the game

 2 -- Recently bought the game (?)

 3 -- Owns the game}
}
}
\description{
Get friend lists, retrieve friends' game playtimes, recommendations and
general user data.

Of these functions \code{get_friend_list} is the only non-authenticated
function. \code{get_friends} is similar in concept, but requires
authentication and delivers more detailed results.
}
\examples{
\dontrun{
# get the friend list of any user
get_friend_list("76561197960435530")

# need authentication
auth_credentials("username")

# how much did user's friends play team fortress?
get_friends_playtimes(440)

# which friends recommended team fortress?
get_friends_recommendations(440)

# more detailed list of user's friends
get_friends()
}
}
\seealso{
\code{\link{friends_in_review}}
}
