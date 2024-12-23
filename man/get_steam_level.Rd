% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_level.R
\name{get_steam_level}
\alias{get_steam_level}
\alias{steam_level_distribution}
\alias{get_level_percentile}
\title{Steam levels}
\usage{
get_steam_level(steamid)

steam_level_distribution(levels = 1:100)

get_level_percentile(level)
}
\arguments{
\item{steamid}{SteamID of a user. The SteamID must be in a format that can
be converted by \code{\link{convert_steamid}}. This includes vanity,
Steam64, Steam2, and Steam3 IDs.}

\item{levels}{A vector of user levels to show the distribution of.}

\item{level}{User level to retrieve.}
}
\value{
\code{get_steam_level} and \code{get_level_percentile} return
a length-1 numeric vector. \code{steam_level_distribution} returns a numeric
vector of length \code{length(levels)}.
}
\description{
Get steam levels of users, their percentiles and distributions.

\code{get_steam_level} retrieves the level of a user.

\code{steam_level_percentile} retrieves the percentile of a user level
compared to all other user levels.

\code{steam_level_distribution} retrieves the user level percentile for
a number of levels and shows their distribution.
}
\examples{
\dontrun{
steamid <- "76561197984981409"

# get level of a user
get_steam_level("76561197984981409")

# get percentile of a user
get_level_percentile("76561197984981409")

# plot distribution
dist <- steam_level_distribution(1:20)
plot(dist)
}
}
