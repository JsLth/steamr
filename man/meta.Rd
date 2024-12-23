% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{meta}
\alias{meta}
\alias{steam_stats}
\alias{wba_methods}
\alias{wba_servertime}
\alias{wba_servers}
\title{Meta info}
\usage{
steam_stats()

wba_methods()

wba_servertime()

wba_servers(filter = NULL, limit = NULL)
}
\value{
\itemize{
 \item{\code{wba_methods}: An named list where each
 index contains a named list with information on the respective API
 endpoint.}

 \item{\code{wba_servertime}: A \code{POSIXct} value.}

 \item{\code{steam_stats}: A named list of length 2}

 \item{\code{wba_servers}: A dataframe.}

}
}
\description{
Fetch metadata about the Steam server.

\itemize{
 \item{\code{wba_methods}: returns information about all supported
API endpoints of the web API. A complete list including undocumented
endpoints can be found on \url{https://steamapi.xpaw.me/}}
 \item{\code{wba_servertime}: returns the current Steam server time}
 \item{\code{steam_stats} returns the current users online and ingame}
 \item{\code{wba_servers} returns details about running game servers}
}
}
\examples{
\donttest{# details on low-level method for `wba_news`
methods <- wba_methods()
methods$ISteamNews$GetNewsForApp

# get the current server time
wba_servertime()

# get the current user number
steam_stats()

# get details about game servers
get_servers()}
}
\section{Authentication}{
The functions of this reference page do not need any kind of authentication to be used (Key = API key needed, Login = user login needed).
\tabular{llll}{
    \strong{Function} \tab \strong{Key} \tab \strong{Login} \tab \strong{Note}\cr
    \code{wba_methods}    \tab no \tab no \tab Providing a publisher key returns additional publisher-only methods\cr
    \code{wba_servertime} \tab no \tab no \tab                                                                    \cr
    \code{steam_stats}    \tab no \tab no \tab                                                                    \cr
    \code{wba_servers}    \tab no \tab no \tab                                                                    
}




}
