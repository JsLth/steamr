#' @rdname parse_steamkit_enum
#' @export
content_descriptors <- function() {
  as_data_frame(data.frame(
    code = 1:5,
    description = c(
      "Some Nudity or Sexual Content",
      "Frequent Violence or Gore",
      "Adult Only Sexual Content",
      "Frequent Nudity or Sexual Content",
      "General Mature Content"
    )
  ))
}


#' @rdname parse_steamkit_enum
#' @export
universes <- function() {
  as_data_frame(data.frame(
    code = 0:5,
    desc = c("Invalid", "Public", "Beta", "Internal", "Dev", "RC")
  ))
}


#' @rdname parse_steamkit_enum
#' @export
account_types <- function() {
  as_data_frame(data.frame(
    code = 0:10,
    letter = c("I / i", "U", "M", "G", "A", "P", "C", "g", "T / L /c", NA, "a"),
    desc = c(
      "Invalid", "Individual", "Multiseat", "GameServer", "AnonGameServer",
      "Pending", "ContentServer", "Clan", "Chat", "ConsoleUser", "AnonUser"
    ),
    usable = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, NA, TRUE, TRUE, FALSE, TRUE),
    path = c(NA, "profiles / id", NA, NA, NA, NA, NA, "groups / gid", NA, NA, NA),
    info = c(
      "Used for invalid Steam IDs",
      "Single user account",
      "Multiseat (e.g. cybecafe) account",
      "Game server account",
      "Anonymous game server account",
      "Pending",
      "Content server",
      "Steam group (clan)",
      "Steam group chat or lobby",
      "Fake Steam ID for local PSN account on PS3 or Live account on 360, etc.",
      "Anonymous user account (Used to create an account or reset a password)"
    )
  ))
}


#' @rdname parse_steamkit_enum
#' @export
friend_relationships <- function() {
  as_data_frame(data.frame(
    code = c(0:7),
    desc = c(
      "None", "Blocked", "RequestRecipient", "Friend", "RequestInitiator",
      "Ignored", "IgnoredFriend", "SuggestedFriend"
    )
  ))
}


#' @rdname parse_steamkit_enum
#' @export
steam_currencies <- function() {
  as_data_frame(data.frame(
    code = 0:41,
    desc = c(
      "Invalid", "USD", "GBP", "EUR", "CHF", "RUB", "PLN", "BRL", "JPY", "NOK",
      "IDR", "MYR", "PHP", "SGD", "THB", "VND", "KRW", "TRY", "UAH", "MXN", "CAD",
      "AUD", "NZD", "CNY", "INR", "CLP", "PEN", "COP", "ZAR", "HKD", "TWD", "SAR",
      "AED", "SEK", "ARS", "ILS", "BYN", "KZT", "KWD", "QAR", "CRC", "UYU"
    )
  ))
}


#' @rdname parse_steamkit_enum
#' @export
published_file_query_types <- function() {
  as_data_frame(data.frame(
    code = 0:19,
    desc = c(
      "RankedByVote", "RankedByPublicationDate",
      "AcceptedForGameRankedByAcceptanceDate", "RankedByTrend",
      "FavoritedByFriendsRankedByPublicationDate",
      "CreatedByFriendsRankedByPublicationDate", "RankedByNumTimesReported",
      "CreatedByFollowedUsersRankedByPublicationDate", "NotYetRated",
      "RankedByTotalUniqueSubscriptions", "RankedByTotalVotesAsc",
      "RankedByVotesUp", "RankedByTextSearch", "RankedByPlaytimeTrend",
      "RankedByTotalPlaytime", "RankedByAveragePlaytimeTrend",
      "RankedByLifetimeAveragePlaytime", "RankedByPlaytimeSessionsTrend",
      "RankedByLifetimePlaytimeSessions", "RankedByInappropriateContentRating"
    )
  ))
}


#' @rdname parse_steamkit_enum
#' @export
user_badges <- function() {
  as_data_frame(data.frame(
    code = 0:44,
    desc = c(
      "Invalid", "YearsOfService", "Community", "Portal2PotatoARG", "TreasureHunt",
      "SummerSale2011", "WinterSale2011", "SummerSale2012", "WinterSale2012",
      "CommunityTranslator", "CommunityModerator", "ValveEmployee", "GameDeveloper",
      "GameCollector", "TradingCardBetaParticipant", "SteamBoxBeta",
      "Summer2014RedTeam", "Summer2014BlueTeam", "Summer2014PinkTeam",
      "Summer2014GreenTeam", "Summer2014PurpleTeam", "Auction2014",
      "GoldenProfile2014", "TowerAttackMiniGame", "Winter2015ARG_RedHerring",
      "SteamAwards2016Nominations", "StickerCompletionist2017",
      "SteamAwards2017Nominations", "SpringCleaning2018", "Salien",
      "RetiredModerator", "SteamAwards2018Nominations", "ValveModerator",
      "WinterSale2018", "LunarNewYearSale2019", "LunarNewYearSale2019GoldenProfile",
      "SpringCleaning2019", "SummerSale2019", "SummerSale2019_TeamHare",
      "SummerSale2019_TeamTortoise", "SummerSale2019_TeamCorgi",
      "SummerSale2019_TeamCockatiel", "SummerSale2019_TeamPig",
      "SteamAwards2019Nominations", "WinterSaleEvent2019"
    )
  ))
}


#' Enums
#' @description
#' The Steam API defines a number of enums for use with the API. These enums
#' can help resolve some common identifiers such as types and categories.
#' The enums are scraped from the C# library
#' \href{https://github.com/SteamRE/SteamKit}{SteamKit}, which
#' auto-generates enums from protobufs.
#'
#' Some enums are pre-defined, either internally or as exported functions.
#' Non-defined enums can be retrieved using \code{parse_steamkit_enum}.
#'
#' @param enum Name of the enumeration to retrieve. If \code{NULL}, returns
#' the names of available enums.
#' @param type Type of enums. Corresponds to a generated file from
#' SteamKit.
#'
#' @returns If \code{enum} is not \code{NULL}, returns a dataframe
#' containing code and descriptions. Otherwise, returns the names of
#' available enums.
#'
#' @export
#'
#' @examples
#' # investigate all enums of SteamLanguage.cs
#' parse_steamkit_enum(type = "SteamLanguage")
#'
#' # retrieve all code descriptions for SteamRealm
#' # Steam realms can be passed as arguments in various store endpoints
#' parse_steamkit_enum("SteamRealm", type = "SteamLanguage")
#'
#' # retrieve universe descriptions
#' # can be useful to interpret output of steamid parsing
#' parse_steamkit_enum("Universe", type = "SteamLanguage")
#'
#' # retrieve communty item classes
#' # useful to decipher the output of functions like query_loyalty_rewards
#' parse_steamkit_enum("CommunityItemClass", type = "Enums")
parse_steamkit_enum <- function(enum = NULL,
                                type = c("Enums", "EnumsProductInfo", "SteamLanguage")) {
  check_string(enum, null = TRUE)
  type <- match.arg(type)
  url <- "https://raw.githubusercontent.com/SteamRE/SteamKit/master/SteamKit2/SteamKit2/Base/Generated/%s.cs"
  url <- sprintf(url, type)
  lines <- readLines(url, warn = FALSE)
  lines <- extract_enum(lines)

  if (is.null(enum)) {
    enum <- names(lines)
    enum <- substr(enum, 2, nchar(enum))
  } else {
    enum <- lines[[paste0("E", enum)]]
  }

  enum
}


extract_enum <- function(lines) {
  lines <- trimws(lines)
  pubenum <- "public enum"
  in_enum <- FALSE
  lines <- lapply(lines, function(x) {
    if (startsWith(x, "{")) {
      return(NULL)
    }

    if (!in_enum && startsWith(x, pubenum)) {
      in_enum <<- TRUE
    }

    if (in_enum && startsWith(x, "}")) {
      in_enum <<- FALSE
    }

    if (in_enum) {
      x
    }
  })

  lines <- unlist(lines)

  # create groups by which to split
  # a group starts when a line starts with public enum and ends with a
  # closing curly bracket
  groups <- cumsum(startsWith(lines, pubenum))
  lines <- split(lines, groups)

  # set names
  enums <- cvapply(lines, "[[", 1)

  # remove public enum
  enums <- trimws(gsub(sprintf("%s(.+)", pubenum), "\\1", enums))

  # remove return type
  enums <- gsub("( : .+)?$", "", enums)
  names(lines) <- enums

  lines <- lapply(enums, function(enum) {
    x <- lines[[enum]]
    x <- x[-1]

    # trim trailing E
    enum <- substr(enum, 2, nchar(enum))

    # trim trailing enum name
    x <- gsub(sprintf("k_E?%s(_?)", enum), "", x)
    x <- gsub(",$", "", x)
    x <- strsplit(x, " = ")
    df <- do.call(rbind.data.frame, x)
    df <- df[c(2, 1)]
    names(df) <- c("code", "desc")
    df <- df[!df$code %in% "[Obsolete]", ]
    df
  })

  names(lines) <- enums
  lines
}
