#' Enums
#' @description
#' The Steam API defines a number of enums for use with the API. These enums
#' can help resolve some common identifiers such as types and categories.
#'
#' \code{steamkit_enum} scrapes the enums from the
#' \href{https://github.com/SteamRE/SteamKit}{SteamKit} C# library while
#' \code{steamkit_node} scrapes them from the
#' \href{https://github.com/DoctorMcKay/node-steam-user}{SteamUser} Node.js library.
#' SteamKit bundles the enums in different files and SteamUser organizes
#' each enum in a single file. Thus, \code{steamkit_enum} requires a \code{type}
#' argument while \code{node_enum} does not.
#'
#' Some enums are pre-defined, either internally or as exported functions.
#' Non-defined enums can be retrieved using \code{steamkit_enum}.
#'
#' @param enum Name of the enumeration to retrieve. If \code{NULL}, returns
#' the names of available enums.
#' @param type Type of enums. Corresponds to a generated file from
#' SteamKit.
#' @param filter If \code{enum} is \code{NULL}, specifies a keyword to
#' filter the list of available enums by. The keyword is fuzzy-matched
#' using \code{\link{agrepl}}.
#'
#' @returns If \code{enum} is not \code{NULL}, returns a dataframe
#' containing code and descriptions. Otherwise, returns the names of
#' available enums.
#'
#' @export
#'
#' @references
#'
#' \url{https://steam-py.github.io/docs/latest/api/#enumerations}
#'
#' \url{https://partner.steamgames.com/doc/api/steam_api#enums}
#'
#' \url{https://github.com/SteamRE/SteamKit}
#'
#' \url{https://github.com/DoctorMcKay/node-steam-user}
#'
#' @examples
#' # investigate all enums of SteamLanguage.cs
#' steamkit_enum(type = "SteamLanguage")
#'
#' # retrieve all code descriptions for SteamRealm
#' # Steam realms can be passed as arguments in various store endpoints
#' steamkit_enum("SteamRealm", type = "SteamLanguage")
#'
#' # retrieve universe descriptions
#' # can be useful to interpret output of steamid parsing
#' steamkit_enum("Universe", type = "SteamLanguage")
#'
#' # retrieve communty item classes
#' # useful to decipher the output of functions like query_loyalty_rewards
#' steamkit_enum("CommunityItemClass", type = "Enums")
#'
#' # show all enums about published files
#' node_enum(filter = "publishedfile")
#'
#' # node_enum works without specifiying a type
#' node_enum("FriendFlags")
steamkit_enum <- function(enum = NULL,
                          type = c("SteamLanguage", "Enums", "EnumsProductInfo"),
                          filter = NULL) {
  check_string(enum, null = TRUE)
  type <- match.arg(type)
  url <- "https://raw.githubusercontent.com/SteamRE/SteamKit/master/SteamKit2/SteamKit2/Base/Generated/%s.cs"
  url <- sprintf(url, type)
  lines <- readLines(url, warn = FALSE)
  lines <- extract_enum(lines)

  if (is.null(enum)) {
    enum <- names(lines)
    enum <- substr(enum, 2, nchar(enum))

    if (!is.null(filter)) {
      enum <- enum[utils::agrepl(filter, enum, ignore.case = TRUE)]
    }
  } else {
    enum <- lines[[paste0("E", enum)]]
  }

  enum
}


#' @rdname steamkit_enum
#' @export
node_enum <- function(enum = NULL, filter = NULL) {
  check_string(enum, null = TRUE)
  check_length(enum, ge = 0, le = 1)
  all_enums <- get_all_enums()

  if (is.null(enum)) {
    names <- all_enums$name
    if (!is.null(filter)) {
      names <- names[agrepl(filter, names, ignore.case = TRUE)]
    }

    return(names)
  }

  enum <- all_enums[all_enums$name %in% enum, ]
  code <- readLines(enum$download_url)
  parse_node_enum(code)
}


#' @rdname steamkit_enum
#' @export
content_descriptors <- function() {
  as_data_frame(data.frame(
    code = 1:5,
    description = factor(c(
      "Some Nudity or Sexual Content",
      "Frequent Violence or Gore",
      "Adult Only Sexual Content",
      "Frequent Nudity or Sexual Content",
      "General Mature Content"
    ))
  ))
}


#' @rdname steamkit_enum
#' @export
universes <- function() {
  as_data_frame(data.frame(
    code = 0:5,
    desc = factor(c("Invalid", "Public", "Beta", "Internal", "Dev", "RC"))
  ))
}


#' @rdname steamkit_enum
#' @export
account_types <- function() {
  as_data_frame(data.frame(
    code = 0:10,
    letter = c("I / i", "U", "M", "G", "A", "P", "C", "g", "T / L /c", NA, "a"),
    desc = factor(c(
      "Invalid", "Individual", "Multiseat", "GameServer", "AnonGameServer",
      "Pending", "ContentServer", "Clan", "Chat", "ConsoleUser", "AnonUser"
    )),
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


account_instances <- function() {
  as_data_frame(data.frame(
    code = c(0, 1, 2, 4, 8, 16, 32, 64, 128),
    desc = factor(c(
      "AllInstances", "DesktopInstance", "ConsoleInstance", "WebInstance",

    ))
  ))
}


#' @rdname steamkit_enum
#' @export
EFriendRelationship <- function() {
  as_data_frame(data.frame(
    code = 0:7,
    desc = factor(c(
      "None", "Blocked", "RequestRecipient", "Friend", "RequestInitiator",
      "Ignored", "IgnoredFriend", "SuggestedFriend"
    ))
  ))
}


#' @rdname steamkit_enum
#' @export
ECurrency <- function() {
  as_data_frame(data.frame(
    code = 0:41,
    desc = factor(c(
      "Invalid", "USD", "GBP", "EUR", "CHF", "RUB", "PLN", "BRL", "JPY", "NOK",
      "IDR", "MYR", "PHP", "SGD", "THB", "VND", "KRW", "TRY", "UAH", "MXN", "CAD",
      "AUD", "NZD", "CNY", "INR", "CLP", "PEN", "COP", "ZAR", "HKD", "TWD", "SAR",
      "AED", "SEK", "ARS", "ILS", "BYN", "KZT", "KWD", "QAR", "CRC", "UYU"
    ))
  ))
}


#' @rdname steamkit_enum
#' @export
EPublishedFileQueryType <- function() {
  as_data_frame(data.frame(
    code = 0:19,
    desc = factor(c(
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
    ))
  ))
}


EPublishedFileRevision <- function() {
  as_data_frame(data.frame(
    code = 0:5,
    desc = c(
      "Default", "Latest", "ApprovedSnapshot",
      "ApprovedSnapshotChina", "RejectedSnapshot",
      "RejectedSnapshotChina"
    )
  ))
}


ELanguage <- function() {
  as_data_frame(data.frame(
    code = -1:28,
    desc = c(
      "NONE", "English", "German", "French", "Italian", "Korean", "Spanish",
      "SimplifiedChinese", "TraditionalChinese", "Russian", "Thai",
      "Japanese", "Portuguese", "Polish", "Danish", "Dutch", "Finnish",
      "Norwegian", "Swedish", "Romanian", "Turkish", "Hungarian", "Czech",
      "PortugueseBrazil", "Bulgarian", "Greek", "Arabic", "Ukrainian",
      "SpanishLatinAmerican", "Vietnamese"
    )
  ))
}


EPublishedFileInfoMatchingFileType <- function() {
  as_data_frame(data.frame(
    code = 0:20,
    desc = c(
      "Items", "Collections", "Art", "Videos", "Screenshots",
      "CollectionEligible", "Games", "Software", "Concepts",
      "GreenlightItems", "AllGuides", "WebGuides", "IntegratedGuides",
      "UsableInGame", "Merch", "ControllerBindings", "SteamworkAccessInvites",
      "Items_Mtx", "Items_ReadyToUse", "WorkshopShowcase", "GameManagedItems"
    ),
    info = c(
      "Items", "A collection of workshop items", "Artwork", "Videos",
      "Screenshots", "Items that can be put inside a collection",
      "Unsued", "Unused", "Unused", "Unused", "Guides", "Steam web guide",
      "Application integrated guide", NA,
      "Workshop merchandise meant to be voted on for the purpose of being sold",
      "Steam controller bindings", "Used internally",
      "Workshop items that can be sold in-game",
      "Workshop items that can be used right away by the user", NA,
      "Managed completely be the game, not the user, and not shown on the web"
    )
  ))
}


EProfileTypeItem <- function() {
  as_data_frame(data.frame(
    code = 0:24,
    desc = c(
      "Invalid", "RareAchievementShowcase", "GameCollector", "ItemShowcase",
      "TradeShowcase", "Badges", "FavouriteGame", "ScreenshotShowcase",
      "CustomText", "FavouriteGroup", "Recommendation", "WorkshopItem",
      "MyWorkshop", "ArtworkShowcase", "VideoShowcase", "Guides",
      "MyGuides", "Achievements", "Greenlight", "MyGreenlight", "Salien",
      "LoyaltyRewardReactions", "SingleArtworkShowcase",
      "AchievementsCompletionist", "Replay"
    )
  ))
}


ECommunityItemClass <- function() {
  as_data_frame(data.frame(
    code = 0:16,
    desc = c(
        "Invalid", "Badge", "GameCard", "ProfileBackground", "Emoticon",
        "BoosterPack", "Consumable", "GameGoo", "ProfileModifier", "Scene",
        "SalienItem", "Sticker", "ChatEffect", "MiniProfileBackground",
        "AvatarFrame", "AnimatedAvatar", "SteamDeckKeyboardSkin"
    )
  ))
}


#' @rdname steamkit_enum
#' @export
user_badges <- function() {
  as_data_frame(data.frame(
    code = 0:44,
    desc = factor(c(
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
    ))
  ))
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
    df$desc <- factor(df$desc)
    df <- df[!df$code %in% "[Obsolete]", ]
    as_data_frame(df)
  })

  names(lines) <- enums
  lines
}


get_all_enums <- function() {
  url <- "https://api.github.com/repos/DoctorMcKay/node-steam-user/contents/enums"
  enum_files <- jsonlite::read_json(url, simplifyVector = TRUE)
  enum_files$name <- gsub("\\.js$", "", enum_files$name)
  enum_files$name <- gsub("^E", "", enum_files$name)
  enum_files[c("name", "download_url")]
}


parse_node_enum <- function(code) {
  # remove trailing whitespace and empty lines
  code <- trimws(code)
  code <- code[nzchar(code)]

  # remove all comments and excess js code
  code <- code[!grepl("^/|\\*|module|const|}", code)]

  proto <- data.frame(desc = character(), code = character())
  enum <- utils::strcapture("\"?([A-Za-z]+)\"?: ([0-9]+),?$", code, proto)
  enum <- enum[c("code", "desc")]
  as_data_frame(enum[!is.na(enum$code), ])
}
