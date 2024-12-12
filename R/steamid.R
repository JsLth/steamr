#' Get User ID
#' @description
#' Retrieves the Steam ID64 of a user profile, group or game hub based on
#' vanity IDs.
#'
#' @param name Name / Vanity ID of a user account, group or game hub.
#' See details and examples.
#' @param type Type of Steam name. \code{profile} returns the Steam ID of
#' a user profile, \code{group} returns the ID of a public group and
#' \code{game_group} returns the ID of a game's official game hub.
#'
#' @returns A length-1 character vector containing the Steam ID corresponding
#' to the input name.
#'
#' @details
#' There are various way of retrieving vanity IDs depending on the type of
#' vanity URL type. The vanity URL of a Steam user is the account name
#' (not the display name). It can be retrieved by inspecting the profile URL:
#' \preformatted{https://steamcommunity.com/id/{vanity_id}/}
#'
#' Vanity IDs of groups can be retrieved in a similar way:
#' \preformatted{https://steamcommunity.com/groups/{vanity_id}/}
#'
#' Vanity IDs of game hubs are not easily locatable as game hubs are
#' closely linked to store pages. They can be found by inspecting the
#' source code of a game page and searching for \code{VANITY_ID}. Vanity IDs
#' of game hubs are usually the application ID or an abbreviation of the
#' original title, e.g. \code{dota2} for DOTA 2, \code{TF2} for Team Fortress 2
#' or simply \code{70} for Half-Life
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # get user ID
#' resolve_vanity("gabelogannewell")
#'
#' # get group ID
#' resolve_vanity("SteamDB", type = "group")
#'
#' # get game hub ID
#' resolve_vanity("TF2", type = "game_group")
#' }
wba_resolve_vanity <- function(name, type = "profile") {
  check_steam_key()
  check_string(name)
  check_string(type)

  type <- switch(type, profile = 1, group = 2, game_group = 3)
  params <- .make_params(vanityurl = name, url_type = type, access_token = FALSE)
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUser",
    method = "ResolveVanityURL",
    version = "v1",
    params = params
  )

  code <- res$response$success
  if (!identical(code, 1L)) {
    msg <- res$response$message
    stop(sprintf("Could not resolve vanity URL. Error code %s: %s", code, msg))
  }

  res$response$steamid
}#' Get User ID
#' @description
#' Retrieves the Steam ID64 of a user profile, group or game hub based on
#' vanity IDs.
#'
#' @param name Name / Vanity ID of a user account, group or game hub.
#' See details and examples.
#' @param type Type of Steam name. \code{profile} returns the Steam ID of
#' a user profile, \code{group} returns the ID of a public group and
#' \code{game_group} returns the ID of a game's official game hub.
#'
#' @returns A length-1 character vector containing the Steam ID corresponding
#' to the input name.
#'
#' @details
#' There are various way of retrieving vanity IDs depending on the type of
#' vanity URL type. The vanity URL of a Steam user is the account name
#' (not the display name). It can be retrieved by inspecting the profile URL:
#' \preformatted{https://steamcommunity.com/id/{vanity_id}/}
#'
#' Vanity IDs of groups can be retrieved in a similar way:
#' \preformatted{https://steamcommunity.com/groups/{vanity_id}/}
#'
#' Vanity IDs of game hubs are not easily locatable as game hubs are
#' closely linked to store pages. They can be found by inspecting the
#' source code of a game page and searching for \code{VANITY_ID}. Vanity IDs
#' of game hubs are usually the application ID or an abbreviation of the
#' original title, e.g. \code{dota2} for DOTA 2, \code{TF2} for Team Fortress 2
#' or simply \code{70} for Half-Life
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # get user ID
#' resolve_vanity("gabelogannewell")
#'
#' # get group ID
#' resolve_vanity("SteamDB", type = "group")
#'
#' # get game hub ID
#' resolve_vanity("TF2", type = "game_group")
#' }
resolve_vanity <- function(name, type = "profile") {
  check_steam_key()
  check_string(name)
  check_string(type)

  type <- switch(type, profile = 1, group = 2, game_group = 3)
  params <- .make_params(vanityurl = name, url_type = type, access_token = FALSE)
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamUser",
    method = "ResolveVanityURL",
    version = "v1",
    params = params
  )

  code <- res$response$success
  if (!identical(code, 1L)) {
    msg <- res$response$message
    stop(sprintf("Could not resolve vanity URL. Error code %s: %s", code, msg))
  }

  res$response$steamid
}


#' @rdname convert_steamid
#' @param include_vanity Whether to include vanity IDs in the output of
#' \code{lookup_steamid}. Useful to prevent Web API lookups.
#' @export
lookup_steamid <- function(ids, include_vanity = TRUE, vanity_type = "profile") {
  ids <- lapply(ids, function(x) {
    as_data_frame(drop_null(list(
      steam64 = convert_steamid(x, to = "steam64", vanity_type),
      steam2 = convert_steamid(x, to = "steam2", vanity_type),
      steam3 = convert_steamid(x, to = "steam3", vanity_type),
      vanity = if (include_vanity) convert_steamid(x, to = "vanity")
    )))
  })
  rbind_list(ids)
}


#' Convert Steam ID
#' @description
#' Converts between Steam ID64, Steam2, Steam3 and vanity IDs. Based on
#' the \href{https://github.com/xPaw/SteamID.php}{SteamID} library by xPaw.
#'
#' \code{convert_steamid} simply converts a vector of Steam IDs while
#' \code{lookup_steamid} compiles all types of Steam IDs for all input
#' IDs.
#'
#' @param ids String of a Steam ID. Can be ID64, Steam2, Steam3, or a vanity ID.
#' The format is automatically detected. If not format can be identified,
#' assumes vanity.
#' @param to Steam ID format to convert to. Must be one of \code{steam64},
#' \code{steam2}, \code{steam3}, or \code{vanity}. Conversions from or to
#' vanity require a Steam API key to be set.
#' @param vanity_type Type of profile identified by the vanity ID. Passed to
#' \code{\link{resolve_vanity}}. Ignored if \code{ids} does not contain
#' vanity IDs.
#'
#' @details
#' Steam knows four types of IDs (excluding invite codes):
#' \itemize{
#'  \item{ID64 is largely used by the Steam Web API and storefront API.
#'  It is a 64-bit representation of a profile that can also be used
#'  in steamcommunity URLs.}
#'  \item{Steam2 is a textual representation of a profile that consists
#'  of descriptive components: the universe, authentication server, and
#'  account number.}
#'  \item{Steam3 is a newer textual format that is used by many games.
#'  It consists of account type, universe, account number, and account
#'  instance.}
#'  \item{Vanity IDs are the self-chosen account names of an individual
#'  or group. They can be found in steamcommunity URLs. To resolve or
#'  convert them, access to Steam's Web API is required.}
#' }
#'
#' Each Steam ID explicitly or implicitly consists of several components:
#' \itemize{
#'  \item{The authentication server (0 or 1)}
#'  \item{The account number}
#'  \item{The account instance (number between 0 and 4)}
#'  \item{The account type (number between 0 and 10)}
#'  \item{The universe (number between 0 and 5)}
#' }
#'
#' The \code{parse_*} functions are able to parse the individual components of
#' Steam64, Steam2, and Steam3 IDs. The meaning of each number is finely
#' documented in the source code of the
#' \href{SteamID PHP library}{https://github.com/xPaw/SteamID.php/blob/master/SteamID.php}
#'
#' @section Steam ID components:
#' A Steam ID consists of five components. The auth server (`auth`) can be
#' either 0 or 1. The account number (`number`) is a unique identifier for the
#' account.
#'
#' Instance values correspond to the following meanings:
#'
#' \tabular{cl}{
#'   \strong{Instance} \tab \strong{Description} \cr
#'   0 \tab All instances   \cr
#'   1 \tab Desktop instance\cr
#'   2 \tab Console instance\cr
#'   4 \tab Web instance
#' }
#'
#' Types values correspond to the following meanings:
#'
#' \tabular{cl}{
#'   \strong{Type} \tab \strong{Description} \cr
#'    0 \tab Invalid              \cr
#'    1 \tab Individual           \cr
#'    2 \tab Multiseat            \cr
#'    3 \tab Game server          \cr
#'    4 \tab Anonymous game server\cr
#'    5 \tab Pending              \cr
#'    6 \tab Content server       \cr
#'    7 \tab Clan                 \cr
#'    8 \tab Chat                 \cr
#'    9 \tab P2P super seeder     \cr
#'   10 \tab Anonymous user
#' }
#'
#' Universe values correspond to the following meanings:
#'
#' \tabular{cl}{
#'   \strong{Universe} \tab \strong{Description} \cr
#'   0 \tab Invalid \cr
#'   1 \tab Public  \cr
#'   2 \tab Beta    \cr
#'   3 \tab Internal\cr
#'   4 \tab Dev
#' }
#'
#' @note
#' This function is not perfect, but should suffice for most use cases.
#' Special cases and older profiles sometimes return the wrong account number
#' or universe.
#'
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' convert_steamid("vgenkin", "steam64") |>
#'   convert_steamid("steam2") |>
#'   convert_steamid("steam3") |>
#'   convert_steamid("vanity")
#'
#' lookup_steamid("vgenkin")
#' }
convert_steamid <- function(ids, to, vanity_type = "profile") {
  vapply(unname(ids), FUN.VALUE = character(1), function(x) {
    if (is_steam64(x)) {
      switch(
        to,
        steam2 = steam64_to_steam2(x),
        steam3 = steam64_to_steam3(x),
        vanity = steam64_to_vanity(x),
        x
      )
    } else if (is_steam2(x)) {
      steam64 <- steam2_to_steam64(x)
      switch(
        to,
        steam64 = steam64,
        steam3 = steam64_to_steam3(steam64),
        vanity = steam64_to_vanity(steam64),
        x
      )
    } else if (is_steam3(x)) {
      steam64 <- steam3_to_steam64(x)
      switch(
        to,
        steam64 = steam64,
        steam2 = steam64_to_steam2(steam64),
        vanity = steam64_to_vanity(steam64),
        x
      )
    } else {
      steam64 <- resolve_vanity(x, type = vanity_type)
      switch(
        to,
        steam64 = steam64,
        steam2 = steam64_to_steam2(steam64),
        steam3 = steam64_to_steam3(steam64),
        x
      )
    }
  })
}


#' Steam ID checks
#' @description
#' Checks if a given string can be parsed as a Steam ID. Functions are defined
#' for ID64, Steam2, Steam3, and vanity IDs.
#'
#' @param x A character string that is to be checked.
#'
#' @details
#' \code{is_steam64} tests if a character string is a 64-bit representation
#' of a decimal number.
#' \code{is_steam2} and \code{is_steam3} test if \code{x} follows the
#' string format of textual Steam ID representations as described
#' \href{https://developer.valvesoftware.com/wiki/SteamID#As_Represented_Textually}{here}.
#' \code{is_vanity} returns \code{TRUE}, if all other functions return
#' \code{FALSE}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' is_steam64("76561197984981409", "12345") # TRUE, FALSE
#' is_steam2("STEAM_1:1:12357840", "STEAM_5:6:123") # TRUE, FALSE
#' is_steam3("[U:1:24715681]", "U:4:1234") # TRUE, FALSE
#' is_vanity("12345", "76561197984981409") # TRUE, FALSE
#' }
is_steam64 <- function(x) {
  if (!isTRUE(grepl("^[0-9]+$", x))) return(FALSE)
  binlen <- vapply(x, FUN.VALUE = numeric(1), function(s) {
    length(bigBits::buildBinaries(s, inBase = 10)$xbin)
  }, USE.NAMES = FALSE)
  is_64 <- binlen >= 60 & binlen <= 64

  if (is_64 && is.numeric(x)) {
    stop(paste(
      "Steam ID identified as Steam64 but value is numeric.\n",
      "To preserve numeric precision, pass Steam ID as a character string."
    ))
  }

  is_64
}


#' @rdname is_steam64
#' @export
is_steam2 <- function(x) {
  # 1. universe
  # 2. Y
  # 3. account number
  grepl("STEAM_[0-6]:[0-1]:[0-9]+", x)
}


#' @rdname is_steam64
#' @export
is_steam3 <- function(x) {
  # 1. account type char
  # 2. universe
  # 3. community id
  # 4. instance
  grepl("^\\[[IiUMGAPCgTLca]:[0-6]:[0-9]+(:[0124])?\\]$", x)
}


#' @rdname is_steam64
#' @export
is_vanity <- function(x) {
  !is_steam64(x) & !is_steam2(x) & !is_steam3(x)
}


#' @rdname convert_steamid
#' @export
#' @param resolve If \code{TRUE}, resolves the codes parsed by the
#' \code{parse_*} functions to descriptive factors.
parse_steam64 <- function(ids, resolve = FALSE) {
  parsed <- rbind_list(lapply(ids, function(id) {
    if (!is_steam64(id)) {
      stop(sprintf("Value %s is not a valid Steam64 ID.", id))
    }

    bin <- bigBits::buildBinaries(id, inBase = 10)$xbin
    bin <- paste(bin, collapse = "")
    add <- strrep("0", 64 - nchar(bin))
    bin <- paste0(add, bin)
    n <- nchar(bin)

    y <- substr(bin, n, n)
    num <- substr(bin, n - 31, n - 1)
    inst <- substr(bin, n - 51, n - 32)
    type <- substr(bin, n - 55, n - 52)
    univ <- substr(bin, n - 64, n - 56)

    as_data_frame(list(
      auth = bin_to_dec(y),
      number = bin_to_dec(num),
      instance = bin_to_dec(inst),
      type = bin_to_dec(type),
      universe = bin_to_dec(univ)
    ))
  }))

  if (resolve) {
    parsed <- resolve_steamid_components(parsed)
  }

  parsed
}


#' @rdname convert_steamid
#' @export
parse_steam2 <- function(ids, resolve = FALSE) {
  parsed <- rbind_list(lapply(ids, function(id) {
    if (!is_steam2(id)) {
      stop(sprintf("Value %s is not a valid Steam2 ID.", id))
    }

    match <- utils::strcapture(
      "^STEAM_([0-4]):([0-1]):(0|[1-9][0-9]{0,9})$",
      id,
      proto = list(
        univ = integer(),
        auth = integer(),
        num = integer()
      )
    )

    as_data_frame(list(
      auth = match$auth,
      number = match$num,
      instance = 1,
      type = 1,
      universe = match$univ
    ))
  }))

  if (resolve) {
    parsed <- resolve_steamid_components(parsed)
  }

  parsed
}


#' @rdname convert_steamid
#' @export
parse_steam3 <- function(ids, resolve = FALSE) {
  parsed <- rbind_list(lapply(ids, function(id) {
    if (!is_steam3(id)) {
      stop(sprintf("Value %s is not a valid Steam3 ID.", id))
    }

    types <- strtoi(names(type_chars))
    names(types) <- type_chars
    match <- utils::strcapture(
      "^\\[([AGMPCgcLTIUai]):([0-4]):(0|[1-9][0-9]{0,9})([0-9]+)?\\]$",
      id,
      proto = list(
        type = character(),
        univ = integer(),
        num = integer(),
        inst = integer()
      )
    )

    if (all(is.na(match))) {
      stop("Failed to parse: invalid Steam3 ID.")
    }

    type <- match$type
    univ <- match$univ
    num <- bitwShiftR(match$num, 1)

    if (identical(type, "i")) {
      type <- "I"
    }

    if (type %in% c("T", "g")) {
      inst <- 0
    } else if (!is.na(match$inst)) {
      inst <- match$inst
    } else if (type %in% "U") {
      inst <- 1
    } else {
      inst <- 0
    }

    if (type %in% "c") {
      inst <- 524288L
    } else if (type %in% "L") {
      inst <- 262144L
    } else {
      type <- types[[type]]
    }

    as_data_frame(list(
      auth = bitwAnd(num, 1),
      number = num,
      instance = inst,
      type = type,
      universe = univ
    ))
  }))

  if (resolve) {
    parsed <- resolve_steamid_components(parsed)
  }

  parsed
}


resolve_steamid_components <- function(x) {
  inst <- account_instances()
  univ <- universes()
  type <- account_types()

  n <- nrow(x)
  x$instance <- inst[rep(which(inst$code == x$instance), n), ]$desc
  x$type <- type[rep(which(type$code == x$type), n), ]$desc
  x$universe <- univ[rep(which(univ$code == x$universe), n), ]$desc
  x
}


bin_to_dec <- function(x) {
  strtoi(x, base = 2)
}


steam64_to_steam2 <- function(id) {
  type <- get_account_type(id)
  if (identical(as.character(type), "1")) {
    universe <- get_account_universe(id)
    acc_id <- get_account_id(id)
    sprintf(
      "STEAM_%s:%s:%s",
      universe,
      bitwAnd(acc_id, 1),
      bitwShiftR(acc_id, 1)
    )
  } else {
    NA_character_
  }
}


steam64_to_steam3 <- function(id) {
  instance <- get_account_instance(id)
  type <- as.character(get_account_type(id))
  type_char <- type_chars[type] %NA% "i"

  render_instance <- FALSE
  switch(
    type_char,
    "T" = {
      inst <- get_account_instance(id)
      if (bitwAnd(inst, 524288)) {
        type_char <- "c"
      } else if (bitwAnd(inst, 262144)) {
        type_char <- "L"
      }
    },
    "A" = NULL,
    "M" = render_instance <- TRUE
  )

  universe <- get_account_universe(id)
  acc_id <- get_account_id(id)
  out <- sprintf("[%s:%s:%s", type_char, universe, acc_id)

  if (render_instance) {
    out <- paste0(out, ":", instance)
  }

  paste0(out, "]")
}


steam64_to_vanity <- function(id) {
  basename(get_player_summary(id)$profileurl)
}


steam2_to_steam64 <- function(id) {
  comp <- parse_steam2(id)
  parsed <- gmp::as.bigq(0)
  num <- bitwOr(bitwShiftL(strtoi(comp$number), 1), 1)

  parsed <- set_account_universe(parsed, comp$universe)
  parsed <- set_account_instance(parsed, 1)
  parsed <- set_account_type(parsed, 1)
  parsed <- set_account_id(parsed, num)
  as.character(parsed)
}


steam3_to_steam64 <- function(id) {
  comp <- parse_steam3(id)
  parsed <- gmp::as.bigq(0)
  num <- bitwOr(bitwShiftL(strtoi(comp$number), 1), 1)

  if (comp$instance > 4) {
    parsed <- set_account_type(parsed, 8)
  } else {
    parsed <- set_account_type(parsed, comp$type)
  }

  parsed <- set_account_universe(parsed, comp$universe)
  parsed <- set_account_instance(parsed, comp$instance)
  parsed <- set_account_id(parsed, num)
  as.character(parsed)
}


set_account_id <- function(id, value) {
  value <- strtoi(value)
  if (value < 0 || value > 0xFFFFFFFF) {
    stop("Account ID cannot be higher than 0xFFFFFFFF.")
  }

  .steamid_set(id, 0, "4294967295", value)
}


set_account_instance <- function(id, value) {
  value <- strtoi(value)
  if (value < 0 || value > 0xFFFFF) {
    stop("Account instance cannot be higher than 0xFFFFF.")
  }

  .steamid_set(id, 32, "1048575", value)
}


set_account_type <- function(id, value) {
  value <- strtoi(value)
  if (value < 0 || value > 0xF) {
    stop("Account type cannot be higher than 0xF.")
  }

  .steamid_set(id, 52, "15", value)
}


set_account_universe <- function(id, value) {
  value <- strtoi(value)
  if (value < 0 || value > 0xFF) {
    stop("Account universe cannot be higher than 0xFF")
  }

  .steamid_set(id, 56, "255", value)
}


.steamid_set <- function(x, offset, mask, value) {
  bigBits::bigOr(
    bigBits::bigAnd(x, bigBits::bigNot(shiftL(mask, offset))),
    shiftL(bigBits::bigAnd(value, mask), offset)
  )
}


get_account_id <- function(id) {
  .steamid_get(id, 0, "4294967295")
}


get_account_instance <- function(id) {
  .steamid_get(id, 32, "1048575")
}


get_account_type <- function(id) {
  .steamid_get(id, 52, "15")
}


get_account_universe <- function(id) {
  .steamid_get(id, 56, "255")
}


.steamid_get <- function(x, offset, mask) {
  as.integer(bigBits::bigAnd(shiftR(x, offset), mask))
}


shiftL <- function(x, n) {
  gmp::mul.bigz(x, gmp::as.bigq(gmp::pow.bigq(gmp::as.bigz(2), n)))
}


shiftR <- function(x, n) {
  gmp::divq.bigz(x, gmp::as.bigz(gmp::pow.bigq(gmp::as.bigz(2), n)))
}


type_chars <- c(
  "0" = "I",
  "1" = "U",
  "2" = "M",
  "3" = "G",
  "4" = "A",
  "5" = "P",
  "6" = "C",
  "7" = "g",
  "8" = "T",
  "10" = "a"
)
