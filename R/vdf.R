#' Valve Data Files
#' @description
#' Parse Valve's VDF or KeyValues format. VDF files are similar to JSON
#' and are used to store hierarchical metadata on resources, scripts,
#' materials, etc. Request functions such as \code{\link{request_webapi}}
#' allow VDF as a response format. For most use cases in data analysis,
#' the VDF format should not have any advantages over JSON.
#'
#' @param x A string containing a KeyValues or VDF body.
#' @param check_types Whether to automatically detect types and try to
#' convert them to R objects. By default, the KeyValues format only knows
#' strings.
#'
#' @details
#' The parsing algorithm follows the official documentation of the Valve
#' developer community wiki. The basic structure knows three control characters:
#' \code{\{}, \code{\}}, and \code{\"}. Double quotes signify a key or a value.
#' A key can either be followed by a value or by a curly bracket indicating
#' the beginning of a new nesting level. Accordingly, a key can hold either
#' a length-1 value or a named list.
#'
#' Lines beginning with \code{#} are interpreted as
#' macros and are omitted. Comments (beginning with \code{//} or \code{/*}) end
#' a line and remove everything up to the end of the line. Expressions in
#' square brackets are omitted (based on recommendations from
#' \href{https://wiki.teamfortress.com/wiki/WebAPI/VDF}{here}).
#'
#' @export
#'
#' @references
#' \url{https://developer.valvesoftware.com/wiki/KeyValues}
#'
#' \url{https://wiki.teamfortress.com/wiki/WebAPI/VDF}
#'
#' \url{https://github.com/gorgitko/valve-keyvalues-python}
#'
#' @examples
#' # example vdf
#' vdf <- '"someresource" [$WIN]
#' {
#'   "foo" "bar" // Some comment
#'   "odd" "record" [$ODD]
#'   "someotherresource"
#'   {
#'     "baz" "tar"
#'   }
#' }'
#'
#' parse_vdf(vdf)
#'
#' \dontrun{
#' # vdf from web API
#' vdf <- request_webapi(
#'   api = public_api(),
#'   interface = "IStoreService",
#'   method = "GetTagList",
#'   version = "v1",
#'   params = list(language = "english"),
#'   format = "vdf"
#' )
#'
#' parse_vdf(vdf)
#' }
parse_vdf <- function(x, check_types = TRUE) {
  # split by line
  tok <- strsplit(x, "\n")[[1]]

  # remove comments (// or /*)
  tok <- gsub("(//|/\\*).+$", "", tok)

  # tokenize by white space
  #tok <- unlist(strsplit(tok, "[[:space:]]"))

  # remove zero-chars
  tok <- tok[nzchar(tok)]

  # remove bracketed tokens
  tok <- gsub("\\[\\$.*\\]", "", tok)

  # remove linebreaks
  tok <- gsub("[\n]+", "", tok)

  # remove excess white space
  tok <- trimws(tok)

  recurse_vdf(1, tokens = tok, check_types = check_types)
}


recurse_vdf <- function(tokens, i = 0, level = 0, check_types = TRUE) {
  out <- list()
  key <- FALSE

  while (i <= length(tokens)) {
    ctok <- tokens[i]
    ntok <- tokens[i + 1]

    valrex <- "\"?([^\"]+)\"?[[:space:]]\"?([^\"]+)\"?"
    if (startsWith(ctok, "#")) {
      # ctok is macro -> skip
      i <- i + 1
      next
    } else if (identical(ctok, "{")) {
      # ctok is next level
      if (isFALSE(key)) {
        abort("Orphaned \"{\" token found at line {.val {i + 1}}.")
      }
      if (is.na(ntok)) {
        abort("Trailing \"{\" token found at line {.val {i + 1}}.")
      }
      tmp <- recurse_vdf(
        tokens,
        i = i + 1,
        level = level + 1,
        check_types = check_types
      )
      i <- tmp$i
      out[[key]] <- tmp$map
    } else if (identical(ctok, "}")) {
      # ctok is previous level
      if (level <= 0) {
        abort("Excess \"}\" token found at line {.val {i + 1}}.")
      }
      return(list(map = out, i = i + 1))
    } else if (identical(ntok, "{") && !grepl(valrex, ctok)) {
      # ctok is root key, ntok is next level
      key <- gsub("^\"|\"$", "", ctok)

      i <- i + 1
      next
    } else if (is.na(ntok)) {
      abort("Unexpected end reached at line {.val {i + 1}}. Expected \"}\", got {.val {ctok}}.")
    } else {
      # ctok is key-value
      proto <- list(key = character(), value = character())
      kv <- utils::strcapture(valrex, ctok, proto = proto)
      pos <- ifelse(nzchar(kv$key), kv$key, length(out))

      if (check_types && is_number(kv$value)) {
        kv$value <- as.numeric(kv$value)
      }

      out[[pos]] <- kv$value
      i <- i + 1
      next
    }
  }

  out
}


test_vdf <- function() {
  paste(
    '"someresource" [$WIN]\n{\n\t"foo" "bar" // Some comment\n\t"odd"',
    '"record" [$ODD]\n\t"someotherresource"\n\t{\n\t\t"baz" "tar"\n\t}\n}'
  )
}
