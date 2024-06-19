parse_vdf <- function(x) {
  tok <- strsplit(x, "[[:space:]]")[[1]]
  tok <- tok[nzchar(tok)]
  tok <- gsub("\"", "", tok)
  recurse_vdf(1, tokens = tok, max = length(tok))
}


recurse_vdf <- function(i, tokens, level = 0, key = TRUE, max = Inf) {
  ctok <- tokens[i]
  ntok <- tokens[i + 1]
browser()

  if (identical(ctok, "{")) {
    recurse_vdf(i + 1, tokens, level = level, key = TRUE)
  } else if (identical(ctok, "}")) {
    recurse_vdf(i + 1, tokens, level = level - 1, key = FALSE)
  } else if (identical(ntok, "{")) {
    recurse_vdf(i + 1, tokens, level = level + 1, key = FALSE)
  } else if (isTRUE(key)) {
    list(recurse_vdf(i + 1, tokens, level = level, key = FALSE))
  } else {
    ctok
  }
}
