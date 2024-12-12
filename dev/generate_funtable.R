steamr <- asNamespace("steamr")

code <- eapply(steamr, function(x) {
  if (is.function(x)) {
    deparse(body(x))
  }
})
code <- code[!vapply(code, FUN.VALUE = logical(1), is.null)]

needs_auth <- grepl("check_authenticated", code)
needs_key <- grepl("check_steam_key", code)
api <- vapply(code, FUN.VALUE = character(1), function(x) {
  if (any(grepl("request_storefront", x))) {
    "Storefront"
  } else if (any(grepl("request_webapi", x))) {
    "Web API"
  } else {
    NA_character_
  }
})
fun <- names(code)

code_files <- dir("R", full.names = TRUE)
code <- lapply(code_files, readLines)
topic <- lapply(fun, function(x) {
  if (startsWith(x, "%")) x <- dQuote(x, q = FALSE)
  x <- stringr::str_escape(x)
  cf <- lapply(code, paste, collapse = "\n")
  defines_fun <- grepl(paste0("(^|\n)", x, " <- function"), cf)
  file <- code_files[defines_fun]
  title <- substr(file, 3, nchar(file) - 2)
  title <- gsub("_", " ", title)
  if (length(title) > 1) browser()
  title <- stringr::str_to_title(title)
})

df <- tibble::tibble(
  Topic = unlist(topic),
  Function = paste0("`", fun, "`"),
  API = api,
  `Needs key` = ifelse(needs_key, ":heavy_check_mark:", ":x:"),
  `Needs auth` = ifelse(needs_auth, ":heavy_check_mark:", ":x:")
)
df <- na.omit(df)

saveRDS(df, "dev/funtable.rds")
