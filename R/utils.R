"%||%" <- function(x, y) if (is.null(x)) y else x
"%empty%" <- function(x, y) if (!length(x)) y else x

nlapply <- function(x, FUN, ...) {
  res <- lapply(x, FUN, ...)
  names(res) <- names(x)
  res
}


cvapply <- function(x, fun, ..., use_names = TRUE) {
  vapply(x, fun, FUN.VALUE = character(1), USE.NAMES = use_names, ...)
}


lvapply <- function(x, fun, ..., use_names = TRUE) {
  vapply(x, fun, FUN.VALUE = logical(1), USE.NAMES = use_names, ...)
}


drop_null <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


drop_empty <- function(x) {
  x[lengths(x) > 0]
}


obj_name <- function(x) {
  deparse(substitute(x))
}


box <- function(x) {
  if (length(x) > 1) list(x) else x
}


loadable <- function(x) {
  suppressPackageStartupMessages(suppressWarnings(requireNamespace(x)))
}


as_data_frame <- function(x) {
  if (loadable("tibble")) {
    tibble::as_tibble(x)
  } else {
    as.data.frame(x)
  }
}


is_url <- function(url) {
  grepl(
    "^(https?:\\/\\/)?[[:alnum:]\\.]+(\\.[[:lower:]]+)|(:[[:digit:]])\\/?",
    url,
    perl = TRUE
  )
}
