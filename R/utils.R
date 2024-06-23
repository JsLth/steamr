"%||%" <- function(x, y) if (is.null(x)) y else x
"%empty%" <- function(x, y) if (!length(x)) y else x
"%NA%" <- function(x, y) if (is.null(x) || all(is.na(x))) y else x

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


nvapply <- function(x, fun, ..., use_names = TRUE) {
  vapply(x, fun, FUN.VALUE = numeric(1), USE.NAMES = use_names, ...)
}


recurse <- function(x, fun, ...) {
  if (is.data.frame(x) || (!is.list(x) && length(x) == 1)) {
    fun(x)
  } else {
    lapply(x, recurse, fun)
  }
}


vswitch <- function(expr, fun_value = character(1), ...) {
  vapply(expr, FUN.VALUE = fun_value, switch, ...)
}


drop_null <- function(x) {
  x[!lvapply(x, is.null)]
}


drop_na <- function(x) {
  x[!is.na(x)]
}


drop_empty <- function(x) {
  x[lengths(x) > 0]
}


drop_false <- function(x) {
  x[!lvapply(x, isFALSE)]
}


obj_name <- function(x, env = parent.frame()) {
  deparse(substitute(x, env))
}


box <- function(x) {
  if (length(x) == 1) list(x) else x
}


unbox <- function(x) {
  if (is.list(x) && length(x) == 1) {
    x <- x[[1]]
  }
  x
}


to_title <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste0(toupper(substring(s, 1, 1)), substring(s, 2), collapse = " ")
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


fields_as_data_frame <- function(x) {
  for (i in seq_along(x)) {
    if (is.data.frame(x[[i]])) {
      x[[i]] <- as_data_frame(x[[i]])
    }
  }
  x
}


bind_rows <- function(..., .id = NULL) {
  dots <- unbox(list(...))
  out <- rbind_list(dots)
  if (!is.null(.id)) {
    names <- names(dots)
    nrows <- nvapply(dots, nrow)
    ids <- rep(names, times = nrows)
    ids <- data.frame(ids)
    names(ids) <- .id
    out <- cbind(ids, out)
  }

  as_data_frame(out)
}


rbind_list <- function(args) {
  nam <- lapply(args, names)
  unam <- unique(unlist(nam))
  len <- vapply(args, length, numeric(1))
  out <- vector("list", length(len))
  for (i in seq_along(len)) {
    if (!is.data.frame(args[[i]])) {
      args[[i]] <- as.data.frame(drop_null(args[[i]]))
    }
    if (nrow(args[[i]])) {
      nam_diff <- setdiff(unam, names(args[[i]]))
      if (length(nam_diff)) {
        args[[i]][nam_diff] <- NA
      }
    } else {
      next # nocov
    }
  }
  out <- do.call(rbind, args)
  rownames(out) <- NULL
  out
}


is_number <- function(x) {
  is.numeric(x) || all(grepl("^[0-9]+$", x))
}


is_url <- function(url) {
  grepl(
    "^(https?:\\/\\/)?[[:alnum:]\\.]+(\\.[[:lower:]]+)|(:[[:digit:]])\\/?",
    url,
    perl = TRUE
  )
}


match_regex <- function(pattern, text, ...) {
  regmatches(text, regexec(pattern, text, ...))
}


trim_html_error <- function(html, desc) {
  html <- gsub("<head>(.+)</head>", "", html)
  html <- gsub("<.*?>", " ", html)
  html <- gsub("\\s+", " ", html)
  html <- gsub(desc, "", html)
  trimws(html)
}
