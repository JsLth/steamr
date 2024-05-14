check_scalar <- function(x) {
  check <- length(x) == 1
  if (!check) {
    stop(sprintf(
      "%s must be length 1, got length %s instead",
      obj_name(x),
      length(x)
    ))
  }
}


check_integerish <- function(x, null = FALSE) {
  if (null && is.null(x)) return()
  x <- as.double(x)
  check <- identical(x, round(x))
  if (!check) {
    stop(sprintf("%s must be a whole number.", obj_name(x)))
  }
}


check_string <- function(x, null = FALSE) {
  if (null && is.null(x)) return()
  check <- is.character(x)
  if (!check) {
    stop(sprintf("%s must be a character vector.", obj_name(x)))
  }
}


check_bool <- function(x) {
  check <- isTRUE(x) || isFALSE(x)
  if (!check) {
    stop(sprintf("%s must be TRUE or FALSE.", obj_name(x)))
  }
}


check_date <- function(x, null = FALSE) {
  if (null && is.null(x)) return()
  check <- inherits(x, "POSIXt")
  if (!check) {
    stop(sprintf(
      "%s must be a date-time object, not %s",
      obj_name(x),
      class(x)
    ))
  }
}


check_specified <- function(exact = FALSE) {
  env <- parent.frame()
  spec <- !unlist(eapply(env, is.null))
  call_fun <- deparse(sys.call(1))
  check <- ifelse(exact, sum(spec) == 1, !any(spec))

  if (!check) {
    if (!exact) {
      stop(sprintf("At least one argument of %s must be specified.", call_fun))
    } else {
      stop(sprintf("Exactly one argument of %s must be specified", call_fun))
    }

  }
}
