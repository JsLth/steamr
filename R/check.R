check_interactive <- function() {
  if (!interactive()) {
    abort("Session authentication is only possible in interactive mode.")
  }
}


check_authenticated <- function() {
  if (!is_authenticated()) {
    abort(c(
      "Session is not authenticated.",
      "i" = "See {.code ?auth} to learn more about Steam authentication."
    ))
  }
}
