.make_params <- function(..., key = TRUE, env = parent.frame()) {
  if (!...length()) {
    params <- as.list(env)
  } else {
    params <- list(...)
  }

  params <- drop_empty(drop_null(params))
  c(key = if (key) api_key(), params)
}


.make_input_json <- function(..., env = parent.frame()) {
  if (!...length()) {
    obj <- lapply(ls(envir = env), function(x) {
      x <- get(x, envir = env)
      if (is.list(x) && !is.null(names(x))) x else NULL
    })
  } else {
    obj <- list(...)
  }

  obj <- drop_empty(drop_null(obj))
  jsonlite::toJSON(obj, auto_unbox = TRUE, force = TRUE)
}


pivot_longer_list <- function(lst, names_to = "name", values_to = "value") {
  lst <- lapply(names(lst), function(k) {
    val <- lst[[k]]

    if (!is.data.frame(val)) {
      # normal list
      val <- unlist(val, use.names = FALSE)
      name <- rep(k, length(val))
      df <- data.frame(name = name, value = val)
      names(df) <- c(names_to, values_to)
    } else {
      # list of dataframes
      name <- rep(k, nrow(val))
      df <- cbind(name, val)
      names(df) <- c(names_to, names(val))
    }
    df
  })
  rbind_list(lst)
}
