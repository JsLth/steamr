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


pivot_longer_list <- function(lst) {
  lst <- lapply(names(lst), function(k) {
    val <- unlist(lst[[k]])
    name <- rep(k, length(val))
    list(name = name, value = val)
  })
  do.call(rbind.data.frame, lst)
}
