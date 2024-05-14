public_api <- function() "https://api.steampowered.com"
partner_api <- function() "https://partner.steam-api.com"
store_api <- function() "https://store.steampowered.com"
comm_api <- function() "https://steamcommunity.com"
valve_api <- function() "https://valvesoftware.com"

request_webapi <- function(api,
                           interface,
                           method,
                           version,
                           params = list(),
                           simplify = TRUE,
                           format = "json",
                           dry = FALSE) {
  url <- paste(api, interface, method, version, sep = "/")

  params <- params[lengths(params) > 0]
  pnames <- names(params)

  for (k in names(params)) {
    x <- params[[k]]
    if (is.logical(x)) {
      params[[k]] <- as.numeric(x)
    } else if (is.numeric(x)) {
      params[[k]] <- format(x, scientific = FALSE)
    } else if (is.list(x) && !is.null(names(x))) {
      params[[k]] <- jsonlite::toJSON(x, auto_unbox = TRUE, force = TRUE)
    } else if (is.list(x)) {
      idx <- match(k, names(params))
      for (i in seq_along(x)) {
        names(x)[i] <- sprintf("%s[%s]", k, i - 1)
      }
      params <- append(params, x, after = idx)
      params[[k]] <- NULL
    }
  }
  #params$format <- format

  req <- httr2::request(url)
  req <- httr2::req_method(req, "GET")
  req <- do.call(
    httr2::req_url_query,
    c(list(req), params)
  )
  req <- httr2::req_error(req, is_error = function(resp) {
    if (startsWith(resp$headers$`Content-Type`, "text/html")) {
      code <- resp$status_code
      desc <- httr2::resp_status_desc(resp)
      msg <- trim_html_error(httr2::resp_body_html(resp), desc)
      stop(sprintf("HTTP error %s %s: %s", code, desc, msg))
    } else {
      resp <- httr2::resp_body_json(resp)$response
      code <- resp$result
      msg <- resp$error
      stop(sprintf("Error code %s: %s", code, msg))
    }
  })

  if (getOption("steamr_echo", FALSE)) {
    cat("Querying:\n", utils::URLdecode(req$url), "\n")
  }

  if (dry) {
    httr2::req_dry_run(req)
  } else {
    res <- httr2::req_perform(req)

    ecode <- res$headers[["X-eresult"]]
    if (!is.null(ecode) && !identical(ecode, "1")) {
      msg <- eresult[eresult$code %in% ecode, ]$msg
      stop(sprintf("Steam API returned error code %s: %s", ecode, msg), call. = FALSE)
    }

    httr2::resp_body_json(res, simplifyVector = simplify, flatten = TRUE)
  }
}


request_internal <- function(api,
                          interface,
                          method,
                          params = list(),
                          simplify = TRUE,
                          rate = NULL) {
  is_store_api <- identical(api, store_api())
  url <- paste(api, interface, method, sep = "/")

  for (k in names(params)) {
    x <- params[[k]]
    if (is.logical(x)) {
      params[[k]] <- as.numeric(x)
    } else if (is.list(x)) {
      idx <- match(k, names(params))
      for (i in seq_along(x)) {
        names(x)[i] <- sprintf("%s[%s]", k, i - 1)
      }
      params <- append(params, x, after = idx)
      params[[k]] <- NULL
    }
  }

  req <- httr2::request(url)
  req <- httr2::req_method(req, "GET")

  if (is_store_api) {
    rate <- 300 / 300
  }

  if (!is.null(rate) && getOption("steamr_throttle", TRUE)) {
    req <- httr2::req_throttle(req, rate = rate)
  }

  if (is_store_api) {
    req <- do.call(httr2::req_url_query, c(list(req), params))
  } else {
    req <- httr2::req_url_path_append(req, params)
    req$url <- utils::URLencode(req$url)
  }

  if (getOption("steamr_echo", FALSE)) {
    cat("Querying:\n", utils::URLdecode(req$url), "\n")
  }

  res <- httr2::req_perform(req)
  res <- httr2::resp_body_json(res, simplifyVector = TRUE, flatten = TRUE)

  if (is.data.frame(res)) {
    res <- fix_steam_bool(res)
  }

  res
}


fix_steam_bool <- function(res) {
  for (col in names(res)) {
    x <- res[[col]]
    if (is.logical(x)) res[[col]] <- replace(x, which(is.na(x)), FALSE)
  }
  res
}
