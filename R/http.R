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
                           http_method = "GET",
                           simplify = TRUE,
                           serror = TRUE,
                           access_token = NULL,
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

  req <- httr2::request(url)

  req <- httr2::req_method(req, http_method)
  if (identical(http_method, "GET")) {
    req <- do.call(
      httr2::req_url_query,
      c(list(req), params)
    )
  } else {
    req <- do.call(httr2::req_body_form, c(list(req), params))
  }

  if (is.character(access_token)) {
    req <- httr2::req_url_query(req, access_token = access_token)
  }

  req <- use_session(req)
  req <- use_auth(req, api)

  req <- httr2::req_error(req, is_error = function(resp) {
    if (startsWith(resp$headers$`Content-Type`, "text/html")) {
      code <- resp$status_code
      desc <- httr2::resp_status_desc(resp)
      msg <- trim_html_error(httr2::resp_body_html(resp), desc)
      stop(sprintf("HTTP error %s %s: %s", code, desc, msg), call. = FALSE)
    } else {
      resp <- httr2::resp_body_json(resp)$response
      code <- resp$result
      msg <- resp$error
      if (!is.null(resp$error)) {
        stop(sprintf("Error code %s: %s", code, msg), call. = FALSE)
      }
    }
    FALSE
  })

  if (getOption("steamr_echo", FALSE)) {
    cat("Querying:\n", utils::URLdecode(req$url), "\n")
  }

  if (dry) {
    return(httr2::req_dry_run(req))
  } else {
    res <- httr2::req_perform(req)

    ecode <- res$headers[["X-eresult"]]
    if (serror) {

      if (!is.null(ecode) && !identical(ecode, "1")) {
        msg <- eresult[eresult$code %in% ecode, ]$msg
        stop(
          sprintf("Steam Web API returned error code %s: %s", ecode, msg),
          call. = FALSE
        )
      }
    }

    httr2::resp_body_json(res, simplifyVector = simplify, flatten = TRUE)
  }
}


request_internal <- function(api,
                             interface,
                             method,
                             params = list(),
                             params_as_query = TRUE,
                             simplify = TRUE,
                             rate = NULL,
                             dry = FALSE) {
  is_store_api <- identical(api, store_api())
  url <- paste(api, interface, method, sep = "/")

  for (k in names(params)) {
    x <- params[[k]]
    if (is.logical(x)) {
      params[[k]] <- as.numeric(x)
    } else if (is.list(x)) {
      paste(x, collapse = ",")
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

  req <- use_session(req)
  req <- use_auth(req, api)

  if (params_as_query) {
    req <- do.call(httr2::req_url_query, c(list(req), params))
  } else {
    req <- do.call(httr2::req_url_path_append, c(list(req), params))
  }

  req$url <- utils::URLencode(req$url)

  req <- httr2::req_error(
    req,
    is_error = function(resp) {
      headers <- resp$headers[["Content-Type"]]
      !grepl("application/json", headers)
    }
  )

  if (getOption("steamr_echo", FALSE)) {
    cat("Querying:\n", utils::URLdecode(req$url), "\n")
  }

  if (dry) {
    return(httr2::req_dry_run(req))
  } else {
    res <- httr2::req_perform(req)
  }

  res <- httr2::resp_body_json(res, simplifyVector = TRUE, flatten = TRUE)

  if (!length(res)) {
    stop("Steam internal API returned an empty response. Check your input!")
  }

  code <- res$success %||% res$eresult %||% 1L
  if (!identical(code, 1L)) {
    msg <- res$err_msg %||% res$msg %||% "Something has gone wrong."

    stop(
      sprintf("Steam internal API returned error code %s:\n%s", code, msg),
      call. = FALSE
    )
  }

  if (is.data.frame(res)) {
    res <- fix_steam_bool(res)
  }

  res
}


request_generic <- function(url,
                            params = NULL,
                            method = "GET",
                            format = "json",
                            format_args = NULL,
                            dry = FALSE) {
  req <- httr2::request(url)

  if (!is.null(params)) {
    req <- switch(
      method,
      GET = do.call(httr2::req_url_query, c(list(req), params)),
      POST = do.call(httr2::req_body_form, c(list(req), params))
    )
  }

  req <- use_session(req)
  req <- use_auth(req, url)
  req <- httr2::req_method(req, method)

  if (dry) {
    return(httr2::req_dry_run(req))
  } else {
    res <- httr2::req_perform(req)
  }

  fun <- get(paste0("resp_body_", format), envir = asNamespace("httr2"))
  do.call(fun, c(list(res), format_args))
}


use_auth <- function(req, api = NULL) {
  api_host <- get_hostname(api)
  def_hosts <- list(
    store = get_hostname(store_api()),
    community = get_hostname(comm_api())
  )

  if (is.null(api)) {
    api <- if (identical(api_host, def_hosts$store)) {
      "store"
    } else if (identical(api_host, def_hosts$community)) {
      "community"
    }
  }

  auth <- get0("auth", envir = globst)
  if (!is.null(auth) && !is.null(api)) {
    cookies <- auth$cookies[[api]]
    cookies <- paste(paste0(names(cookies), "=", cookies), collapse = "; ")
    req <- httr2::req_headers(req, Cookie = cookies)
  }

  req
}


use_session <- function(req) {
  if (!exists("session", envir = globst)) {
    tempf <- tempfile(pattern = "steam_session", fileext = "")
    tempf <- normalizePath(tempf, winslash = "/", mustWork = FALSE)
    assign("session", tempf, envir = globst)
  }

  httr2::req_cookie_preserve(req, globst$session)
}


# adapted from cookiemonster:::read_cookiefile
read_cookies <- function(file) {
  text <- readLines(file)
  df <- utils::read.delim(text = text[grep("\t", text)], header = FALSE)
  colnames(df) <- c("domain", "flag", "path", "secure", "expiration", "name", "value")
  df$domain <- sub("^#HttpOnly_|^\\.", "", df$domain)
  df$expiration <- as.POSIXct(df$expiration, origin = "1970-01-01")
  as_data_frame(df)
}


fix_steam_bool <- function(res) {
  for (col in names(res)) {
    x <- res[[col]]
    if (is.logical(x)) res[[col]] <- replace(x, which(is.na(x)), FALSE)
  }
  res
}


get_hostname <- function(url) {
  httr2::url_parse(url)$hostname
}
