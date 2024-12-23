#' API requests
#' @description
#' Send HTTP requests to any of the available Steam APIs. As of yet,
#' \code{steamr} supports the Web API, storefront API, SteamSpy and
#' steamcmd.net. Generic requests to any other URL can also be made.
#'
#' These functions are particularly useful to serve use cases not covered by
#' the high-level functions of \code{steamr}, e.g. API methods that require
#' a publisher key.
#'
#' @param api URL to a supported Steam API. Can be \code{public_api()}
#' (public Web API), \code{partner_api()} (private Web API),
#' \code{store_api()} (store.steampowered), \code{comm_api()} (steamcommunity),
#' or \code{valve_api()} (valvesoftware). Can also be any other host.
#' @param interface Interface containing the API method.
#' @param method Method that is to be requested.
#' @param version Version string of the API method. Usually, this is \code{v1},
#' sometimes it is \code{v2}, and in rare occasions it can go as high as
#' \code{v3}.
#' @param params A list of parameters to send with the request. Note that
#' \code{input_json} blobs need to be pre-formatted. Most (but not all)
#' methods require an API key in the \code{key} parameter. Authenticated
#' methods require an access_token in \code{access_token}.
#' @param http_method HTTP method to be used. The Steam Web API and storefront
#' API only take \code{GET} and \code{POST}.
#' @param simplify Whether to simplify the output or leave it as a nested
#' list.
#' @param paginate If specified, tries to paginate through response pages.
#' Must be a named list containing parameters for the pagination process.
#' Can contain the following keys:
#'
#' \describe{
#'  \item{method}{Can be one of \code{cursor}, \code{offset}, and \code{input_json}.
#'  \code{cursor} takes the \code{next_cursor} value of the response and
#'  re-inserts it in the \code{cursor} parameter of the next request
#'  (see \code{\link[httr2]{iterate_with_cursor}}). \code{offset} takes
#'  increments the \code{page} parameter by 1 with
#'  each request (see \code{\link[httr2]{iterate_with_offset}}).
#'  \code{input_json} re-creates the \code{input_json} blob
#'  with an incremented \code{page} parameter with each request.}
#'
#'  \item{limit}{Integer specifying the maximum number of requests or pages
#'  sent. If \code{limit = 3}, returns the first three pages.}
#'
#'  \item{cursor}{If \code{method} is cursor, specifies the name of the
#'  cursor parameter in the request. Defaults to \code{"cursor"}}.
#'
#'  \item{next_cursor}{If \code{method} is cursor, specifies the name of the
#'  response key giving the next cursor. Defaults to \code{"next_cursor"}}.
#'
#'  \item{content_field}{If \code{method} is cursor, specifies the field name
#'  that must be populated with contents in order to continue paginating.
#'  If not specified, defaults to the first field. This can be useful if
#'  the only indicator of pagination ending is if a certain field is not
#'  present in the response.}
#'
#'  \item{total_count}{If \code{method} is start, specifies the name of the
#'  response field giving the total number of results.}
#'
#'  \item{page_size}{If \code{method} is start, specifies the total number
#'  of results per page.}
#' }
#' @param format Format of the response. One of \code{json}, \code{xml}, or
#' \code{vdf}. \code{vdf} is Steam's
#' \href{https://developer.valvesoftware.com/wiki/KeyValues}{KeyValues} format,
#' which can be parsed using \code{\link{parse_vdf}}.
#' @param serror Whether to throw a formatted error when the response has the
#' \code{X-eresult} header signifying an API error.
#' @param dry If \code{TRUE} does not run the request, but returns the
#' result of a dry run to a local server. See \code{\link[httr2]{req_dry_run}}.
#'
#' @returns The formatted response of the request. If \code{format = "json"},
#' formats the response as a list or dataframe (depending on the
#' \code{simplify} argument). If \code{format = "xml"}, returns an \code{xml}
#' object of the \code{\link[xml2]{xml2}} library. If \code{format = "vdf"},
#' returns the unformatted string of a VDF document that can be parsed with
#' \code{\link{parse_vdf}}.
#'
#' @export
#'
#' @examples
#' # simple Web API request
#' request_webapi(
#'   api = public_api(),
#'   interface = "IStoreBrowseService",
#'   method = "GetStoreCategories",
#'   version = "v1"
#' )
#'
#' # paginate through query results
#' # QueryRewardItems uses cursors but other methods
#' # also use page parameters
#' request_webapi(
#'   api = public_api(),
#'   interface = "ILoyaltyRewardsService",
#'   method = "QueryRewardItems",
#'   params = list(appids = 440),
#'   paginate = "cursor"
#' )
#'
#' # send a storefront request
#' request_storefront(
#'   api = store_api(),
#'   interface = "search",
#'   method = "suggest",
#'   params = list(term = "team fortress")
#' )
#'
#' # request_steamspy is a low-level alternative to the
#' # steamspy function
#' request_steamspy(list(request = "tag", tag = "Early Access"))
#'
#' \dontrun{
#' # request_generic exists to enable non-standard API queries
#' # the following example is used internally in auth_* functions
#' request_generic(
#'   "https://login.steampowered.com/jwt/finalizelogin",
#'   method = "POST",
#'   params = list(
#'     nonce = "<refresh token>",
#'     sessionid = "<sessionid>",
#'     redir = "https://steamcommunity.com/login/home/?goto="
#'   )
#' )
#' }
request_webapi <- function(api,
                           interface,
                           method,
                           version = "v1",
                           params = list(),
                           http_method = "GET",
                           simplify = TRUE,
                           paginate = NULL,
                           format = c("json", "xml", "vdf"),
                           cache = FALSE,
                           serror = TRUE,
                           dry = FALSE) {
  format <- match.arg(format)
  params <- prepare_params(params)
  params$format <- format

  req <- httr2::request(api)
  template <- sprintf("%s /{interface}/{method}/{version}", http_method)
  req <- httr2::req_template(req, template)

  if (identical(http_method, "GET")) {
    req <- do.call(
      httr2::req_url_query,
      c(list(req), params)
    )
  } else {
    req <- do.call(httr2::req_body_form, c(list(req), params))
  }

  req <- use_session(req)

  verbose <- getOption("steamr_debug", FALSE)

  if (cache) {
    req <- httr2::req_cache(req, path = tempdir(), debug = verbose)
  }

  req <- httr2::req_retry(req, max_tries = getOption("steamr_max_tries", 3))

  req <- httr2::req_error(req, is_error = function(resp) {
    content_type <- resp$headers$`Content-Type`
    if (startsWith(content_type, "text/html")) {
      code <- resp$status_code
      desc <- httr2::resp_status_desc(resp)
      msg <- trim_html_error(httr2::resp_body_html(resp), desc)
      abort(c("HTTP error {code} {desc}", "x" = msg), call = NULL)
    } else if (startsWith(content_type, "application/json")) {
      resp <- httr2::resp_body_json(resp)$response
      code <- resp$result
      msg <- resp$error
      if (!is.null(resp$error)) {
        abort("Error code {code}", "x" = msg, call = NULL)
      }
    }
    FALSE
  })

  if (verbose) {
    cat(http_method, utils::URLdecode(req$url), "\n")
  }

  if (dry) {
    return(httr2::req_dry_run(req))
  }

  if (!is.null(paginate)) {
    paginate_steam(req, paginate, simplify = simplify)
  } else {
    res <- httr2::req_perform(req)
    ecode <- res$headers[["X-eresult"]]
    if (serror) {

      if (!is.null(ecode) && !identical(ecode, "1")) {
        msg <- eresult[eresult$code %in% ecode, ]$msg
        abort(
          "Steam Web API returned error code {ecode}: {msg}",
          call = NULL
        )
      }
    }

    switch(
      format,
      json = httr2::resp_body_json(res, simplifyVector = simplify, flatten = TRUE),
      xml = httr2::resp_body_xml(res),
      vdf = httr2::resp_body_string(res, encoding = "UTF-8")
    )
  }
}


#' @rdname request_webapi
#' @export
#' @param params_as_query In most storefront endpoints, parameters are
#' passed as a URL query (i.e. \code{domain.org/path?name=param}). However, some endpoints expect them to be passed
#' as a URL path (i.e. \code{domain.org/path/param1/param2}). If \code{FALSE}, constructs parameters as a URL path,
#' otherwise as a query.
request_storefront <- function(api,
                               interface,
                               method,
                               params = list(),
                               params_as_query = TRUE,
                               http_method = "GET",
                               simplify = TRUE,
                               paginate = NULL,
                               rate = NULL,
                               cache = FALSE,
                               dry = FALSE) {
  is_store_api <- identical(api, store_api())
  params <- prepare_params(params, api = "storefront")

  req <- httr2::request(api)
  template <- sprintf("%s /{interface}/{method}", http_method)
  req <- httr2::req_template(req, template)

  if (is_store_api) {
    rate <- 300 / 300
  }

  if (!is.null(rate) && getOption("steamr_throttle", TRUE)) {
    req <- httr2::req_throttle(req, rate = rate)
  }

  req <- use_session(req)

  verbose <- getOption("steamr_debug", FALSE)

  if (cache) {
    req <- httr2::req_cache(req, path = tempdir(), debug = verbose)
  }

  req <- httr2::req_retry(req, max_tries = getOption("steamr_max_tries", 3))

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

  if (verbose) {
    cat(http_method, utils::URLdecode(req$url), "\n")
  }

  if (dry) {
    return(httr2::req_dry_run(req))
  }

  if (!is.null(paginate)) {
    paginate_steam(req, paginate, simplify = simplify)
  } else {
    res <- httr2::req_perform(req)

    res <- httr2::resp_body_json(res, simplifyVector = TRUE, flatten = TRUE)

    if (!length(res)) {
      abort(
        "Steam storefront API returned an empty response",
        "i" = "Check your input!",
        call = NULL
      )
    }

    code <- res$success %||% res$eresult %||% res$status %||% 0L
    if (!identical(code, 1L) && isFALSE(code)) {
      msg <- res$err_msg %||% res$msg %||% "Something has gone wrong."

      abort(
        "Steam storefront API returned error code {code}",
        "x" = msg,
        call = NULL
      )
    }

    if (is.data.frame(res)) {
      res <- fix_steam_bool(res)
    }

    res
  }
}


#' @rdname request_webapi
#' @export
request_steamspy <- function(params) {
  req <- httr2::request("https://steamspy.com/api.php")
  req <- do.call(httr2::req_url_query, c(list(req), params))

  if (getOption("steamr_debug", FALSE)) {
    cat("GET", utils::URLdecode(req$url), "\n")
  }

  if (identical(params$request, "all")) {
    is_http_code <- function(resp, code = 500) resp$status_code %in% code
    req <- httr2::req_error(
      req,
      is_error = function(resp) !is_http_code(resp, c(200, 500))
    )
    req <- httr2::req_throttle(req, rate = 1 / 60)
    res <- httr2::req_perform_iterative(
      req,
      next_req = httr2::iterate_with_offset(
        param_name = "page",
        start = 0,
        offset = 1,
        resp_complete = is_http_code
      ),
      max_reqs = getOption("steamr_max_reqs", Inf)
    )
    res <- lapply(res, function(x) {
      res <- httr2::resp_body_json(x, simplifyVector = TRUE, flatten = TRUE)
      res <- try(rbind_list(res))
      if (inherits(res, "try-error")) browser()
      if (!is_http_code(x, 500)) res

    })
    res <- res[lvapply(res, is.null, use_names = FALSE)]
    rbind_list(res)
  } else {
    req <- httr2::req_throttle(req, rate = 1 / 1)
    res <- httr2::req_perform(req)
    httr2::resp_body_json(res, simplifyVector = TRUE, flatten = TRUE)
  }
}


#' @rdname request_webapi
#' @export
#' @param url URL to send a request to.
#' @param ... Further arguments passed to the parsing function defined
#' by \code{format}, i.e. \code{\link[httr2]{resp_body_json}},
#' \code{\link[httr2]{resp_body_xml}} or \code{\link[httr2]{resp_body_string}}.
#' @param headers Key-value pairs of additional headers to append to the
#' request.
request_generic <- function(url,
                            params = NULL,
                            http_method = "GET",
                            format = "json",
                            ...,
                            headers = NULL,
                            options = NULL,
                            dry = FALSE) {
  req <- httr2::request(url)

  if (!is.null(params)) {
    req <- switch(
      http_method,
      GET = do.call(httr2::req_url_query, c(list(req), params)),
      POST = do.call(httr2::req_body_form, c(list(req), params))
    )
  }

  req <- use_session(req)
  req <- httr2::req_method(req, http_method)

  if (!is.null(headers)) {
    req <- do.call(httr2::req_headers, c(list(req), headers))
  }

  if (!is.null(options)) {
    req <- do.call(httr2::req_options, c(list(req), options))
  }

  if (getOption("steamr_debug", FALSE)) {
    cli::cli_inform(paste(http_method, utils::URLdecode(req$url)))
  }

  if (dry) {
    return(httr2::req_dry_run(req))
  } else {
    res <- httr2::req_perform(req)
  }

  fun <- get(paste0("resp_body_", format), envir = asNamespace("httr2"))
  do.call(fun, c(list(res), ...))
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


prepare_params <- function(params, api = "webapi") {
  do_auth <- getOption("steamr_do_auth", TRUE)
  if (!inherits(params, "steam_params") && do_auth) {
    params <- local(do.call(.make_params, params))
  }

  if (!do_auth) {
    params$key <- NULL
    params$access_token <- NULL
  }

  params <- params[lengths(params) > 0]
  pnames <- names(params)
  switch(
    api,
    webapi = webapi_params(params),
    storefront = storefront_params(params)
  )
}


webapi_params <- function(params) {
  for (k in names(params)) {
    x <- params[[k]]
    if (is.logical(x)) {
      params[[k]] <- as.numeric(x)
    } else if (is.numeric(x)) {
      params[[k]] <- format(x, scientific = FALSE, trim = TRUE)
    }

    if (length(x) > 1 && !is.null(names(x))) {
      params[[k]] <- jsonlite::toJSON(x, auto_unbox = TRUE, force = TRUE)
    } else if (is.list(x) || length(x) > 1) {
      idx <- match(k, names(params))
      for (i in seq_along(x)) {
        names(x)[i] <- sprintf("%s[%s]", k, i - 1)
      }
      params <- append(params, x, after = idx)
      params[[k]] <- NULL
    }
  }

  params
}


storefront_params <- function(params) {
  for (k in names(params)) {
    x <- params[[k]]
    if (is.logical(x)) {
      params[[k]] <- as.numeric(x)
    } else if (length(x) > 1) {
      params[[k]] <- paste(x, collapse = ",")
    }
  }

  params
}



paginate_steam <- function(req, paginate, simplify) {
  limit <- getOption("steamr_max_reqs", paginate$limit %||% Inf)
  paginator <- switch(
    paginate$method,
    cursor = httr2::iterate_with_cursor(
      param_name = paginate$cursor %||% "cursor",
      resp_param_value = function(resp) {
        ncursor <- paginate$next_cursor %||% "next_cursor"
        cfield <- paginate$content_field %||% 1L
        content <- unbox(httr2::resp_body_json(resp))
        cursor <- httr2::url_parse(resp$url)$query$cursor

        if (identical(content$count, 0L)) {
          return(NULL)
        }

        if (identical(content$cursor, cursor)) {
          return(NULL)
        }

        if (is.null(content[[cfield]])) {
          return(NULL)
        }

        content[[ncursor]]
      }
    ),
    start = httr2::iterate_with_offset(
      param_name = paginate$param %||% "start",
      start = 0,
      offset = 100,
      resp_pages = function(resp) {
        if (identical(resp$status_code, 200L)) {
          body <- httr2::resp_body_json(resp)
          total <- paginate$total_count %||% "total_count"
          size <- paginate$page_size %||% "pagesize"
          n <- ceiling(body[[total]] / body[[size]])
          if (n > 0) n
        }
      },
      resp_complete = function(resp) {
        unboxer <- paginate$unboxer
        body <- httr2::resp_body_json(resp)
        length(body[[unboxer]]) == 0
      }
    ),
    input_json = iterate_with_input_json(req)
  )
  resps <- httr2::req_perform_iterative(
    req,
    next_req = paginator,
    max_reqs = limit
  )

  resps <- lapply(
    httr2::resps_successes(resps),
    function(resp, ...) {
      resp <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)
      check <- switch(
        paginate$method,
        start = !length(resp$results) == 0,
        !identical(resp$response$count, 0L)
      )
      if (check) resp
    }
  )

  resps[!lvapply(resps, is.null)]
}


iterate_with_input_json <- function(req) {
  input_json <- httr2::url_parse(req$url)$query$input_json
  resp_complete <- function(resp) {
    content <- httr2::resp_body_json(resp)$response
    !content$metadata$count > 0
  }

  i <- 0
  function(resp, req) {
    if (!isTRUE(resp_complete(resp))) {
      i <<- i + 1
      httr2::req_url_query(req, input_json = sprintf(input_json, i))
    }
  }
}


get_hostname <- function(url) {
  httr2::url_parse(url)$hostname
}


#' @rdname request_webapi
#' @export
public_api <- function() "https://api.steampowered.com"
#' @rdname request_webapi
#' @export
partner_api <- function() "https://partner.steam-api.com"
#' @rdname request_webapi
#' @export
store_api <- function() "https://store.steampowered.com"
#' @rdname request_webapi
#' @export
comm_api <- function() "https://steamcommunity.com"
#' @rdname request_webapi
#' @export
valve_api <- function() "https://valvesoftware.com"
