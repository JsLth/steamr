#' HTML to BBCode
#' @description
#' Converts an HTML string to BBCode. BBCode is commonly used in the Steam
#' forum and other bulletin boards.
#'
#' @param x A string containing HTML formatting.
#' @param preserve_newlines If \code{TRUE}, preserves linebreaks. If
#' \code{FALSE}, also preserves linebreaks -- or so it seems.
#'
#' @returns A character string containing BBCode formatted text.
#'
#' @evalRd auth_table(list("wba_html_to_bbcode", key = FALSE, login = FALSE))
#'
#' @export
#'
#' @examples
#' \donttest{string1 <- "<b>test</b>"
#' string2 <- "<br>\n"
#' string3 <- "<a href='google.com'>link</a>"
#'
#' html_to_bbcode(string1)
#' html_to_bbcode(string2)
#' html_to_bbcode(string3)}
wba_html_to_bbcode <- function(x, preserve_newlines = FALSE) {
  params <- .make_params(content = x, preserve_newlines = preserve_newlines)
  res <- request_webapi(
    api = public_api(),
    interface = "INewsService",
    method = "ConvertHTMLToBBCode",
    version = "v1",
    params = params
  )$response

  if (isFALSE(res$found_html)) {
    cli::cli_warn("No HTML found in input.")
  }

  res$converted_content
}
