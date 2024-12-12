#' Support FAQ
#' @description
#' Search support FAQ articles.
#'
#' @inheritParams common
#' @param search_text Search text to match FAQ articles by.
#'
#' @export
#'
#' @examples
#' # get help articles on portal 2
#' search_support("portal 2")
#'
#' # get help articles on purchases
#' search_support("purchase")
#'
#' # get localized articles in french
#' search_support("purchase", elanguage = 2)
search_support <- function(search_text,
                           elanguage = NULL,
                           paginate = TRUE,
                           max_pages = Inf) {
  check_string(search_text)
  check_integerish(elanguage, null = TRUE)

  params <- .make_params(
    search_text = search_text,
    elanguages = as.list(elanguage),
    count = 100,
    cursor = "*"
  )

  res <- request_webapi(
    api = public_api(),
    interface = "IClanFAQSService",
    method = "SearchFAQs",
    version = "v1",
    params = params,
    paginate = if (paginate) list(method = "cursor", content_field = "faqs")
  )

  if (paginate) {
    res <- lapply(res, function(x) x$response$faqs)
    res <- rbind_list(res)
  } else {
    res <- res$response$faqs
  }

  as_data_frame(res)
}
