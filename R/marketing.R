#' Marketing
#' @description
#' Marketing messages are sometimes shown to the user as a popup window. This
#' function can retrieve details about the marketing messages currently active
#' in a given country.
#'
#' @inheritParams wba_charts
#' @param anonymous_user If \code{TRUE}, will not return any marketing messages
#' that require ownership, playtime or wishlisting.
#'
#' @returns A dataframe containing the groupID
#'
wba_active_marketing_messages <- function(country = NULL,
                                          anonymous_user = FALSE) {
  assert_string(country, null = TRUE)
  assert_flag(anonymous_user)
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IMarketingMessagesService",
    method = "GetActiveMarketingMessages",
    version = "v1",
    params = params
  )$response$message
  as_data_frame(res)
}
