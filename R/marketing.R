get_active_marketing_messages <- function(country = NULL,
                                          anonymous_user = FALSE) {
  check_string(country, null = TRUE)
  check_bool(anonymous_user)
  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "IMarketingMessagesService",
    method = "GetActiveMarketingMessages",
    version = "v1",
    params = params
  )$response
}


get_marketing_message <- function(gid,
                                  language = "english",
                                  elanguage = NULL,
                                  country_code = "US",
                                  steam_realm = 1L,
                                  include = NULL,
                                  apply_user_filters = FALSE) {
  check_string(gid)
  check_length(gid, ge = 1, le = 1)
  context <- store_browse_context(
    language = language,
    elanguage = elanguage,
    country_code = country_code,
    steam_realm = steam_realm
  )
  data_request <- store_browse_item_data_request(
    include = include,
    apply_user_filters = apply_user_filters
  )
  input_json <- .make_input_json(context = context, data_request = data_request)

  request_webapi(
    api = public_api(),
    interface = "IMarketingMessagesService",
    method = "GetDisplayMarketingMessage",
    version = "v1",
    params = params
  )$response
}
