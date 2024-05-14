query <- function(query,
                  start = NULL,
                  count = NULL,
                  sort = NULL,
                  released_only = FALSE,
                  coming_soon_only = FALSE,
                  only_free_items = FALSE,
                  exclude_free_items = FALSE,
                  include = NULL,
                  tags = NULL,
                  regional_top_n = NULL,
                  global_top_n = NULL,
                  regional_long_term_top_n = NULL,
                  global_long_term_top_n = NULL,
                  sale_filter = NULL,
                  language = "english",
                  elanguage = NULL,
                  country_code = "US",
                  steam_realm = 1) {
  context <- store_browse_context(
    language = language,
    elanguage = elanguage,
    country_code = country_code,
    steam_realm = steam_realm
  )
  input_json <- .make_input_json(
    context = context
  )

  params <- .make_params(query_name = query, input_json = input_json)
  res <- request_webapi(
    api = public_api(),
    interface = "IStoreQueryService",
    method = "Query",
    version = "v1",
    params = params
  )
  res
}


search_suggestions <- function(query,
                               query_name = NULL,
                               language = "english",
                               elanguage = NULL,
                               country_code = "US",
                               steam_realm = 1,
                               max_results = 10,
                               released_only = FALSE,
                               coming_soon_only = FALSE,
                               only_free_items = FALSE,
                               exclude_free_items = FALSE,
                               types = NULL,
                               tags = NULL,
                               regional_top_n = NULL,
                               global_top_n = NULL,
                               regional_long_term_top_n = NULL,
                               global_long_term_top_n = NULL,
                               sale_filter = NULL,
                               include = NULL,
                               apply_user_filters = FALSE) {
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
  input_json <- .make_input_json()

  params <- .make_params(
    search_term = query,
    query_name = query_name,
    max_results = max_results,
    input_json = input_json
  )
  res <- request_webapi(
    api = public_api(),
    interface = "IStoreQueryService",
    method = "SearchSuggestions",
    version = "v1",
    params = params
  )
  res
}
