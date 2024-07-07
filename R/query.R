#' Query apps (Web API)
#' @description
#' Query apps using search terms and filters.
#'
#' \code{query} returns a dataframe of \code{count} applications that match
#' the specified filters.
#'
#' \code{search_suggestions} returns a dataframe that match the specified
#' search term and filters.
#'
#' \code{query_by_recommended_tags} is an authenticated function that returns
#' a query based on a user's recommended tags.
#'
#' Both \code{search_suggestions} and \code{query_by_recommended_tags} do not
#' currently seem to work. Any help is welcome. For the storefront equivalents
#' of search suggestions, see \code{\link{search_apps}}. These do actually work
#' but do not allow for much freedom in filtering.
#'
#' @param start Result page at which to start. The page count starts at 0.
#' If the page count exceeds the maximum pages, the output returns a
#' \code{count} attribute of 0.
#' @param count Number of results per page up to a maximum of 1,000. Defaults
#' to 10.
#' @param sort Column of the output by which to sort.
#' @param released_only Whether to return only released applications.
#' @param coming_soon_only Whether to return only applications that are
#' being released in the future.
#' @param only_free_items Whether to return only applications without a
#' price tag.
#' @param exclude_free_items Whether to return only applications with a
#' price tag.
#' @param type_filters Specifies the types of applications to include in the
#' output. Can be one or several of \code{apps}, \code{packages}, \code{bundles},
#' \code{games}, \code{demos}, \code{mods}, \code{dlc}, \code{software},
#' \code{video}, \code{hardware}, \code{series}, and \code{music}.
#' @param tags A vector of tagIDs. Only games matching these tags will be
#' returned. Tags are categories or genres given to a game by the user
#' community. For an overview of existing tagIDs, see
#' \code{\link{get_most_popular_tags}}.
#' @param tags_exclude A vector of tagIDs. Only games \emph{not} matching
#' these tags will be returned. Tags are categories or genres given to a game
#' by the user community. For an overview of existing tagIDs, see
#' \code{\link{get_most_popular_tags}}.
#' @param content_descriptors A vector of content descriptor IDs. Only games
#' matching these tags will be returned. Content descriptors are descriptions
#' of the publisher on the explicitness of a game, e.g. strong language or gore.
#' For an overview of existing content descriptors, see
#' \code{\link{content_descriptors}}.
#' @param content_descriptors_exclude A vector of content descriptor IDs. Only
#' games \emph{not} matching these tags will be returned. Content descriptors
#' are descriptions of the publisher on the explicitness of a game, e.g. strong
#' language or gore.
#' For an overview of existing content descriptors, see
#' \code{\link{content_descriptors}}.
#' @param regional_top_n,regional_longterm_top_n Specifies the long-term or
#' short-term number of top games within the country specified by
#' \code{country_code}.
#' @param global_top_n,global_longterm_top_n Specifies the long-term or
#' short-term number of global top games.
#' @param sale_tagid Unknown. Probably the ID of a sale to query.
#' @param hub_type,hub_category,hub_tagid,discount_filter Unknown. Possibly
#' used to filter by community hub.
#' @param optin_name,optin_tagid,prune_tagid,optin_only Unknown. Possibly
#' related to Beta opt-in.
#' @param iterate Whether to automatically iterate through pages. Depending
#' on the query, this can take a long time. The maximum number of requests
#' can be controlled using \code{options(steamr_max_reqs = ...)}.
#' @inheritParams get_items
#'
#' @returns A dataframe containing information about the queried games.
#' The output contains three attributes \code{matches}, \code{start}, and
#' \code{count} reporting on the total number of matches, current page,
#' and number of matches returned. If \code{paginate = TRUE},
#' \code{start} is a vector of all paginated pages and \code{count} is the
#' sum of all paginated counts.
#'
#' The number of rows is generally controlled using the \code{count} and
#' \code{paginate} arguments. However, the \code{*_top_n} arguments overwrite
#' the output count by their specified value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # return the first 500 matches
#' query(count = 500)
#'
#' # return the first 2500 matches
#' options(steamr_max_reqs = 5)
#' query(count = 500, paginate = TRUE)
#'
#' # return page 5 of the query
#' query(start = 6)
#'
#' # sort by item type
#' query(sort = 1)
#'
#' # filter out mature content
#' cd <- content_descriptors()
#' cd <- cd[grepl("Mature Content", cd$description), ]$code
#' query(content_descriptors_exclude = 5)
#'
#' # filter by strategy games
#' tags <- get_most_popular_tags()
#' tagids <- tags[grepl("Strategy", tags$name), ]$tagid
#' query(tags = tagids)
#'
#' # out of all matches, return the 5 regional best-sellers
#' query(regional_top_n = 5)
#' }
query <- function(start = NULL,
                  count = NULL,
                  sort = NULL,
                  released_only = FALSE,
                  coming_soon_only = FALSE,
                  only_free_items = FALSE,
                  exclude_free_items = FALSE,
                  type_filters = NULL,
                  tags = NULL,
                  tags_exclude = NULL,
                  content_descriptors = NULL,
                  content_descriptors_exclude = NULL,
                  regional_top_n = NULL,
                  global_top_n = NULL,
                  regional_longterm_top_n = NULL,
                  global_longterm_top_n = NULL,
                  sale_tagid = NULL,
                  hub_type = NULL,
                  hub_category = NULL,
                  hub_tagid = NULL,
                  discount_filter = NULL,
                  optin_name = NULL,
                  optin_tagid = NULL,
                  prune_tagid = NULL,
                  optin_only = FALSE,
                  language = "english",
                  elanguage = NULL,
                  country_code = "US",
                  steam_realm = 1,
                  include = "basic_info",
                  apply_user_filters = FALSE,
                  paginate = FALSE,
                  max_pages = Inf) {
  args <- as.list(environment())
  query <- do.call(store_query_params, args)
  context <- do.call(store_browse_context, args)
  data_request <- do.call(store_browse_item_data_request, args)

  if (paginate) {
    query$start <- "%s"
  }

  input_json <- .make_input_json(
    query = query,
    context = context,
    data_request = data_request
  )

  params <- .make_params(input_json = input_json)
  res <- request_webapi(
    api = public_api(),
    interface = "IStoreQueryService",
    method = "Query",
    version = "v1",
    params = params,
    paginate = if (paginate) list(method = "input_json", limit = max_pages)
  )

  if (paginate) {
    matches <- NULL
    start <- seq_along(res)
    count <- 0
    res <- lapply(res, function(x) {
      x <- x$response
      if (is.null(matches)) {
        matches <<- x$metadata$total_matching_records
      }
      count <<- count + x$metadata$count
      x$store_items
    })
    res <- as_data_frame(rbind_list(res))
  } else {
    res <- res$response
    matches <- res$metadata$total_matching_records
    start <- res$metadata$start
    count <- res$metadata$count
    res <- as_data_frame(res$store_items)
  }

  attr(res, "matches") <- matches
  attr(res, "start") <- start
  attr(res, "count") <- count
  res
}


search_suggestions <- function(query,
                               released_only = FALSE,
                               coming_soon_only = FALSE,
                               only_free_items = FALSE,
                               exclude_free_items = FALSE,
                               type_filters = NULL,
                               tags = NULL,
                               regional_top_n = NULL,
                               global_top_n = NULL,
                               regional_longterm_top_n = NULL,
                               global_longterm_top_n = NULL,
                               sale_tagid = NULL,
                               hub_type = NULL,
                               hub_category = NULL,
                               hub_tagid = NULL,
                               discount_filter = NULL,
                               optin_name = NULL,
                               optin_tagid = NULL,
                               prune_tagid = NULL,
                               optin_only = FALSE,
                               language = "english",
                               elanguage = NULL,
                               country_code = "US",
                               steam_realm = 1,
                               include = NULL,
                               apply_user_filters = FALSE,
                               use_spellcheck = FALSE,
                               search_tags = FALSE,
                               search_creators = FALSE,
                               prefilter_creators = FALSE) {
  filters <- store_query_filters(
    released_only = FALSE,
    coming_soon_only = FALSE,
    only_free_items = FALSE,
    exclude_free_items = FALSE,
    type_filters = NULL,
    tags = NULL,
    regional_top_n = NULL,
    global_top_n = NULL,
    regional_longterm_top_n = NULL,
    global_longterm_top_n = NULL,
    sale_tagid = NULL,
    hub_type = NULL,
    hub_category = NULL,
    hub_tagid = NULL,
    discount_filter = NULL,
    optin_name = NULL,
    optin_tagid = NULL,
    prune_tagid = NULL,
    optin_only = FALSE
  )

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

  input_json <- .make_input_json(
    filters = filters,
    context = context,
    data_request = data_request
  )

  params <- .make_params(
    search_term = query,
    input_json = input_json,
    use_spellcheck = use_spellcheck,
    search_tags = search_tags,
    search_creators = search_creators,
    prefilter_creators = prefilter_creators
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


query_by_recommended_tags <- function(sort = NULL,
                                      sale_tagid = NULL,
                                      hub_type = NULL,
                                      hub_category = NULL,
                                      hub_tagid = NULL,
                                      discount_filter = NULL,
                                      optin_name = NULL,
                                      optin_tagid = NULL,
                                      prune_tagid = NULL,
                                      optin_only = FALSE,
                                      recommended_tag_count = NULL,
                                      min_items_per_tags = NULL,
                                      language = "english",
                                      elanguage = NULL,
                                      country_code = "US",
                                      steam_realm = 1L,
                                      sort_sections = NULL,
                                      min_items = NULL,
                                      include_packages = FALSE,
                                      include_bundles = FALSE) {
  check_authenticated()

  page_filter <- store_page_filter(
    sale_tagid = NULL,
    hub_type = NULL,
    hub_category = NULL,
    hub_tagid = NULL,
    discount_filter = NULL,
    optin_name = NULL,
    optin_tagid = NULL,
    prune_tagid = NULL,
    optin_only = FALSE
  )

  context <- store_browse_context(
    language = "english",
    elanguage = NULL,
    country_code = "US",
    steam_realm = 1L
  )

  input_json <- .make_input_json(page_filter = page_filter, context = context)
  params <- .make_params(
    sort = sort,
    input_json = input_json,
    recommended_tag_count = recommended_tag_count,
    min_items_per_tags = min_items_per_tags,
    context = context
  )

  request_webapi(
    api = public_api(),
    interface = "IStoreQueryService",
    method = "GetItemsByUserRecommendedTags",
    version = "v1",
    params = params
  )$response
}


store_query_params <- function(start = NULL, count = NULL, sort = NULL, ...) {
  check_number(start)
  check_number(count)
  check_number(sort)

  obj <- list(
    start = start,
    count = count,
    sort = sort,
    filters = store_query_filters(...)
  )
  class(obj) <- "StoreQueryParams"
  drop_null(obj) %empty% NULL
}


store_query_filters <- function(released_only = FALSE,
                                coming_soon_only = FALSE,
                                tags = NULL,
                                tags_exclude = NULL,
                                content_descriptors = NULL,
                                content_descriptors_exclude = NULL,
                                regional_top_n = NULL,
                                global_top_n = NULL,
                                regional_longterm_top_n = NULL,
                                global_longterm_top_n = NULL,
                                ...) {
  check_bool(released_only)
  check_bool(coming_soon_only)
  check_number(tags, null = TRUE)
  check_number(tags_exclude, null = TRUE)
  check_number(content_descriptors, null = TRUE)
  check_number(content_descriptors_exclude, null = TRUE)

  obj <- list(
    released_only = released_only,
    coming_soon_only = coming_soon_only,
    type_filters = store_query_type_filters(...),
    tagids_must_match = store_query_tag_filter(tags),
    tagids_exclude = box(tags_exclude),
    price_filters = store_query_price_filters(...),
    content_descriptors_must_match = box(content_descriptors),
    content_descriptors_excluded = box(content_descriptors_exclude),
    regional_top_n_sellers = regional_top_n,
    global_top_n_sellers = global_top_n,
    regional_long_term_top_n_sellers = regional_longterm_top_n,
    global_long_term_top_n_sellers = global_longterm_top_n
  )
  class(obj) <- "StoreQueryFilters"
  drop_false(drop_null(obj)) %empty% NULL
}


store_query_type_filters <- function(type_filters = NULL, ...) {
  check_string(type_filters, null = TRUE)

  info <- c(
    "apps", "packages", "bundles", "games", "demos", "mods",
    "dlc", "software", "video", "hardware", "series", "music"
  )

  if (identical(type_filters, "all")) {
    type_filters <- info
  }

  obj <- lapply(info, "%in%", type_filters)
  names(obj) <- paste0("include_", info)

  class(obj) <- "StoreQueryFilters_TypeFilters"
  drop_false(drop_null(obj)) %empty% NULL
}


store_query_tag_filter <- function(tagids = NULL, ...) {
  check_number(tagids, null = TRUE)

  obj <- list(list(tagids = box(tagids)))
  if (is.null(unlist(obj))) {
    obj <- list(NULL)
  }
  class(obj) <- "StoreQueryFilter_TagFilter"
  drop_null(obj) %empty% NULL
}


store_query_price_filters <- function(only_free_items = FALSE,
                                      exclude_free_items = FALSE,
                                      ...) {
  check_bool(only_free_items)
  check_bool(exclude_free_items)

  obj <- as.list(environment())
  class(obj) <- "StoreQueryFilters_PriceFilters"
  drop_false(obj) %empty% NULL
}


store_page_filter <- function(sale_tagid = NULL,
                              hub_type = NULL,
                              hub_category = NULL,
                              hub_tagid = NULL,
                              discount_filter = NULL,
                              optin_name = NULL,
                              optin_tagid = NULL,
                              prune_tagid = NULL,
                              optin_only = FALSE,
                              ...) {
  check_number(sale_tagid, null = TRUE)
  check_string(hub_type, null = TRUE)
  check_string(hub_category, null = TRUE)
  check_number(hub_tagid, null = TRUE)
  check_number(discount_filter, null = TRUE)
  check_string(optin_name, null = TRUE)
  check_number(optin_tagid, null = TRUE)
  check_number(prune_tagid, null = TRUE)
  check_bool(optin_only)

  obj <- list(
    sale_filter = list(sale_tagid = sale_tagid),
    content_hub_filter = list(
      hub_type = hub_type,
      hub_category = hub_category,
      hub_tagid = hub_tagid,
      discount_filter = discount_filter,
      optin = list(
        name = optin_name,
        optin_tagid = optin_tagid,
        prune_tagid = prune_tagid,
        optin_only = optin_only
      )
    )
  )
  class(obj) <- "StorePageFilter"
  drop_false(drop_null(obj)) %empty% NULL
}
