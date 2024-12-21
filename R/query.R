#' Query apps (Web API)
#' @description
#' Query apps using filters. Much of the argument documentation is still
#' unknown. Also, the \code{IStoreQueryService} interface defines two more
#' methods \code{SearchSuggestions} and
#' \code{GetItemByUserCombinedTagsPriority}, which might be really useful but
#' do not seem to work. If you have any hints, please let me know!
#'
#' @param start Result page at which to start. The page count starts at 0.
#' If the page count exceeds the maximum pages, the output returns a
#' \code{count} attribute of 0.
#' @param count Number of results per page up to a maximum of 1,000. Defaults
#' to 10.
#' @param sort Number that seems to control the order of the apps when queried.
#' It is unknown what the number refers to.
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
#' \code{\link{wba_tags}}.
#' @param tags_exclude A vector of tagIDs. Only games \emph{not} matching
#' these tags will be returned. Tags are categories or genres given to a game
#' by the user community. For an overview of existing tagIDs, see
#' \code{\link{wba_tags}}.
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
#' used to filter by content hub aka genre.
#' @param optin_name,optin_tagid,prune_tagid,optin_only Unknown. Possibly
#' related to Beta opt-in.
#' @param iterate Whether to automatically iterate through pages. Depending
#' on the query, this can take a long time. The maximum number of requests
#' can be controlled using \code{options(steamr_max_reqs = ...)}.
#' @inheritParams wba_store_items
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
#' @section \code{sort} parameter:
#'
#' The \code{sort} parameter seems to change the output but I have no idea how.
#' \code{wba_query} returns a unique set of appIDs for the following values:
#' \code{c(1, 2, 3, 8, 9, 10, 11, 12, 13, 20, 21, 22, 30, 31, 32, 34, 40, 41, 50)}
#'
#' All other values passed to \code{sort} return the same set of appIDs as one
#' of the values specified above. The following script returns vector where each
#' index is a value passed to \code{sort} and each corresponding value is a
#' number between 1 to 50 that can be passed to \code{sort} to retrieve the same
#' sequence of appIDs.
#'
#' \preformatted{idsets <- cvapply(1:1000, \(i) paste(wba_query(sort = i)$id, collapse = ""))
#' unique_sets <- which(!duplicated(idsets))
#' names(unique_sets) <- idsets[which(!duplicated(idsets))]
#' unname(unique_sets[idsets])
#' #   [1]  1  2  3  3  3  3  3  8  9 10 11 12 13  3  3  8  3  3  3 20
#' #  [21] 21 22  3  3  3  3  3  3  3 30 31 32  3 34  3  3  3  3  3 40
#' #  [41] 41 34  9  3  3 34  3  3  3 50  3  3 34  3  3  3  8  3  3  3
#' #  [61]  8  8  3  3  3  3  3  3  3  3 34 34  3 34  9  9  3  3  3  3
#' #  [81] 34 34  3  3  3  3  3  8  8  3  3  8  8  3  3  3  9  3  9  3
#' # [101]  3  3  3  3  3  3  9  3  3 34  3  8  3  3  3  3  3  3 34  9
#' # [121]  3  3  3  3  3  3  3  3  3  3  3 34  3 34  3 34  3  3  3  3
#' # [141]  3 34 34  3  3 34  3  3  3  3  3  3  3  3  9  3  3 34  3  3
#' # [161]  3  9  3  9  9  3  3  3  3  3  3  3  9  3  3  3  3  3  3  3
#' # [181]  3  3  3  9  3 34  9  9  8  3  9  3  3  3  3  8  3  8  3  3
#' # [201] 34  3  9  3  8  3  3  3  3  8  3  9  3  9  9  3  3 34  3  3
#' # [221] 34  9  3 34  3  3  3  3  3  3  3  3  3  3  3 34 34  3  3  3
#' # [241] 34  9  3  3  3  3  9  9  3  9  3  3 34  3  3  3  8  3 34  3
#' # [261]  3  9 34  3  3  3 34  3  3  9 34  3  3  3  3  9  3  3  3  3
#' # [281]  3  3  3  3  3  3  3  3  3  3  8  3  3  3  3 34  3  3  3  9
#' # [301]  3  3  3  3  3  3  3  3  3 34  9  3  3 34  3  3  3  8 34  3
#' # [321]  3  3 34  3  3  3 34  9  3  3  3  3  3  8  3  3  3  3  8  3
#' # [341]  9 34  3  3  3  3  3  3  9  3  3  9  3  3  3  3  8 34  3  3
#' # [361]  9  3  3  3  3  3  3  3 34  3  3  8 34  3  8  3  3  8  3 34
#' # [381]  9 34 34  3  3  3  3  3  3 34  3  9  3  3  3  9  3  3  3  3
#' # [401]  3  3  3  3  3 34  3  9  9  3  3  3  3  3  3  9  3  3 34  9
#' # [421]  3  3  3  3  3  3  9  9  9  3  3  3  3  3  3  3  3  3 34  3
#' # [441]  3  9  3  3  3  9  3  3  8 34  3  9  9  3  8  3  3  8 34  8
#' # [461] 34  3  3  3  3  3  3  3  3  3  9  3  9  3  8  3  3  8  3  3
#' # [481]  3  3  3  3  3  3  3  3  3  3  8  3  3  3  3  3  3  3  3  3
#' # [501]  3 34  3  3  3  3  3  9  3  3 34  3  3  3  3  3  3 34 34  9
#' # [521]  3  9 34  3  3  9  3  3 34  3  3 34  3  3  3  3  9  3  3  3
#' # [541]  3  3  3  3  3  9  9  9  3  3  3  3  3  3 34  3  3  3  3  3
#' # [561]  3  3 34  3  3  3  9 34  3  3  3 34  3  3  3  3  3  3 34  3
#' # [581]  3  3  3  3  3  8  3  3  3  3  3  3  3  3  3  3 34  3  3  8
#' # [601]  3  9  3  3  3  3  9  9  3  3  3  3 34  3  3  3  3  3  3  8
#' # [621]  9  3  8  3  3  9  3  3  3  3  3  3  3  9  3  3  3  3 34  8
#' # [641]  3  3  3  3  3 34  3  9  9  3  3 34  3  3  3  3  3  3  3  3
#' # [661] 34  3  3  9  3  3  3  9  3  3  9  3  3 34  3  3  3  3  3  3
#' # [681]  3  3  9  3 34 34  3 34 34  3  8  3  3 34  3  3  3  3  3  3
#' # [701]  3  9  3  3  3  3  3  3  3 34  3  3  3  3  9  3  3  3  3  3
#' # [721]  9  3 34  3  3  3  3  3  3  3  3  3  3  3  3  3  9 34  3  3
#' # [741]  9  8  8 34  9  3  3  3  3  3  3 34  3  3  3  8  3  3  3  3
#' # [761]  3  9 34  3  3  3  9  3  3  3  3  3  3  3  3  3 34  3  3  9
#' # [781] 34  3  3  3  3  3 34  3  3  3  3  3  3  3  3  3 34  3  3  3
#' # [801] 34  3  3  3  3  8  3 34  3  3  3 34  3  3  3  3  3  9  3  3
#' # [821]  3  9  3  3  3  3  3  8  3  3  8  3 34  3  3  8  3  3  9  3
#' # [841]  3  3  3  3 34  3  3  3  3  3  3  3  3  9  9  3  3  3  9  3
#' # [861]  3  3 34  9  3  3  3 34  3  3  3 34  3  9  3  3 34  8  3  3
#' # [881]  3  3  3 34  8  8  3  9  9  3  3  3  3  8  3  8  3  3  3  8
#' # [901]  3  8  3  3  9  3  9  3  8  3  3  3  8  9  3  3  3  3  3  3
#' # [921] 34  3  3  3  3  8 34  3  3  3  8  3  9  3  3  3  9  9  3 34
#' # [941]  3  3  9  3  3  3  3  3  3  3  3  9  3  3 34  3  3 34  3  9
#' # [961]  3  3  3  8  3  3  3  3  3  3  3 34  3 34  3  9  3  3  9  3
#' # [981]  3  3  3  3 34  8  9  9 34  3  8  9  3  3  9  8  3  3  3  3
#' }
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # return the first 500 matches
#' wba_query(count = 500)
#'
#' # return the first 2500 matches
#' options(steamr_max_reqs = 5)
#' wba_query(count = 500, paginate = TRUE)
#'
#' # return page 5 of the query
#' wba_query(start = 6)
#'
#' # sort by item type
#' wba_query(sort = 1)
#'
#' # filter out mature content
#' cd <- content_descriptors()
#' cd <- cd[grepl("Mature Content", cd$description), ]$code
#' wba_query(content_descriptors_exclude = cd)
#'
#' # filter by strategy games
#' tags <- wba_tags()
#' tagids <- tags[grepl("Strategy", tags$name), ]$tagid
#' wba_query(tags = tagids)
#'
#' # out of all matches, return the 5 regional best-sellers
#' # note that "regional" refers to the store context provided
#' wba_query(regional_top_n = 5, context = store_context(country_code = "US"))
#' }
wba_query <- function(start = NULL,
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
                      context = store_context(),
                      data_request = store_data_request("basic_info"),
                      paginate = FALSE,
                      max_pages = Inf) {
  args <- as.list(environment())
  query <- do.call(store_query_params, args)
  assert_class(context, "StoreBrowseContext")
  assert_class(data_request, "StoreBrowseItemDataRequest")

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


# wba_suggest <- function(query,
#                         released_only = FALSE,
#                         coming_soon_only = FALSE,
#                         only_free_items = FALSE,
#                         exclude_free_items = FALSE,
#                         type_filters = NULL,
#                         tags = NULL,
#                         regional_top_n = NULL,
#                         global_top_n = NULL,
#                         regional_longterm_top_n = NULL,
#                         global_longterm_top_n = NULL,
#                         sale_tagid = NULL,
#                         hub_type = NULL,
#                         hub_category = NULL,
#                         hub_tagid = NULL,
#                         discount_filter = NULL,
#                         optin_name = NULL,
#                         optin_tagid = NULL,
#                         prune_tagid = NULL,
#                         optin_only = FALSE,
#                         context = store_context(),
#                         data_request = store_data_request("basic_info"),
#                         use_spellcheck = FALSE,
#                         search_tags = FALSE,
#                         search_creators = FALSE,
#                         prefilter_creators = FALSE) {
#   assert_class(context, "StoreBrowseContext")
#   assert_class(data_request, "StoreBrowseItemDataRequest")
#
#   filters <- store_query_filters(
#     released_only = FALSE,
#     coming_soon_only = FALSE,
#     only_free_items = FALSE,
#     exclude_free_items = FALSE,
#     type_filters = NULL,
#     tags = NULL,
#     regional_top_n = NULL,
#     global_top_n = NULL,
#     regional_longterm_top_n = NULL,
#     global_longterm_top_n = NULL,
#     sale_tagid = NULL,
#     hub_type = NULL,
#     hub_category = NULL,
#     hub_tagid = NULL,
#     discount_filter = NULL,
#     optin_name = NULL,
#     optin_tagid = NULL,
#     prune_tagid = NULL,
#     optin_only = FALSE
#   )
#
#   input_json <- .make_input_json(
#     filters = filters,
#     context = context,
#     data_request = data_request
#   )
#
#   params <- .make_params(
#     search_term = query,
#     input_json = input_json,
#     use_spellcheck = use_spellcheck,
#     search_tags = search_tags,
#     search_creators = search_creators,
#     prefilter_creators = prefilter_creators
#   )
#
#   res <- request_webapi(
#     api = public_api(),
#     interface = "IStoreQueryService",
#     method = "SearchSuggestions",
#     version = "v1",
#     params = params
#   )
#   res
# }
#
#
# wba_query_by_tags <- function(sort = NULL,
#                               sale_tagid = NULL,
#                               hub_type = NULL,
#                               hub_category = NULL,
#                               hub_tagid = NULL,
#                               discount_filter = NULL,
#                               optin_name = NULL,
#                               optin_tagid = NULL,
#                               prune_tagid = NULL,
#                               optin_only = FALSE,
#                               recommended_tag_count = NULL,
#                               min_items_per_tags = NULL,
#                               context = store_context(),
#                               sort_sections = NULL,
#                               min_items = NULL,
#                               include_packages = FALSE,
#                               include_bundles = FALSE) {
#   check_authenticated()
#   assert_class(context, "StoreBrowseContext")
#
#   page_filter <- store_page_filter(
#     sale_tagid = NULL,
#     hub_type = NULL,
#     hub_category = NULL,
#     hub_tagid = NULL,
#     discount_filter = NULL,
#     optin_name = NULL,
#     optin_tagid = NULL,
#     prune_tagid = NULL,
#     optin_only = FALSE
#   )
#
#   input_json <- .make_input_json(page_filter = page_filter, context = context)
#   params <- .make_params(
#     sort = sort,
#     input_json = input_json,
#     recommended_tag_count = recommended_tag_count,
#     min_items_per_tags = min_items_per_tags,
#     context = context
#   )
#
#   request_webapi(
#     api = public_api(),
#     interface = "IStoreQueryService",
#     method = "GetItemsByUserRecommendedTags",
#     version = "v1",
#     params = params
#   )$response
# }


store_query_params <- function(start = NULL, count = NULL, sort = NULL, ...) {
  assert_integerish(start, null.ok = TRUE)
  assert_count(count, null.ok = TRUE)
  assert_integerish(sort, null.ok = TRUE)

  obj <- list(
    start = start,
    count = count,
    sort = sort,
    filters = store_query_filters(...)
  )

  obj <- drop_null(obj)
  class(obj) <- c("StoreQueryParams", "steam_object")
  obj
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
  assert_flag(released_only)
  assert_flag(coming_soon_only)
  assert_numeric(tags, null.ok = TRUE)
  assert_integerish(tags_exclude, null.ok = TRUE)
  assert_integerish(content_descriptors, null.ok = TRUE)
  assert_integerish(content_descriptors_exclude, null.ok = TRUE)

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
    global_long_term_top_n_sellers = global_longterm_top_n,
    store_page_filter = store_page_filter(...)
  )

  class(obj) <- c("StoreQueryFilters", "steam_object")
  drop_false(drop_null(obj)) %empty% NULL
}


store_query_type_filters <- function(type_filters = NULL, ...) {
  assert_string(type_filters, null.ok = TRUE)

  info <- c(
    "apps", "packages", "bundles", "games", "demos", "mods",
    "dlc", "software", "video", "hardware", "series", "music"
  )

  if (identical(type_filters, "all")) {
    type_filters <- info
  }

  obj <- lapply(info, "%in%", type_filters)
  names(obj) <- paste0("include_", info)

  class(obj) <- c("StoreQueryFilters_TypeFilters", "steam_object")
  drop_false(drop_null(obj)) %empty% NULL
}


store_query_tag_filter <- function(tagids = NULL, ...) {
  assert_integerish(tagids, null.ok = TRUE)

  obj <- list(list(tagids = box(tagids)))
  if (is.null(unlist(obj))) {
    obj <- list()
  }
  class(obj) <- c("StoreQueryFilter_TagFilter", "steam_object")
  drop_null(obj) %empty% NULL
}


store_query_price_filters <- function(only_free_items = FALSE,
                                      exclude_free_items = FALSE,
                                      ...) {
  assert_flag(only_free_items)
  assert_flag(exclude_free_items)

  obj <- as.list(environment())

  class(obj) <- c("StoreQueryFilters_PriceFilters", "steam_object")
  drop_false(obj) %empty% NULL
}


store_page_filter <- function(...) {
  obj <- list(
    sale_filter = store_sale_page_filter(...),
    content_hub_filter = store_contenthub_filter(...)
  )

  class(obj) <- c("StorePageFilter", "steam_object")
  drop_false(drop_null(obj)) %empty% NULL
}


store_sale_page_filter <- function(sale_tagid = NULL, ...) {
  assert_integerish(sale_tagid, null.ok = TRUE)

  obj <- list(sale_tagid = sale_tagid)
  class(obj) <- c("StorePageFilter_SalePageFilter", "steam_object")
  drop_null(obj) %empty% NULL
}


store_contenthub_filter <- function(hub_type = NULL,
                                    hub_category = NULL,
                                    hub_tagid = NULL,
                                    discount_filter = NULL,
                                    ...) {
  assert_string(hub_type, null.ok = TRUE)
  assert_string(hub_category, null.ok = TRUE)
  assert_integerish(hub_type, null.ok = TRUE)
  assert_integerish(discount_filter, null.ok = TRUE)

  obj <- list(
    hub_type = hub_type,
    hub_category = hub_category,
    hub_tagid = hub_tagid,
    discount_filter = discount_filter,
    optin = store_optin_info(...)
  )

  class(obj) <- c("StorePageFilter_ContentHubFilter", "steam_object")
  drop_false(drop_null(obj)) %empty% NULL
}


store_optin_info <- function(optin_name = NULL,
                             optin_tagid = NULL,
                             prune_tagid = NULL,
                             optin_only = FALSE,
                             ...) {
  assert_string(optin_name, null.ok = TRUE)
  assert_integerish(optin_tagid, null.ok = TRUE)
  assert_integerish(prune_tagid, null.ok = TRUE)
  assert_flag(optin_only, null.ok = TRUE)

  obj <- list(
    name = optin_name,
    optin_tagid = optin_tagid,
    prune_tagid = prune_tagid,
    optin_only = optin_only
  )

  class(obj) <- c("StorePageFilter_ContentHubFilter_OptInInfo", "steam_object")
  drop_false(drop_null(obj)) %empty% NULL
}
