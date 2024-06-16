#' Query loyalty rewards
#' @description
#' Request loyalty reward items using filter queries.
#'
#' @param search_term Arbitrary search term to filter loyalty rewards. It is
#' unclear which field is actually searched by this parameter but it is
#' possible to search for games.
#' @param appids App IDs to query for.
#' @param time_available Datetime object specifying at which time the reward
#' must be available. Currently, this is kind of useless as there are no
#' reward items whose availability has ended.
#' @param community_item_classes Filter by community item class. Community
#' item classes seem to take up whole numbers between 1 and 15, but the
#' meaning of these numbers is not entirely clear. See details for details.
#' @param language Language of the returned descriptions.
#' @param count Count of items per page. Defaults to 100. By default, all
#' pages are iterated. The `count` argument is only relevant to either control
#' the number of total requests sent or when setting
#' `options(steamr_max_reqs = ...)`.
#' @param sort Unknown.
#' @param sort_descending Whether to sort descending by app ID.
#'
#' @details
#' Based on some rudimentary observations,
#'
#' | Type | Description |
#' |------|-------------|
#' | 1    | Item        |
#' | 2    | Collection  |
#' | 6    | Bundle      |
#'
#'
#'
#' @examples
#' \dontrun{
#' # query the entire database (this takes a while)
#' query_loyalty_rewards()
#'
#' # query the entire database but request only the first 2000 items
#' options(steamr_max_reqs = 20)
#' query_loyalty_rewards()
#'
#' # it is better to reduce the query volume, for example by search term
#' query_loyalty_rewards("team fortress")
#'
#' # ... or by game
#' query_loyalty_rewards(appids = c(10, 440))
#' }
#'
query_loyalty_rewards <- function(search_term = NULL,
                                  appids = NULL,
                                  time_available = NULL,
                                  community_item_classes = NULL,
                                  language = "english",
                                  count = NULL,
                                  sort = NULL,
                                  sort_descending = FALSE,
                                  reward_types = NULL,
                                  excluded_community_item_classes = NULL,
                                  definitionids = NULL,
                                  filters = NULL,
                                  filter_match_all_category_tags = NULL,
                                  filter_match_any_category_tags = NULL,
                                  contains_definitionids = NULL,
                                  include_direct_purchase_disabled = FALSE,
                                  excluded_content_descriptors = NULL,
                                  excluded_appids = NULL) {
  check_string(search_term, null = TRUE)

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ILoyaltyRewardsService",
    method = "QueryRewardItems",
    version = "v1",
    params = params,
    paginate = "cursor"
  )
  res <- lapply(res, function(x) x$response$definitions)
  as_data_frame(rbind_list(res))
}


get_loyalty_apps <- function() {
  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "ILoyaltyRewardsService",
    method = "GetEligibleApps",
    version = "v1"
  )$response
}
