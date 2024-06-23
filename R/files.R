#' Published files
#' @description
#' Query published files.
#'
#' @param query_type How to rank the query. Can be a code defined in
#' \code{\link{EPublishedFileQueryType}}.
#' @param numperpage Number of results per page, up to a maximum of 100.
#' @param creator_appid AppID to filter by. Only files created by this
#' appID will be returned.
#' @param consumer_appid AppID to filter by. Only files consumed by this
#' appID will be returned.
#' @param required_tags Tags that must be present in the queried files. If
#' \code{match_all_tags} is  \code{TRUE}, then all tags must be present.
#' @param excluded_tags Tags that must not be present in the queried files.
#' @param match_all_tags If \code{TRUE}, then all tags in \code{required_tags}
#' must be present in the queried files, otherwise only at least one.
#' @param required_flags Flags that must be present in the queried files.
#' @param omitted_flags Flags that must not be present in the queried files.
#' @param search_text Text to match in the file's title or description.
#' @param filetype File types to return. Can be one of the codes in
#' \code{\link{EPublishedFileInfoMatchingFileType}}.
#' @param child_publishedfileid PublishedfileID that must be referenced by
#' the queried files.
#' @param days If \code{query_type = 3} (ranked by trend), specifies the
#' number of days to get votes for (between 1 and 7 days).
#' @param only_recent_votes If \code{query_type = 3} (ranked by trend),
#' specifies whether result should only include files that have been voted
#' on in the last \code{days} days.
#' @param cache_max_age_seconds Maximum file age in seconds. If set, files
#' are allowed to get stale by the amount of time specified.
#' @param total_only If \code{TRUE}, only returns the total number of queried
#' files.
#' @param ids_only If \code{TRUE}, only returns the queried publishedfileIDs.
#' Takes precedence over the \code{return} argument.
#' @param return A character vector specifying the information that should be
#' returned. Can be several of the following: \code{vote_data}, \code{tags},
#' \code{kv_tags}, \code{previews}, \code{children}, \code{short_description},
#' \code{for_sale_data}, \code{metadata}, \code{playtime_stats}, \code{details}
#' and \code{reactions}. \code{details} returns a default set of details.
#' If \code{playtime_stats}, returns playtime stats for the number of days
#' specified in \code{playtime_days}. \code{children} returns the
#' publishedfileIDs referenced by the published files. \code{for_sale_data}
#' returns pricing information where applicable. \code{short_description}
#' replaces the \code{file_description} with a \code{short_description} field
#' containing a shorter description.
#' @param playtime_days If \code{return} includes \code{playtime_stats},
#' specifies the number of days for which to return stats.
#' @param strip_bbcode If \code{TRUE}, strips all BB code from descriptions.
#' @param desired_revision Specifies the state of file revision. Can be a
#' code specified in \code{\link{EPublishedFileRevision}}.
#'
#' @returns \describe{
#'  \item{\code{query_files}}{A dataframe containing least the request result
#'  code, the publishedfileID and the SteamID of the creator. Depending on
#'  the \code{return} argument, further details are provided. If
#'  \code{total_only = TRUE}, returns a length-1 vector containing the total
#'  number of queried files. If \code{ids_only}, forces the output to return
#'  the minimum amount of information (publishedfileIDs and SteamIDs only).}
#'
#'  \item{\code{get_published_file}}{A dataframe with one row containing
#'  default details of a published file. Depending on the \code{return}
#'  argument, further details are provided. Default details are always
#'  the minimum amount of information returned.}
#' }
#'
#' @export
#'
#' @examples
#' # query the first three pages
#' options(steamr_max_reqs = 3)
#' query_files()
#'
#' # return essential details and pricing data
#' query_files(return = c("details", "for_sale_data"))
#'
#' # query only for artworks
#' # an overview of filetypes can be retrieved using:
#' # EPublishedFileInfoMatchingFileType()
#' query_files(filetype = 3)
#'
#' # query only workshop collections
#' query_files(filetype = 1)
#'
#' # query all files that were produced by team fortress
#' query_files(creator_appid = 440)
#'
#' # get the number of files produced by team fortress
#' query_files(total_only = TRUE)
#'
#' # retrieve files published in portuguese language
#' query_files(language = 4)
query_files <- function(query_type = NULL,
                        numperpage = 100L,
                        creator_appid = NULL,
                        consumer_appid = NULL,
                        required_tags = NULL,
                        excluded_tags = NULL,
                        match_all_tags = FALSE,
                        required_flags = NULL,
                        omitted_flags = NULL,
                        search_text = NULL,
                        filetype = NULL,
                        child_publishedfileid = NULL,
                        days = NULL,
                        only_recent_votes = FALSE,
                        cache_max_age_seconds = NULL,
                        elanguage = 0L,
                        total_only = FALSE,
                        ids_only = FALSE,
                        return = "details",
                        playtime_days = NULL,
                        strip_bbcode = FALSE,
                        desired_revision = NULL,
                        paginate = TRUE) {
  check_number(query_type, null = TRUE)
  check_integerish(numperpage)
  check_number(creator_appid, null = TRUE)
  check_number(consumer_appid, null = TRUE)
  check_string(required_tags, null = TRUE)
  check_string(excluded_tags, null = TRUE)
  check_bool(match_all_tags)
  check_string(required_flags, null = TRUE)
  check_string(omitted_flags, null = TRUE)
  check_string(search_text, null = TRUE)
  check_number(filetype, null = TRUE)
  check_number(child_publishedfileid, null = TRUE)
  check_integerish(days, null = TRUE)
  check_bool(only_recent_votes)
  check_integerish(cache_max_age_seconds, null = TRUE)
  check_integerish(elanguage)
  check_bool(total_only)
  check_bool(ids_only)
  check_string(return, null = TRUE)
  check_bool(strip_bbcode)
  check_number(desired_revision, null = TRUE)

  return <- publishedfile_pseudo_data_request(return, playtime_days)

  params <- as.list(c(
    query_type = query_type,
    numperpage = numperpage,
    creator_appid = creator_appid,
    appid = consumer_appid,
    requiredtags = required_tags,
    excluded_tags = excluded_tags,
    match_all_tags = match_all_tags,
    required_flags = required_flags,
    omitted_flags = omitted_flags,
    search_text = search_text,
    filetype = filetype,
    child_publishedfileid = child_publishedfileid,
    days = days,
    include_recent_votes_only = only_recent_votes,
    cache_max_age_seconds = cache_max_age_seconds,
    language = elanguage,
    total_only = total_only,
    ids_only = ids_only,
    return,
    strip_description_bbcode = strip_bbcode,
    desired_revision = desired_revision,
    cursor = "*"
  ))
  params <- do.call(.make_params, params)

  res <- request_webapi(
    api = public_api(),
    interface = "IPublishedFileService",
    method = "QueryFiles",
    version = "v1",
    params = params,
    paginate = if (paginate) "cursor"
  )

  if (paginate) {
    total <- NULL
    res <- lapply(res, function(x) {
      x <- x$response
      if (is.null(total)) {
        total <<- x$total
      }
      x$publishedfiledetails
    })
    res <- as_data_frame(rbind_list(res))
  } else {
    total <- res$total

    if (total_only) {
      res <- total
    } else {
      res <- as_data_frame(res$publishedfiledetails)

    }
  }

  attr(res, "total") <- total
  res
}


#' @rdname query_files
#' @export
#' @param publishedfileid A vector of publishedfileID of a file to get details
#' for.
get_published_file <- function(publishedfileids,
                               return = NULL,
                               playtime_days = NULL,
                               language = "english",
                               appid = NULL,
                               strip_bbcode = FALSE,
                               desired_revision = NULL) {
  check_number(publishedfileids)
  check_string(return, null = TRUE)
  check_integerish(playtime_days, null = TRUE)
  check_string(language)
  check_number(appid, null = TRUE)
  check_bool(strip_bbcode)
  check_number(desired_revision, null = TRUE)

  return <- publishedfile_pseudo_data_request(return, playtime_days, "details")
  publishedfileids <- box(publishedfileids)

  params <- as.list(c(
    publishedfileids = list(publishedfileids),
    return,
    language = language,
    sppid = appid,
    strip_description_bbcode = strip_bbcode,
    desired_revision = desired_revision,
    access_token = FALSE
  ))
  params <- do.call(.make_params, params)

  res <- request_webapi(
    api = public_api(),
    interface = "IPublishedFileService",
    method = "GetDetails",
    version = "v1",
    params = params
  )$response$publishedfiledetails
  as_data_frame(res)
}


publishedfile_pseudo_data_request <- function(return,
                                              playtime_days,
                                              method = "query",
                                              ...) {
  if (identical(method, "details") && "details" %in% return) {
    warning(paste(
      "Further details cannot be provided in single file request.\n",
      "\"details\" will be dropped from return parameter."
    ))
    return <- setdiff(return, "details")
  }

  if ("playtime_stats" %in% return && is.null(playtime_days)) {
    warning(paste(
      "\"playtime_stats\" included in return argument, but no playtime_days set.",
      "\nplaytime_stats will be omitted."
    ))
  }

  info <- data.frame(
    query = c(
      "vote_data", "tags", "kv_tags", "previews", "children", "short_description",
      "for_sale_data", "metadata", "playtime_stats", "details", "reactions"
    ),
    details = c(
      "votes", "tags", "kvtags", "additionalpreviews", "children", "short_description",
      "forsaledata", "metadata", "playtime_stats", "details", "reactions"
    )
  )

  obj <- lapply(info$query, "%in%", return)
  prefix <- ifelse(identical(method, "query"), "return_", "include")
  names(obj) <- paste0(prefix, info[[method]])

  if (identical(method, "details")) {
    names(obj)[names(obj) %in% "includeshort_description"] <- "short_description"
  }

  if ("return_playtime_stats" %in% return && !is.null(playtime_stats)) {
    obj$return_playtime_stats <- playtime_stats
  }

  drop_false(drop_null(obj)) %empty% NULL
}


#' @rdname query_files
#' @export
get_user_files <- function(steamid,
                           creator_appid = NULL,
                           consumer_appid = NULL,
                           shortcutid = NULL,
                           numperpage = 100,
                           filetype = NULL,
                           requiredtags = NULL,
                           excludedtags = NULL,
                           cache_mag_age_seconds = NULL,
                           language = "english",
                           excluded_content_descriptors = NULL,
                           total_only = FALSE,
                           ids_only = FALSE,
                           return = NULL,
                           playtime_days = NULL,
                           desired_revision = NULL) {

}
