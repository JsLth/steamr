query_files <- function() {

}


get_published_file <- function(publishedfileid) {
  params <- .make_params(access_token = FALSE)
  res <- request_webapi(
    api = public_api(),
    interface = "IPublishedFileService",
    method = "GetDetails",
    version = "v1",
    params = params
  )$response$publishedfiledetails
  as_data_frame(res)
}
