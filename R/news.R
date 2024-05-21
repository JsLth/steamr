get_news <- function(appid,
                     maxlength = NULL,
                     enddate = NULL,
                     count = 20,
                     feeds = NULL,
                     tags = NULL) {
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamNews",
    method = "GetNewsForApp",
    version = "v2",
    params = params
  )
  as_data_frame(res$appnews$newsitems)
}
