get_app_list <- function() {
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamApps",
    method = "GetAppList",
    version = "v2",
    params = params
  )

  as_data_frame(res$applist$apps)
}


up_to_date_check <- function(appid, version) {
  check_integerish(appid)
  check_integerish(version)

  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "ISteamApps",
    method = "UpToDateCheck",
    version = "v1",
    params = params
  )$response
}
