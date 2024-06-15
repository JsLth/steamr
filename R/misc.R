get_servers <- function(filter = NULL, limit = NULL) {
  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "IGameServersService",
    method = "GetServerList",
    version = "v1",
    params = params
  )$response$servers
  as_data_frame(res)
}
