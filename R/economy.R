get_asset_prices <- function(appid, currency = NULL, language = "english") {
  check_number(appid)
  check_string(currency, null = TRUE)
  check_string(language)

  params <- .make_params()
  res <- request_webapi(
    api = public_api(),
    interface = "ISteamEconomy",
    method = "GetAssetPrices",
    version = "v1",
    params = params
  )$result$assets
  as_data_frame(res)
}


get_asset_class <- function(appid,
                            classid0,
                            class_count = 1L,
                            language = "english",
                            instanceid0 = NULL) {
  check_number(appid)
  check_number(classid0)
  check_integerish(class_count)
  check_string(language)
  check_number(instanceid0, null = TRUE)

  params <- .make_params()
  request_webapi(
    api = public_api(),
    interface = "ISteamEconomy",
    method = "GetAssetClassInfo",
    version = "v1",
    params = params
  )$result
}


get_trade_history <- function() {

}
