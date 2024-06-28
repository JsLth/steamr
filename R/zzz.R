.onLoad <- function(libname, pkgname) {
  cache_file <- cache_path("auth_cache.rds")

  if (file.exists(cache_file)) {
    cache <- readRDS(cache_file)
    set_auth(cache$auth, cache$session)
  }
}


cache_path <- function(file = NULL) {
  path <- rappdirs::user_cache_dir("steamr")
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  path <- normalizePath(path, "/")
  file.path(path, file %||% "")
}
