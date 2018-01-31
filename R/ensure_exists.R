#' Download a file if it does not exist
#'
#' @param path The destination path on disk
#' @param url The source URL
#' @export
ensure.exists = function(path, url) {
  if (!file.exists(path)) {
    download.file(url, path)
  }
}
