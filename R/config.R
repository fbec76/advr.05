tnm_default_base_url <- function() {
  getOption("thenmap.base_url", "http://api.thenmap.net")
}

tnm_default_version <- function() {
  getOption("thenmap.version", "v2")
}

tnm_default_language <- function() {
  getOption("thenmap.language", "en")
}

tnm_default_timeout <- function() {
  getOption("thenmap.timeout", 30)
}

#' Set a custom configuration for the thenmap API client
#'
#' @param base_url URL used for the API endpoint
#' @param version Version of the API
#' @param language Default language to be used for API calls
#' @param timeout Default timeout to exit http request
#'
#' @export
tnm_set_config <- function(base_url = NULL, version = NULL, language = NULL, timeout = NULL) {
  if (!is.null(base_url)) options(thenmap.base_url = base_url)
  if (!is.null(version)) options(thenmap.version = version)
  if (!is.null(language)) options(thenmap.language = language)
  if (!is.null(timeout)) options(thenmap.timeout = timeout)
  invisible(tnm_get_config())
}

tnm_get_config <- function() {
  list(
    base_url = tnm_default_base_url(),
    version = tnm_default_version(),
    language = tnm_default_language(),
    timeout = tnm_default_timeout()
  )
}