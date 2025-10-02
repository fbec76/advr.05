#' Retrieve Metadata Information About a TNM Dataset
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 resp_body_string
#'
#'
#' @param dataset Name or ID of the TNM dataset.
#' @param version Dataset version (default from config).
#' @param language Language code (default from config).
#'
#' @return A list containing metadata and properties of the dataset.
#'
#' @export
tnm_info <- function(dataset, version = tnm_get_config()$version, language = tnm_get_config()$language) {
  q <- list()
  if (!is.na(language)) q$language <- language
  req <- tnm_request(dataset, "info", "*", q)
  resp <- tnm_perform(req)
  tnm_parse_json_df(resp)
}