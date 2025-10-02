#' Retrieve and Parse Data from a Thenmap Dataset
#'
#' @param dataset Name or ID of the Thenmap dataset.
#' @param date Optional date (default "*").
#' @param data_props Vector of requested data property names (default `c("id", "name")`).
#' @param language Language code (default from config).
#' @param version Dataset version (default from config).
#'
#' @return A  data frame or list with the downloaded dataset.
#'
#' @export
tnm_data <- function(dataset, date = "*", data_props = c("id", "name"),
                     language = tnm_get_config()$language,
                     version = tnm_get_config()$version) {
  q <- list(data_props = tnm_collapse_props(data_props))
  if (!is.na(language)) q$language <- language
  req <- tnm_request(dataset, "data", date, q)
  resp <- tnm_perform(req)
  tnm_parse_json_df(resp)
}