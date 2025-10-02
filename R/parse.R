#' @importFrom jsonlite fromJSON
#' @importFrom httr2 resp_body_string
tnm_parse_json_df <- function(resp) {
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
}

#' @importFrom readr read_csv
#' @importFrom httr2 resp_body_string
tnm_parse_csv_df <- function(resp) {
  readr::read_csv(httr2::resp_body_string(resp), show_col_types = FALSE)
}

#' @importFrom jsonlite fromJSON
#' @importFrom httr2 resp_body_string
#' @importFrom geojsonsf geojson_sf
#' @importFrom geojsonio topojson_read
tnm_parse_geo <- function(resp, geo_type) {
  txt <- httr2::resp_body_string(resp)
  if (geo_type == "geojson") {
    geojsonsf::geojson_sf(txt)
  } else {
    obj <- geojsonio::topojson_read(txt, what = "sp")
    if (requireNamespace("sf", quietly = TRUE)) return(sf::st_as_sf(obj))
    obj
  }
}