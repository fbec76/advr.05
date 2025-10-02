#' @importFrom jsonlite fromJSON
#' @importFrom httr2 resp_body_string
tnm_parse_json_df <- function(resp) {
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
}

#' @importFrom jsonlite fromJSON
#' @importFrom httr2 resp_body_string
#' @importFrom geojsonsf geojson_sf
#' @importFrom geojsonio topojson_read
tnm_parse_geo <- function(resp, geo_type) {
  json <- httr2::resp_body_string(resp)
  if (geo_type == "geojson") {
    geojsonsf::geojson_sf(json)
  } else {
    tf <- tempfile(fileext = ".json")
    writeLines(json, tf)
    obj <- geojsonio::geojson_read(tf, what = "sp")
    if (requireNamespace("sf", quietly = TRUE)) return(sf::st_as_sf(obj))
    obj
  }
}