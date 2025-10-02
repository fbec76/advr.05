#' Retrieve Geospatial Data from a Thenmap Dataset
#'
#' @param dataset Name or ID of the Thenmap dataset.
#' @param date Date to retrieve data for.
#' @param geo_type Geospatial format: `"geojson"` or `"topojson"` (default `"geojson"`).
#' @param geo_crs Coordinate reference system, e.g., `"wgs84"` (default).
#' @param geo_props Vector of requested geographic property names (optional).
#' @param language Language code (default from config).
#' @param version Dataset version (default from config).
#'
#' @return An `{sf}` object or parsed geospatial list.
#'
#' @export
tnm_geo <- function(dataset, date,
                    geo_type = c("geojson", "topojson"),
                    geo_crs = "wgs84",
                    geo_props = NULL,
                    language = tnm_get_config()$language,
                    version = tnm_get_config()$version) {
  geo_type <- match.arg(geo_type)
  q <- list(geo_type = geo_type, geo_crs = geo_crs)
  if (!is.null(geo_props)) q$geo_props <- tnm_collapse_props(geo_props)
  if (!is.na(language)) q$language <- language
  req <- tnm_request(dataset, "geo", date, q)
  resp <- tnm_perform(req)
  tnm_parse_geo(resp, geo_type)
}