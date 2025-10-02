#' Retrieve an SVG Map from a TNM Dataset
#'
#' Downloads an SVG image for a specified TNM dataset and date, with options for projection, size, and properties.
#'
#' @importFrom httr2 resp_body_string
#'
#' @param dataset Name or ID of the TNM dataset.
#' @param date Date to retrieve data for.
#' @param svg_proj Optional projection name for the SVG.
#' @param svg_width Width of the SVG in pixels (default 600).
#' @param svg_height Height of the SVG in pixels (default 600).
#' @param svg_props Optional vector of SVG property names to include.
#' @param language Language code (default from config).
#' @param version Dataset version (default from config).
#' @param path If set, path to save the SVG file. If `NULL`, returns SVG as a string.
#'
#' @return SVG string if `path` is `NULL`, otherwise invisibly returns the file path.
#'
#' @export
tnm_svg <- function(dataset, date,
                    svg_proj = NULL, svg_width = 600, svg_height = 600,
                    svg_props = NULL,
                    language = tnm_get_config()$language,
                    version = tnm_get_config()$version,
                    path = NULL) {
  q <- list(svg_width = svg_width, svg_height = svg_height)
  if (!is.null(svg_proj)) q$svg_proj <- svg_proj
  if (!is.null(svg_props)) q$svg_props <- tnm_collapse_props(svg_props)
  if (!is.na(language)) q$language <- language
  req <- tnm_request(dataset, "svg", date, q)
  resp <- tnm_perform(req)
  svg <- httr2::resp_body_string(resp)
  if (!is.null(path)) {
    writeLines(svg, path, useBytes = TRUE)
    return(invisible(path))
  }
  svg
}