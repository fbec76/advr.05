#' @importFrom cli cli_warn
tnm_validate_dataset <- function(dataset) {
  stopifnot(is.character(dataset), length(dataset) == 1L)
  ok <- grepl("^[a-z]{2,}-(?:[0-9]|10)$", dataset)
  if (!ok) {
    stop("Dataset must look like 'area-level'", call. = FALSE)
  }
  invisible(dataset)
}

tnm_validate_date <- function(date, allow_star = TRUE) {
  if (inherits(date, "Date")) return(format(date, "%Y-%m-%d"))
  if (allow_star && identical(date, "*")) return(date)
  rx <- "^(\\d{4})(?:-(\\d{2})(?:-(\\d{2}))?)?$"
  if (!is.character(date) ||
    length(date) != 1L ||
    !grepl(rx, date)) {
    stop("Date must be 'YYYY', 'YYYY-MM', 'YYYY-MM-DD', or '*'.", call. = FALSE)
  }
  date
}

tnm_collapse_props <- function(x) {
  if (is.null(x)) return(NULL)
  stopifnot(is.character(x))
  paste(unique(x), collapse = "|")
}