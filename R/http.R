#' @importFrom httr2 request req_url_path_append req_timeout resp_check_status
tnm_request <- function(dataset, module, date, query = list()) {
  cfg <- tnm_get_config()
  tnm_validate_dataset(dataset)
  date <- tnm_validate_date(date, allow_star = TRUE)
  req <- httr2::request(cfg$base_url) |>
    httr2::req_url_path_append(cfg$version, dataset, module, date) |>
    httr2::req_timeout(cfg$timeout)
  if (length(query)) req <- httr2::req_url_query(req, !!!query)
  req
}

tnm_perform <- function(req) {
  resp <- req |>
    httr2::req_perform()
  httr2::resp_check_status(resp)
  resp
}