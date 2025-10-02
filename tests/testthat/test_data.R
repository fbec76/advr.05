test_that("[tnm_data] works with mocked response", {
  mock_response <- structure(list(), class = "httr2_response")

  with_mocked_bindings(
    tnm_request = function(dataset, module, date, query) {
      mock_response
    },
    tnm_perform = function(...) mock_response,
    tnm_parse_json_df = function(...) {
      data.frame(
        id = "1",
        name = "Country",
        area = 12345,
        stringsAsFactors = FALSE
      )
    },
  {
    result <- tnm_data(
      dataset = "dataset-1",
      date = "2025-01-01",
      data_props = c("id", "name"),
      language = "lang"
    )

    expect_equal(nrow(result), 1)
    expect_true("area" %in% colnames(result))
  }
  )
})

test_that("[tnm_data] uses wildcard date by default", {
  mock_response <- structure(list(), class = "httr2_response")

  with_mocked_bindings(
    tnm_request = function(dataset, module, date, query) {
      expect_equal(date, "*")
      mock_response
    },
    tnm_perform = function(...) mock_response,
    tnm_parse_json_df = function(...) {
      data.frame(
        id = "wildcard-1",
        name = "Placeholder Wildcard",
        stringsAsFactors = FALSE
      )
    },
  {
    result <- tnm_data("wildcard-1")
  }
  )
})

# ! Actual API call test !
test_that("[tnm_data] can connect to actual API (requires internet)", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch({
    tnm_data("world-2", date = "2020", data_props = c("id", "name"))
  }, error = function(e) {
    skip(paste("API not available:", e$message))
  })
  expect_true(nrow(result) > 0)
  expect_true("id" %in% colnames(result))
  expect_true("name" %in% colnames(result))
})
