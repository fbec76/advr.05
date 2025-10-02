test_that("[tnm_info] works with mocked response", {
  mock_response <- structure(list(), class = "httr2_response")

  mock_info_data <- list(
    id = "dataset-1",
    name = "Test Dataset",
    description = "A test dataset for validation",
    properties = list(
      id = list(type = "string", description = "Identifier"),
      name = list(type = "string", description = "Name")
    ),
    version = "v1.0",
    lastUpdated = "2025-01-01"
  )

  with_mocked_bindings(
    tnm_request = function(dataset, module, date, query) {
      expect_equal(module, "info")
      expect_equal(date, "*")
      mock_response
    },
    tnm_perform = function(...) mock_response,
    tnm_parse_json_df = function(...) mock_info_data,
  {
    result <- tnm_info(
      dataset = "dataset-1",
      language = "en"
    )

    expect_type(result, "list")
    expect_equal(result$id, "dataset-1")
    expect_equal(result$name, "Test Dataset")
    expect_true("description" %in% names(result))
  }
  )
})

test_that("[tnm_info] handles complex metadata structure", {
  mock_response <- structure(list(), class = "httr2_response")

  complex_info_data <- list(
    id = "complex-dataset",
    name = "Complex Dataset",
    description = "A dataset with complex metadata",
    properties = list(
      id = list(type = "string", description = "Unique identifier", required = TRUE),
      name = list(type = "string", description = "Display name", required = TRUE),
      area = list(type = "numeric", description = "Geographic area", required = FALSE),
      population = list(type = "integer", description = "Population count", required = FALSE)
    ),
    categories = c("geography", "demographics"),
    tags = c("world", "countries", "statistics"),
    license = "CC-BY-4.0",
    source = "TNM Official Data",
    temporal = list(
      start = "2020-01-01",
      end = "2025-12-31",
      frequency = "annual"
    )
  )

  with_mocked_bindings(
    tnm_request = function(dataset, module, date, query) {
      mock_response
    },
    tnm_perform = function(...) mock_response,
    tnm_parse_json_df = function(...) complex_info_data,
  {
    result <- tnm_info("complex-dataset")

    expect_type(result, "list")
    expect_equal(result$id, "complex-dataset")
    expect_equal(result$categories, c("geography", "demographics"))
    expect_true("temporal" %in% names(result))
    expect_equal(result$temporal$frequency, "annual")
  }
  )
})

# ! Actual API call test !
test_that("[tnm_info] can connect to actual API (requires internet)", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch({
    tnm_info("world-2", language = "en")
  }, error = function(e) {
    skip(paste("API not available:", e$message))
  })

  print(colnames(result))
  expect_type(result, "list")
  expect_true("data_props" %in% names(result))
})

