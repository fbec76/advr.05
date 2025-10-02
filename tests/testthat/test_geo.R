test_that("[tnm_geo] works with mocked response", {
  mock_response <- structure(list(), class = "httr2_response")

  with_mocked_bindings(
    tnm_request = function(dataset, module, date, query) {
      mock_response
    },
    tnm_perform = function(...) mock_response,
    tnm_parse_geo = function(resp, geo_type) {
      expect_equal(geo_type, "topojson")
      structure(
        data.frame(
          id = "1",
          name = "Country",
          area = 12345,
          stringsAsFactors = FALSE
        ),
        class = c("sf", "data.frame")
      )
    },
  {
    result <- tnm_geo(
      dataset = "world-2",
      date = "2020-01-01",
      geo_type = "topojson",
      geo_crs = "epsg:4326",
      geo_props = c("id", "name"),
      language = "en"
    )

    expect_s3_class(result, "sf")
    expect_true("name" %in% colnames(result))
  }
  )
})

test_that("[tnm_geo] works with geojson format", {
  mock_response <- structure(list(), class = "httr2_response")

  with_mocked_bindings(
    tnm_request = function(dataset, module, date, query) {
      mock_response
    },
    tnm_perform = function(...) mock_response,
    tnm_parse_geo = function(resp, geo_type) {
      structure(
        data.frame(
          id = "geo_id",
          name = "GeoJSON Country",
          stringsAsFactors = FALSE
        ),
        class = c("sf", "data.frame")
      )
    },
  {
    result <- tnm_geo("world-2", "2020", geo_type = "geojson")
    expect_equal(result$name, "GeoJSON Country")
  }
  )
})

test_that("[tnm_geo] works with topojson format", {
  mock_response <- structure(list(), class = "httr2_response")

  with_mocked_bindings(
    tnm_request = function(dataset, module, date, query) {
      mock_response
    },
    tnm_perform = function(...) mock_response,
    tnm_parse_geo = function(resp, geo_type) {
      structure(
        data.frame(
          id = "topo_id",
          name = "TopoJSON Country",
          stringsAsFactors = FALSE
        ),
        class = c("sf", "data.frame")
      )
    },
  {
    result <- tnm_geo("world-2", "2020", geo_type = "topojson")
    expect_equal(result$name, "TopoJSON Country")
  }
  )
})


test_that("[tnm_geo] handles different CRS formats", {
  mock_response <- structure(list(), class = "httr2_response")

  with_mocked_bindings(
    tnm_request = function(dataset, module, date, query) {
      mock_response
    },
    tnm_perform = function(...) mock_response,
    tnm_parse_geo = function(...) {
      structure(
        data.frame(id = "crs_id", name = "CRS Country", stringsAsFactors = FALSE),
        class = c("sf", "data.frame")
      )
    },
  {
    result <- tnm_geo("world-2", "2020", geo_crs = "epsg:3857")
    expect_s3_class(result, "sf")
  }
  )
})

# ! Actual API call test !
test_that("[tnm_geo] can connect to actual API (requires internet)", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch({
    tnm_geo("world-2", date = "2020", geo_props = c("id", "name"))
  }, error = function(e) {
    skip(paste("API not available:", e$message))
  })

  expect_s3_class(result, "sf")
  expect_true(nrow(result) > 0)
  expect_true("id" %in% colnames(result))
  expect_true("name" %in% colnames(result))
})
