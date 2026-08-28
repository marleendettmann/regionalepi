test_that("surveillance incidence has distinct non-additive validation", {
  x <- surveillance_incidence_example()
  expect_identical(validate_surveillance_incidence(x), x)
  x$incidence <- NA_real_
  expect_no_error(validate_surveillance_incidence(x))
  x$incidence <- 0
  expect_no_error(validate_surveillance_incidence(x))
  x$incidence <- -0.1
  expect_error(validate_surveillance_incidence(x), "non-negative")
  x$incidence <- Inf
  expect_error(validate_surveillance_incidence(x), "finite")
  x <- surveillance_incidence_example()
  x$query_id <- NA_character_
  expect_error(validate_surveillance_incidence(x), "query_id.*NA")
  expect_error(validate_surveillance_incidence(x[setdiff(names(x), "incidence")]),
               "missing required")
})

test_that("additive geography operations reject canonical incidence", {
  data <- data.frame(
    geo_id = "a", geo_name = "A", geo_level = "district",
    geo_vintage = as.Date("2020-12-31"), incidence = 1.5,
    stringsAsFactors = FALSE
  )
  geography <- data.frame(
    geo_id = "a", geo_name = "A", geo_level = "district",
    valid_from = as.Date("2019-01-01"), valid_to = as.Date(NA),
    source = "synthetic", stringsAsFactors = FALSE
  )
  relations <- data.frame(
    from_geo_id = "a", from_vintage = as.Date("2020-12-31"),
    to_geo_id = "a", to_vintage = as.Date("2021-12-31"),
    relation_type = "identity", weight = NA_real_, source = "synthetic",
    note = NA_character_, stringsAsFactors = FALSE
  )
  expect_error(
    harmonize_vintage(
      data, relations, geography, as.Date("2020-12-31"),
      as.Date("2021-12-31"), "incidence", character()
    ),
    "non-additive"
  )
  aggregate_data <- transform(data, source = "synthetic", source_version = "v1")
  spatial <- data.frame(
    source = "synthetic", source_version = "v1", from_geo_id = "a",
    from_geo_level = "district", to_geo_id = "a", relation_type = "identity",
    valid_from = as.Date("2020-01-01"), valid_to = as.Date(NA),
    review_status = "reviewed", reason = "synthetic", stringsAsFactors = FALSE
  )
  expect_error(
    aggregate_geography(
      aggregate_data, spatial, geography, as.Date("2020-12-31"),
      "incidence", character()
    ),
    "non-additive"
  )
})
