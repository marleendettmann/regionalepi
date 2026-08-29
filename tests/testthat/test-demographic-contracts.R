test_that("demographic indicator accepts unresolved vintage without unit count rules", {
  x <- demographic_example(ids = c("01001", "02000"), years = 2020)
  expect_identical(validate_demographic_indicator(x), x)
  expect_true(all(is.na(x$geo_vintage)))
  expect_identical(nrow(x), 2L)
})

test_that("demographic indicator rejects malformed values and duplicate keys", {
  x <- demographic_example(years = 2020)
  x$geo_id <- as.numeric(x$geo_id)
  expect_error(validate_demographic_indicator(x), "geo_id.*character")
  x <- demographic_example(years = 2020)
  x$indicator_value[[1L]] <- NA_real_
  expect_error(validate_demographic_indicator(x), "must not contain NA")
  x <- demographic_example(years = 2020)
  x$value_origin[[1L]] <- "calculated-ish"
  expect_error(validate_demographic_indicator(x), "source_provided or derived")
  x <- demographic_example(years = 2020)
  expect_error(validate_demographic_indicator(rbind(x, x[1L, ])), "unique keys")
})

test_that("regional area is positive numeric and not tied to 401 units", {
  x <- regional_area_example()
  expect_identical(validate_regional_area(x), x)
  x$area_km2[[1L]] <- 0
  expect_error(validate_regional_area(x), "strictly positive")
  x <- regional_area_example(); x$area_km2[[1L]] <- -1
  expect_error(validate_regional_area(x), "strictly positive")
})

test_that("context population now permits only the approved vintage exception", {
  x <- population_example()
  x$geo_vintage <- as.Date(NA)
  expect_no_error(validate_context_population(x))
  x$geo_id <- NA_character_
  expect_error(validate_context_population(x), "geo_id.*must not contain NA")
})
