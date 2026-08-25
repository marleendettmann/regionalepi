test_that("valid surveillance data passes and unresolved character IDs are allowed", {
  x <- surveillance_example()
  expect_identical(validate_surveillance(x), x)
  x$geo_id <- NA_character_
  expect_no_error(validate_surveillance(x))
})

test_that("surveillance requires columns and correct types", {
  x <- surveillance_example()
  expect_error(validate_surveillance(x[setdiff(names(x), "cases")]), "missing required")
  x$geo_id <- 1001
  expect_error(validate_surveillance(x), "geo_id.*character")
  x <- surveillance_example()
  x$date <- "2020-01-01"
  expect_error(validate_surveillance(x), "date.*Date")
  x <- surveillance_example()
  x$source <- NA_character_
  expect_error(validate_surveillance(x), "source.*must not contain NA")
})

test_that("surveillance counts are non-negative whole values", {
  x <- surveillance_example()
  x$cases <- -1
  expect_error(validate_surveillance(x), "non-negative")
  x$cases <- 1.5
  expect_error(validate_surveillance(x), "whole-valued")
  x$cases <- NA_real_
  expect_error(validate_surveillance(x), "must not contain NA")
  x$cases <- Inf
  expect_error(validate_surveillance(x), "finite")
})

test_that("surveillance optional fields have canonical types", {
  x <- surveillance_example()
  x$retrieved_at <- as.Date("2020-01-01")
  expect_error(validate_surveillance(x), "retrieved_at.*POSIXct")
  x <- surveillance_example()
  x$source_version <- 1
  expect_error(validate_surveillance(x), "source_version.*character")
})
