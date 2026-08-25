test_that("population base data accepts non-whole numeric population", {
  x <- population_example()
  expect_identical(validate_context_population(x), x)
  x$age_to <- NA_integer_
  expect_no_error(validate_context_population(x))
})

test_that("population validation rejects missing columns, types, and values", {
  x <- population_example()
  expect_error(validate_context_population(x[setdiff(names(x), "population")]), "missing required")
  x$geo_id <- 1001
  expect_error(validate_context_population(x), "geo_id.*character")
  x <- population_example()
  x$population <- -0.1
  expect_error(validate_context_population(x), "non-negative")
  x$population <- Inf
  expect_error(validate_context_population(x), "finite")
})

test_that("population age intervals are valid whole-valued numbers", {
  x <- population_example()
  x$age_from <- 20L
  x$age_to <- 19L
  expect_error(validate_context_population(x), "age_to")
  x <- population_example()
  x$age_from <- 0
  expect_no_error(validate_context_population(x))
  x$age_from <- 0.5
  expect_error(validate_context_population(x), "whole-valued")
  x$age_from <- -1
  expect_error(validate_context_population(x), "non-negative")
})

test_that("generic context data is validated", {
  x <- context_example()
  expect_identical(validate_context_base(x), x)
  expect_error(validate_context_base(x[setdiff(names(x), "unit")]), "missing required")
  x$geo_id <- 1001
  expect_error(validate_context_base(x), "geo_id.*character")
  x <- context_example()
  x$value <- Inf
  expect_error(validate_context_base(x), "finite")
  x$value <- NA_real_
  expect_error(validate_context_base(x), "must not contain NA")
})
