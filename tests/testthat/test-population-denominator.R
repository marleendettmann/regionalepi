population_denominator_example <- function() {
  data.frame(
    geo_id = c("01001", "16063"),
    geo_name = c("Flensburg", "Wartburgkreis"),
    geo_level = c("district", "district"),
    geo_vintage = as.Date(c(NA, NA)),
    reference_date = as.Date(c("2024-12-31", "2024-12-31")),
    population = c(95000, 154957),
    population_basis = c("census_2022", "census_2022"),
    source = rep("synthetic", 2L),
    source_table = rep("table", 2L),
    retrieved_at = rep(as.POSIXct("2025-01-02", tz = "UTC"), 2L),
    data_status = rep("final", 2L),
    stringsAsFactors = FALSE
  )
}

test_that("population_denominator validates its approved contract", {
  x <- population_denominator_example()
  expect_identical(validate_population_denominator(x), x)
  expect_true(all(is.na(x$geo_vintage)))
  expect_identical(x$geo_id[1L], "01001")
})

test_that("population_denominator requires all fields and character IDs", {
  x <- population_denominator_example()
  expect_error(validate_population_denominator(x[-1L]), "missing required.*geo_id")
  x$geo_id <- c(1001, 16063)
  expect_error(validate_population_denominator(x), "geo_id.*character")
})

test_that("population_denominator enforces completeness with only vintage exempt", {
  fields <- c(
    "geo_id", "geo_name", "geo_level", "reference_date", "population",
    "population_basis", "source", "source_table", "retrieved_at", "data_status"
  )
  for (field in fields) {
    x <- population_denominator_example()
    x[[field]][1L] <- NA
    expect_error(validate_population_denominator(x), field, fixed = TRUE)
  }
  x <- population_denominator_example()
  x$geo_vintage[1L] <- as.Date(NA)
  expect_no_error(validate_population_denominator(x))
})

test_that("population values are finite and non-negative without integer restriction", {
  for (value in c(-1, NA_real_, NaN, Inf, -Inf)) {
    x <- population_denominator_example()
    x$population[1L] <- value
    expect_error(validate_population_denominator(x), "population")
  }
  x <- population_denominator_example()
  x$population[1L] <- 95000.5
  expect_no_error(validate_population_denominator(x))
})

test_that("population denominator keys and retrieval events are unambiguous", {
  x <- population_denominator_example()
  x[2L, c("geo_id", "reference_date")] <- x[1L, c("geo_id", "reference_date")]
  expect_error(validate_population_denominator(x), "combination must be unique")

  x <- population_denominator_example()
  x$retrieved_at[2L] <- as.POSIXct("2025-01-03", tz = "UTC")
  expect_error(validate_population_denominator(x), "one retrieval event")
  x$retrieved_at <- as.Date(x$retrieved_at)
  expect_error(validate_population_denominator(x), "retrieved_at.*POSIXct")
})
