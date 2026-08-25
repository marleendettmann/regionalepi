test_that("geography preserves character identifiers and open validity", {
  x <- geography_example()
  expect_identical(validate_geography(x), x)
  x$geo_id <- 1001
  expect_error(validate_geography(x), "geo_id.*character")
  x$geo_id <- NA_character_
  expect_error(validate_geography(x), "must not contain NA")
})

test_that("geography validity intervals are ordered", {
  x <- geography_example()
  x$valid_to <- as.Date("2019-12-31")
  expect_error(validate_geography(x), "must not precede")
})

test_that("geography relations distinguish allowed relation types", {
  x <- relations_example()
  expect_identical(validate_geography_relations(x), x)
  for (kind in c("identity", "aggregate", "historical_merge",
                 "historical_split", "boundary_change")) {
    x$relation_type <- kind
    expect_no_error(validate_geography_relations(x))
  }
  x$relation_type <- "merge"
  expect_error(validate_geography_relations(x), "invalid.*relation_type")
  x$relation_type <- "unresolved"
  expect_error(validate_geography_relations(x), "invalid.*relation_type")
})

test_that("relation geographic identifiers must remain character", {
  x <- relations_example()
  x$from_geo_id <- 1001
  expect_error(validate_geography_relations(x), "from_geo_id.*character")
  x <- relations_example()
  x$to_geo_id <- 1001
  expect_error(validate_geography_relations(x), "to_geo_id.*character")
})

test_that("relation weights are optional allocation proportions", {
  x <- relations_example()
  expect_no_error(validate_geography_relations(x))
  x$weight <- 0
  expect_no_error(validate_geography_relations(x))
  x$weight <- 1
  expect_no_error(validate_geography_relations(x))
  x$weight <- -0.01
  expect_error(validate_geography_relations(x), "allocation proportion")
  x$weight <- 1.01
  expect_error(validate_geography_relations(x), "allocation proportion")
  x$weight <- Inf
  expect_error(validate_geography_relations(x), "finite")
})
