test_that("required tabular fields are complete unless explicitly excepted", {
  surveillance <- surveillance_example()
  surveillance$geo_name <- NA_character_
  expect_error(validate_surveillance(surveillance), "must not contain NA")

  population <- population_example()
  population$reference_date <- as.Date(NA)
  expect_error(validate_context_population(population), "must not contain NA")

  context <- context_example()
  context$variable <- NA_character_
  expect_error(validate_context_base(context), "must not contain NA")

  geography <- geography_example()
  geography$valid_from <- as.Date(NA)
  expect_error(validate_geography(geography), "must not contain NA")

  relations <- relations_example()
  relations$source <- NA_character_
  expect_error(validate_geography_relations(relations), "must not contain NA")
})

test_that("documented NA exceptions remain accepted", {
  surveillance <- surveillance_example()
  surveillance$geo_id <- NA_character_
  surveillance$geo_vintage <- as.Date(NA)
  surveillance$sex <- NA_character_
  surveillance$retrieved_at <- as.POSIXct(NA)
  expect_identical(validate_surveillance(surveillance), surveillance)

  population <- population_example()
  population$age_to <- NA_real_
  population$sex <- NA_character_
  expect_identical(validate_context_population(population), population)

  geography <- geography_example()
  geography$parent_geo_id <- NA_character_
  expect_identical(validate_geography(geography), geography)

  relations <- relations_example()
  expect_identical(validate_geography_relations(relations), relations)
})

test_that("optional fields are type checked when present", {
  population <- population_example()
  population$sex <- 1
  expect_error(validate_context_population(population), "sex.*character")
  population <- population_example()
  population$retrieved_at <- as.Date("2020-01-01")
  expect_error(validate_context_population(population), "retrieved_at.*POSIXct")

  geography <- geography_example()
  geography$parent_geo_id <- 1001
  expect_error(validate_geography(geography), "parent_geo_id.*character")
})

test_that("required columns are enforced across remaining tabular contracts", {
  expect_error(
    validate_context_base(context_example()[setdiff(names(context_example()), "unit")]),
    "missing required"
  )
  expect_error(
    validate_geography(geography_example()[setdiff(names(geography_example()), "source")]),
    "missing required"
  )
  expect_error(
    validate_geography_relations(
      relations_example()[setdiff(names(relations_example()), "relation_type")]
    ),
    "missing required"
  )
})

test_that("zero-row typed canonical tables remain valid", {
  surveillance <- surveillance_example()[0, ]
  population <- population_example()[0, ]
  context <- context_example()[0, ]
  geography <- geography_example()[0, ]
  relations <- relations_example()[0, ]
  expect_identical(validate_surveillance(surveillance), surveillance)
  expect_identical(validate_context_population(population), population)
  expect_identical(validate_context_base(context), context)
  expect_identical(validate_geography(geography), geography)
  expect_identical(validate_geography_relations(relations), relations)
})
