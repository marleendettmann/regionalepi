source_spatial_example <- function(
    version = "v1", from = "source:A", to = "01001", type = "aggregate",
    valid_from = "2024-12-31", valid_to = "2024-12-31") {
  data.frame(
    source = "synthetic", source_version = version,
    from_geo_id = from, from_geo_level = "source_unit", to_geo_id = to,
    relation_type = type, valid_from = as.Date(valid_from),
    valid_to = as.Date(valid_to), review_status = "reviewed",
    reason = "reviewed synthetic mapping", stringsAsFactors = FALSE
  )
}

test_that("source spatial relation contract accepts reviewed mappings", {
  aggregate <- source_spatial_example()
  identity <- source_spatial_example(
    from = "01001", to = "01001", type = "identity", version = NA_character_
  )
  expect_identical(validate_source_spatial_relations(aggregate), aggregate)
  expect_identical(validate_source_spatial_relations(identity), identity)
})

test_that("source spatial relation fields and states are strict", {
  x <- source_spatial_example()
  expect_error(validate_source_spatial_relations(x[-1]), "missing required")
  names(x)[2] <- names(x)[1]
  expect_error(validate_source_spatial_relations(x), "column names.*unique")

  x <- source_spatial_example()
  x$from_geo_id <- 1
  expect_error(validate_source_spatial_relations(x), "from_geo_id.*character")
  x <- source_spatial_example()
  x$source_version <- 1
  expect_error(validate_source_spatial_relations(x), "source_version.*character")
  x <- source_spatial_example()
  x$review_status <- "draft"
  expect_error(validate_source_spatial_relations(x), "review_status")
  x <- source_spatial_example()
  x$relation_type <- "historical_merge"
  expect_error(validate_source_spatial_relations(x), "invalid.*relation_type")
})

test_that("relation semantics and applicability intervals are validated", {
  expect_error(
    validate_source_spatial_relations(source_spatial_example(
      from = "01001", to = "01002", type = "identity"
    )),
    "identity.*itself"
  )
  expect_error(
    validate_source_spatial_relations(source_spatial_example(
      from = "01001", to = "01001", type = "aggregate"
    )),
    "aggregate.*different"
  )
  reversed <- source_spatial_example(valid_from = "2025-01-01",
                                     valid_to = "2024-12-31")
  expect_error(validate_source_spatial_relations(reversed), "must not precede")

  overlapping <- rbind(
    source_spatial_example(valid_from = "2024-01-01", valid_to = "2024-12-31"),
    source_spatial_example(valid_from = "2024-12-31", valid_to = "2025-12-31")
  )
  expect_error(validate_source_spatial_relations(overlapping), "must not overlap")
  separate <- rbind(
    source_spatial_example(valid_from = "2024-01-01", valid_to = "2024-12-30"),
    source_spatial_example(valid_from = "2024-12-31", valid_to = "2025-12-31")
  )
  expect_identical(validate_source_spatial_relations(separate), separate)
})

test_that("concrete and missing source versions are distinct keys", {
  distinct <- rbind(
    source_spatial_example(version = "v1"),
    source_spatial_example(version = NA_character_)
  )
  expect_identical(validate_source_spatial_relations(distinct), distinct)
})
