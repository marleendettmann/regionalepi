test_that("reviewed 2024 resource bundle has stable structure and provenance", {
  resource <- regionalepi_geography_resources_2024
  expect_named(resource, c("bkg_districts", "survstat_aliases",
                           "survstat_spatial_units",
                           "survstat_source_spatial_relations", "provenance"),
               ignore.order = FALSE)
  expect_identical(resource$provenance$resource_version, "2024.12.31-v1")
  expect_identical(resource$provenance$reference_date, as.Date("2024-12-31"))
  expect_identical(resource$provenance$product_version, "2026-01")
  expect_identical(resource$provenance$source_license,
                   "Creative Commons Namensnennung 4.0 International (CC BY 4.0)")
  expect_true(resource$provenance$derived_resource)
  expect_match(resource$provenance$valid_to_transformation,
               "does not explicitly define 9999-12-31")
})

test_that("reviewed Berlin source spatial relations have exact integrity", {
  resource <- regionalepi_geography_resources_2024
  relations <- resource$survstat_source_spatial_relations
  spatial <- resource$survstat_spatial_units
  expect_identical(validate_source_spatial_relations(relations), relations)
  expect_identical(nrow(relations), 12L)
  expect_setequal(relations$from_geo_id, spatial$source_geo_id)
  expect_identical(anyDuplicated(relations$from_geo_id), 0L)
  expect_true(all(relations$source == "SurvStat@RKI"))
  expect_true(all(relations$source_version == "SurvStat@RKI 2.0"))
  expect_true(all(relations$from_geo_level == "survstat_berlin_bezirk"))
  expect_true(all(relations$to_geo_id == "11000"))
  expect_true(all(relations$relation_type == "aggregate"))
  expect_true(all(relations$valid_from == as.Date("2024-12-31")))
  expect_true(all(relations$valid_to == as.Date("2024-12-31")))
  expect_true(all(relations$review_status == "reviewed"))
  expect_false(any(c("from_vintage", "to_vintage", "weight") %in%
                     names(relations)))
})

test_that("BKG district register satisfies canonical and reviewed invariants", {
  geography <- regionalepi_geography_resources_2024$bkg_districts
  expect_identical(validate_geography(geography), geography)
  expect_identical(nrow(geography), 400L)
  expect_identical(length(unique(geography$geo_id)), 400L)
  expect_type(geography$geo_id, "character")
  expect_true(all(nchar(geography$geo_id) == 5L))
  expect_false(anyNA(geography$vghid))
  expect_false(anyNA(geography$canonical_type))
  expect_false(inherits(geography, "sf"))
  expect_false(any(names(geography) %in% c("geometry", "geom")))
  berlin <- geography[geography$geo_id == "11000", , drop = FALSE]
  expect_identical(nrow(berlin), 1L)
  expect_identical(berlin$geo_name, "Berlin")
  expect_identical(berlin$canonical_type, "Stadt")
})

test_that("reviewed aliases have exact dated canonical targets", {
  resource <- regionalepi_geography_resources_2024
  aliases <- resource$survstat_aliases
  geography <- resource$bkg_districts
  expect_identical(validate_geography_aliases(aliases), aliases)
  expect_identical(nrow(aliases), 19L)
  expect_identical(anyDuplicated(paste(
    aliases$source, aliases$source_version, aliases$source_label,
    aliases$source_type
  )), 0L)
  expect_true(all(aliases$valid_from == as.Date("2024-12-31")))
  expect_true(all(aliases$valid_to == as.Date("2024-12-31")))
  target <- match(aliases$target_geo_id, geography$geo_id)
  expect_false(anyNA(target))
  expect_identical(aliases$target_vghid, geography$vghid[target])
})

test_that("reviewed spatial units are opaque stable source identities", {
  resource <- regionalepi_geography_resources_2024
  spatial <- resource$survstat_spatial_units
  expect_identical(nrow(spatial), 12L)
  expect_identical(anyDuplicated(spatial$source_geo_id), 0L)
  expect_true(all(startsWith(spatial$source_geo_id,
                             "survstat:rki:berlin-bezirk:")))
  expect_true(all(spatial$source_geo_level == "survstat_berlin_bezirk"))
  expect_true(all(spatial$valid_from == as.Date("2024-12-31")))
  expect_true(all(spatial$valid_to == as.Date("2024-12-31")))
  expect_false(any(spatial$source_geo_id %in% resource$bkg_districts$geo_id))
})

test_that("all reviewed directives resolve without aggregate targets", {
  resource <- regionalepi_geography_resources_2024
  labels <- c(resource$survstat_aliases$source_label,
              resource$survstat_spatial_units$source_label)
  data <- data.frame(
    geo_id = rep(NA_character_, length(labels)), geo_name = labels,
    geo_level = rep("survstat_kreis", length(labels)),
    geo_vintage = rep(as.Date(NA), length(labels)),
    date = rep(as.Date("2024-01-01"), length(labels)),
    time_unit = rep("week", length(labels)),
    pathogen = rep("synthetic", length(labels)), cases = rep(1, length(labels)),
    source = rep("SurvStat@RKI", length(labels)),
    source_version = rep("SurvStat@RKI 2.0", length(labels)),
    stringsAsFactors = FALSE
  )
  result <- resolve_geography(
    data, resource$bkg_districts, as.Date("2024-12-31"), "SurvStat@RKI",
    "canonical_type", aliases = resource$survstat_aliases,
    spatial_units = resource$survstat_spatial_units
  )
  expect_identical(result$diagnostics$alias_resolved_count, 19L)
  expect_identical(result$diagnostics$spatial_relation_required_count, 12L)
  expect_identical(result$diagnostics$unresolved_count, 0L)
  expect_identical(result$diagnostics$ambiguous_count, 0L)
  spatial_rows <- result$resolution$status == "requires_spatial_relation"
  expect_true(all(is.na(result$resolution$target_geo_id[spatial_rows])))
  expect_true(result$diagnostics$output_geo_vintage_unchanged)
  expect_true(all(is.na(result$data$geo_vintage)))
})
