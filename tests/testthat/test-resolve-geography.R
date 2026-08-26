resolution_surveillance <- function(labels, versions = "2.0", ids = NA_character_,
                                    repeats = 1L, vintage = as.Date(NA)) {
  labels <- rep(labels, each = repeats)
  versions <- rep(versions, length.out = length(labels))
  ids <- rep(ids, length.out = length(labels))
  data.frame(
    geo_id = ids,
    geo_name = labels,
    geo_level = rep("survstat_kreis", length(labels)),
    geo_vintage = rep(vintage, length(labels)),
    date = as.Date("2024-01-01") + seq_along(labels) - 1L,
    time_unit = rep("week", length(labels)),
    pathogen = rep("synthetic", length(labels)),
    cases = rep(1, length(labels)),
    source = rep("SurvStat@RKI", length(labels)),
    source_version = versions,
    stringsAsFactors = FALSE
  )
}

resolution_geography <- function(
    ids = c("01001", "01002"), names = c("Alpha", "Beta"),
    types = c("Land", "Stadt"), from = as.Date("2020-01-01"),
    to = as.Date(NA), vghid = paste0("V", seq_along(ids))) {
  data.frame(
    geo_id = ids, geo_name = names, geo_level = rep("district", length(ids)),
    valid_from = rep(from, length(ids)), valid_to = rep(to, length(ids)),
    source = rep("synthetic register", length(ids)), TYP = types,
    VGHID = vghid, stringsAsFactors = FALSE
  )
}

resolution_alias <- function(
    label = "SK Aliasstadt", target = "01002", version = "2.0",
    from = as.Date("2020-01-01"), to = as.Date(NA)) {
  data.frame(
    source = "SurvStat@RKI", source_version = version,
    source_label = label, source_type = "SK", target_geo_id = target,
    valid_from = from, valid_to = to, reason = "reviewed synthetic alias",
    review_status = "reviewed", target_vghid = NA_character_,
    stringsAsFactors = FALSE
  )
}

resolution_spatial_unit <- function(
    label = "SK Teilgebiet", id = "source-unit-A", version = "2.0",
    from = as.Date("2020-01-01"), to = as.Date(NA)) {
  data.frame(
    source = "SurvStat@RKI", source_version = version,
    source_label = label, source_type = "SK", source_geo_id = id,
    source_geo_name = "Reviewed source unit A",
    source_geo_level = "source_subdistrict", valid_from = from, valid_to = to,
    reason = "requires reviewed spatial relation", review_status = "reviewed",
    stringsAsFactors = FALSE
  )
}

run_resolution <- function(data, geography = resolution_geography(), ...) {
  resolve_geography(
    data, geography, as.Date("2024-12-31"), "SurvStat@RKI", "TYP", ...
  )
}

test_that("exact LK and SK identities use canonical register values", {
  data <- resolution_surveillance(c("LK Alpha", "SK Beta"))
  result <- run_resolution(data)

  expect_named(result, c("data", "resolution", "diagnostics"), ignore.order = FALSE)
  expect_identical(result$data$geo_id, c("01001", "01002"))
  expect_identical(result$data$geo_name, c("Alpha", "Beta"))
  expect_identical(result$data$geo_level, c("district", "district"))
  expect_identical(result$data$source_geo_id, c(NA_character_, NA_character_))
  expect_identical(result$data$source_geo_name, c("LK Alpha", "SK Beta"))
  expect_identical(result$data$source_geo_level,
                   c("survstat_kreis", "survstat_kreis"))
  expect_identical(result$resolution$status,
                   c("resolved_identity", "resolved_identity"))
  expect_identical(result$resolution$resolution_method,
                   c("exact_typed", "exact_typed"))
  expect_identical(result$resolution$target_vghid, c("V1", "V2"))
  expect_identical(result$diagnostics$exact_resolved_count, 2L)
  expect_identical(result$diagnostics$canonical_target_ids_used,
                   c("01001", "01002"))
  expect_identical(validate_surveillance(result$data), result$data)
})

test_that("type is required to distinguish same-name city and district", {
  geography <- resolution_geography(
    c("01001", "01002"), c("Gamma", "Gamma"), c("Land", "Stadt")
  )
  result <- run_resolution(
    resolution_surveillance(c("LK Gamma", "SK Gamma")), geography
  )
  expect_identical(result$data$geo_id, c("01001", "01002"))
})

test_that("Region and StädteRegion forms map only through Regio", {
  geography <- resolution_geography(
    c("03001", "05001"), c("Region Hannover", "StädteRegion Aachen"),
    c("Regio", "Regio")
  )
  result <- run_resolution(
    resolution_surveillance(c("Region Hannover", "StädteRegion Aachen")),
    geography
  )
  expect_identical(result$resolution$source_type, c("Region", "StädteRegion"))
  expect_identical(result$resolution$candidate_name,
                   c("Region Hannover", "StädteRegion Aachen"))
  expect_identical(result$data$geo_id, c("03001", "05001"))
})

test_that("reviewed aliases apply only after exact matching fails", {
  result <- run_resolution(
    resolution_surveillance("SK Aliasstadt"),
    aliases = resolution_alias()
  )
  expect_identical(result$data$geo_id, "01002")
  expect_identical(result$data$geo_name, "Beta")
  expect_identical(result$resolution$resolution_method, "reviewed_alias")
  expect_identical(result$resolution$directive_reason,
                   "reviewed synthetic alias")
  expect_identical(result$resolution$target_vghid, "V2")
  expect_identical(result$diagnostics$alias_resolved_count, 1L)

  exact_geography <- resolution_geography(
    c("01002", "01999"), c("Beta", "Aliasstadt"), c("Stadt", "Stadt")
  )
  exact <- run_resolution(
    resolution_surveillance("SK Aliasstadt"), exact_geography,
    aliases = resolution_alias(target = "01002")
  )
  expect_identical(exact$data$geo_id, "01999")
  expect_identical(exact$resolution$resolution_method, "exact_typed")
})

test_that("alias dates and NA source versions have exact applicability", {
  expired <- resolution_alias(to = as.Date("2023-12-31"))
  expect_error(
    run_resolution(resolution_surveillance("SK Aliasstadt"), aliases = expired),
    "unresolved"
  )
  alias_na <- resolution_alias(version = NA_character_)
  result <- run_resolution(
    resolution_surveillance("SK Aliasstadt", versions = NA_character_),
    aliases = alias_na
  )
  expect_identical(result$data$geo_id, "01002")
  expect_error(
    run_resolution(
      resolution_surveillance("SK Aliasstadt", versions = "2.0"),
      aliases = alias_na
    ),
    "unresolved"
  )
})

test_that("conflicting and invalid alias targets fail explicitly", {
  aliases <- rbind(resolution_alias(), resolution_alias())
  expect_error(
    run_resolution(resolution_surveillance("SK Aliasstadt"), aliases = aliases),
    "ambiguous"
  )
  expect_error(
    run_resolution(
      resolution_surveillance("SK Aliasstadt"),
      aliases = resolution_alias(target = "09999")
    ),
    "unresolved"
  )
  aliases <- resolution_alias()
  aliases$review_status <- "draft"
  expect_error(validate_geography_aliases(aliases), "review_status")
  aliases <- resolution_alias()
  aliases$source_version <- 2
  expect_error(validate_geography_aliases(aliases), "source_version.*character")
  aliases <- resolution_alias()
  aliases$target_geo_id <- 1002
  expect_error(validate_geography_aliases(aliases), "target_geo_id.*character")
})

test_that("spatial units retain reviewed opaque source identity", {
  data <- resolution_surveillance("SK Teilgebiet", ids = "raw-input-id")
  result <- run_resolution(data, spatial_units = resolution_spatial_unit())

  expect_identical(result$data$geo_id, "source-unit-A")
  expect_identical(result$data$geo_name, "Reviewed source unit A")
  expect_identical(result$data$geo_level, "source_subdistrict")
  expect_identical(result$data$source_geo_id, "raw-input-id")
  expect_identical(result$data$source_geo_name, "SK Teilgebiet")
  expect_identical(result$resolution$status, "requires_spatial_relation")
  expect_identical(result$resolution$resolution_method,
                   "reviewed_spatial_unit")
  expect_identical(result$resolution$recognized_geo_id, "source-unit-A")
  expect_true(is.na(result$resolution$target_geo_id))
  expect_identical(result$diagnostics$spatial_relation_required_count, 1L)
  expect_identical(result$diagnostics$canonical_target_ids_used, character())
})

test_that("spatial source IDs are validated and NA versions are exact", {
  spatial <- resolution_spatial_unit(id = "")
  expect_error(
    run_resolution(resolution_surveillance("SK Teilgebiet"), spatial_units = spatial),
    "source_geo_id.*empty"
  )
  duplicate <- rbind(
    resolution_spatial_unit("SK Teilgebiet", "opaque-A"),
    resolution_spatial_unit("SK Anderes Teilgebiet", "opaque-A")
  )
  expect_error(
    run_resolution(resolution_surveillance("SK Teilgebiet"),
                   spatial_units = duplicate),
    "source_geo_id.*unique"
  )
  spatial_na <- resolution_spatial_unit(version = NA_character_)
  result <- run_resolution(
    resolution_surveillance("SK Teilgebiet", versions = NA_character_),
    spatial_units = spatial_na
  )
  expect_identical(result$data$geo_id, "source-unit-A")
  expect_error(
    run_resolution(resolution_surveillance("SK Teilgebiet", versions = "2.0"),
                   spatial_units = spatial_na),
    "unresolved"
  )
})

test_that("unresolved and ambiguous units abort the complete call", {
  expect_error(
    run_resolution(resolution_surveillance("Unknown Alpha")),
    "Unknown Alpha.*unresolved"
  )
  expect_error(
    run_resolution(resolution_surveillance("SK Missing")),
    "SK Missing.*unresolved"
  )
  overlapping <- rbind(
    resolution_geography("01001", "Alpha", "Land"),
    resolution_geography("01002", "Alpha", "Land")
  )
  expect_error(
    run_resolution(resolution_surveillance("LK Alpha"), overlapping),
    "ambiguous"
  )
  duplicate_target <- rbind(
    resolution_geography("01002", "Beta", "Stadt"),
    resolution_geography("01002", "Beta historical overlap", "Stadt")
  )
  expect_error(
    run_resolution(
      resolution_surveillance("SK Aliasstadt"), duplicate_target,
      aliases = resolution_alias()
    ),
    "ambiguous"
  )
})

test_that("unknown forms require an explicit reviewed directive", {
  alias <- resolution_alias(label = "Special source form")
  alias$source_type <- "Special"
  aliased <- run_resolution(
    resolution_surveillance("Special source form"), aliases = alias
  )
  expect_identical(aliased$resolution$resolution_method, "reviewed_alias")
  expect_identical(aliased$resolution$source_type, "Special")

  spatial <- resolution_spatial_unit(label = "Special spatial form")
  spatial$source_type <- "Special"
  recognized <- run_resolution(
    resolution_surveillance("Special spatial form"), spatial_units = spatial
  )
  expect_identical(recognized$resolution$status, "requires_spatial_relation")
  expect_identical(recognized$resolution$source_type, "Special")
})

test_that("matching is strict and performs no normalization", {
  variants <- c("SK beta", "SK  Beta", "SK Béta", "SK Bet-a")
  for (label in variants) {
    expect_error(run_resolution(resolution_surveillance(label)), "unresolved")
  }
})

test_that("source provenance columns cannot be overwritten", {
  for (field in c("source_geo_id", "source_geo_name", "source_geo_level")) {
    data <- resolution_surveillance("LK Alpha")
    data[[field]] <- "existing"
    expect_error(run_resolution(data), "reserved provenance")
  }
})

test_that("source and arguments are validated", {
  data <- resolution_surveillance("LK Alpha")
  data$source <- "other"
  expect_error(run_resolution(data), "source.*must equal")
  data <- resolution_surveillance("LK Alpha")
  expect_error(
    resolve_geography(data, resolution_geography(), as.Date(NA),
                      "SurvStat@RKI", "TYP"),
    "reference_date"
  )
  expect_error(
    resolve_geography(data, resolution_geography(), "2024-12-31",
                      "SurvStat@RKI", "TYP"),
    "reference_date"
  )
  expect_error(
    resolve_geography(data, resolution_geography(), as.Date("2024-12-31"),
                      "SurvStat@RKI", "missing"),
    "canonical_type_col.*missing"
  )
  geography <- resolution_geography()
  geography$TYP <- 1
  expect_error(run_resolution(data, geography), "TYP.*character")
  geography <- resolution_geography()
  geography$geo_id <- as.numeric(geography$geo_id)
  expect_error(run_resolution(data, geography), "geo_id.*character")
})

test_that("geo_vintage remains byte-for-byte unchanged", {
  for (vintage in list(as.Date(NA), as.Date("2023-12-31"))) {
    data <- resolution_surveillance("LK Alpha", vintage = vintage)
    result <- run_resolution(data)
    expect_identical(result$data$geo_vintage, data$geo_vintage)
    expect_identical(result$resolution$resolution_reference_date,
                     as.Date("2024-12-31"))
    expect_true(result$diagnostics$output_geo_vintage_unchanged)
  }
})

test_that("repeated observations resolve once and retain row order", {
  data <- resolution_surveillance(c("LK Alpha", "SK Beta"), repeats = 3L)
  data$row_marker <- seq_len(nrow(data))
  result <- run_resolution(data)
  expect_equal(nrow(result$resolution), 2L)
  expect_identical(result$resolution$input_rows, c(3L, 3L))
  expect_identical(result$data$row_marker, seq_len(nrow(data)))
  expect_identical(result$data$geo_id,
                   rep(c("01001", "01002"), each = 3L))
})

test_that("mixed exact alias and spatial paths share one audit", {
  data <- resolution_surveillance(c("LK Alpha", "SK Aliasstadt", "SK Teilgebiet"))
  result <- run_resolution(
    data, aliases = resolution_alias(),
    spatial_units = resolution_spatial_unit()
  )
  expect_identical(
    result$resolution$resolution_method,
    c("exact_typed", "reviewed_alias", "reviewed_spatial_unit")
  )
  expect_identical(result$diagnostics$unique_source_units, 3L)
  expect_identical(result$diagnostics$exact_resolved_count, 1L)
  expect_identical(result$diagnostics$alias_resolved_count, 1L)
  expect_identical(result$diagnostics$spatial_relation_required_count, 1L)
})

test_that("validity endpoints are inclusive and VGHID consistency is checked", {
  geography <- resolution_geography(
    from = as.Date("2024-12-31"), to = as.Date("2024-12-31")
  )
  expect_identical(
    run_resolution(resolution_surveillance("LK Alpha"), geography)$data$geo_id,
    "01001"
  )
  alias <- resolution_alias(
    from = as.Date("2024-12-31"), to = as.Date("2024-12-31")
  )
  alias$target_vghid <- "wrong"
  expect_error(
    run_resolution(resolution_surveillance("SK Aliasstadt"), aliases = alias),
    "ambiguous"
  )
})

test_that("zero-row input returns typed structured output", {
  data <- resolution_surveillance("LK Alpha")[0, ]
  result <- run_resolution(data)
  expect_equal(nrow(result$data), 0L)
  expect_equal(nrow(result$resolution), 0L)
  expect_s3_class(result$resolution$resolution_reference_date, "Date")
  expect_identical(result$diagnostics$unique_source_units, 0L)
  expect_identical(result$diagnostics$resolution_methods_used, character())
  expect_identical(validate_surveillance(result$data), result$data)
})
