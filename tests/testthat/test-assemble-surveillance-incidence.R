synthetic_incidence_spec <- function(excluded = paste0("x", 1:12)) {
  data.frame(
    spec_id = rep("synthetic-replacement", length(excluded)),
    spec_version = rep("v1", length(excluded)),
    measure = rep("incidence", length(excluded)),
    base_query_role = rep("kreis_incidence", length(excluded)),
    replacement_query_role = rep("bundesland_incidence", length(excluded)),
    exclude_geo_id = excluded,
    replacement_target_geo_id = rep("target", length(excluded)),
    temporal_keys = rep("reporting_year;reporting_week", length(excluded)),
    expected_base_units = rep(14, length(excluded)),
    expected_excluded_units = rep(12, length(excluded)),
    expected_replacement_units = rep(1, length(excluded)),
    expected_output_units = rep(3, length(excluded)),
    review_status = rep("reviewed", length(excluded)),
    reason = rep("synthetic reviewed replacement", length(excluded)),
    stringsAsFactors = FALSE
  )
}

synthetic_incidence_query <- function(id, role, dimension) {
  list(
    query_id = id, query_role = role, source = "SurvStat@RKI",
    source_version = "SurvStat@RKI 2.0", measure = "incidence",
    value_semantics = "non_additive", pathogen = "Disease",
    reporting_years = 2024L, time_unit = "week",
    source_geography_dimension = dimension,
    source_geography_filter = NULL,
    row_dimension = if (dimension == "survstat_kreis") "Kreis" else "Meldejahr",
    column_dimension = "Meldewoche", reference_definition = TRUE,
    reporting_path = "reviewed", epidemiological_filters = list(sex = "all"),
    empty_rows_and_columns = TRUE, totals = TRUE,
    retrieved_at = as.POSIXct("2026-08-28 10:00:00", tz = "UTC"),
    data_status = as.POSIXct("2026-08-28 08:00:00", tz = "UTC"),
    geo_vintage = as.Date(NA),
    reporting_week_coverage = list(`2024` = c(1L, 2L))
  )
}

synthetic_incidence_data <- function(ids, query_id, values) {
  observations <- length(ids) * 2L
  data.frame(
    geo_id = rep(ids, each = 2L), geo_name = rep(ids, each = 2L),
    geo_level = rep("district", observations),
    geo_vintage = rep(as.Date(NA), observations),
    date = rep(as.Date(c("2024-01-01", "2024-01-08")), length(ids)),
    time_unit = rep("week", observations), pathogen = rep("Disease", observations),
    incidence = rep(values, length.out = observations),
    source = rep("SurvStat@RKI", observations),
    retrieved_at = rep(as.POSIXct("2026-08-28 10:00:00", tz = "UTC"), observations),
    source_version = rep("SurvStat@RKI 2.0", observations),
    reporting_year = rep(2024L, observations),
    reporting_week = rep(1:2, length(ids)),
    data_status = rep(as.POSIXct("2026-08-28 08:00:00", tz = "UTC"), observations),
    reference_definition = rep(TRUE, observations),
    reporting_path = rep("reviewed", observations),
    query_id = rep(query_id, observations), stringsAsFactors = FALSE
  )
}

incidence_query_pair <- function() {
  list(
    base_data = synthetic_incidence_data(
      c(paste0("x", 1:12), "retained-1", "retained-2"),
      "base-query", c(1, 2)
    ),
    replacement_data = synthetic_incidence_data("target", "replacement-query", c(9.5, 8.5)),
    base_provenance = synthetic_incidence_query(
      "base-query", "kreis_incidence", "survstat_kreis"
    ),
    replacement_provenance = synthetic_incidence_query(
      "replacement-query", "bundesland_incidence", "survstat_bundesland"
    )
  )
}

test_that("reviewed replacement removes rates and appends rates without arithmetic", {
  pair <- incidence_query_pair()
  result <- assemble_surveillance_incidence(
    pair$base_data, pair$replacement_data,
    pair$base_provenance, pair$replacement_provenance,
    synthetic_incidence_spec()
  )
  expect_identical(nrow(result$data), 6L)
  for (week in 1:2) {
    rows <- result$data$reporting_week == week
    expect_setequal(result$data$geo_id[rows],
                    c("retained-1", "retained-2", "target"))
  }
  target <- result$data[result$data$geo_id == "target", ]
  expect_identical(target$incidence, c(9.5, 8.5))
  expect_identical(target$query_id, rep("replacement-query", 2L))
  expect_true(all(result$data$query_id[result$data$geo_id != "target"] ==
                    "base-query"))
  expect_false(result$diagnostics$rate_summation_performed)
  expect_false(result$diagnostics$rate_averaging_performed)
  expect_named(result$provenance, c("base-query", "replacement-query"))
})

test_that("assembly fails on incompatible query metadata and strict data status", {
  pair <- incidence_query_pair()
  changed <- pair$replacement_provenance
  changed$pathogen <- "Other"
  expect_error(
    assemble_surveillance_incidence(
      pair$base_data, pair$replacement_data, pair$base_provenance, changed,
      synthetic_incidence_spec()
    ),
    "pathogen"
  )
  changed <- pair$replacement_provenance
  changed$data_status <- changed$data_status + 1
  expect_error(
    assemble_surveillance_incidence(
      pair$base_data, pair$replacement_data, pair$base_provenance, changed,
      synthetic_incidence_spec()
    ),
    "data_status"
  )
})

test_that("assembly fails on missing, duplicate, and unexpected coverage", {
  pair <- incidence_query_pair()
  call <- function(base = pair$base_data, replacement = pair$replacement_data) {
    assemble_surveillance_incidence(
      base, replacement, pair$base_provenance, pair$replacement_provenance,
      synthetic_incidence_spec()
    )
  }
  expect_error(call(pair$base_data[pair$base_data$geo_id != "x12", ]),
               "base source-unit coverage|exclusion coverage")
  duplicate <- rbind(pair$replacement_data, pair$replacement_data[1, ])
  expect_error(call(replacement = duplicate), "one row per geography|replacement coverage")
  changed <- pair$replacement_data
  changed$geo_id <- "wrong"
  expect_error(call(replacement = changed), "replacement coverage")
})
