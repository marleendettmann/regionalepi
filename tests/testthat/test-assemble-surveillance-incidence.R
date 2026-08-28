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
  expect_identical(result$diagnostics$assembly_reporting_years, 2024L)
  expect_identical(
    result$diagnostics$assembly_reporting_week_coverage,
    list(`2024` = c(1L, 2L))
  )
  expect_named(result$provenance, c("base-query", "replacement-query"))
})

test_that("assembly compatibility uses supplied observations rather than query scope", {
  pair <- incidence_query_pair()
  pair$base_provenance$reporting_years <- 2024L
  pair$base_provenance$reporting_week_coverage <- list(`2024` = c(1L, 2L))
  pair$replacement_provenance$reporting_years <- 2017:2026
  pair$replacement_provenance$reporting_week_coverage <- stats::setNames(
    rep(list(1:52), 10L), as.character(2017:2026)
  )
  original_base <- pair$base_provenance
  original_replacement <- pair$replacement_provenance

  result <- assemble_surveillance_incidence(
    pair$base_data, pair$replacement_data,
    pair$base_provenance, pair$replacement_provenance,
    synthetic_incidence_spec()
  )

  expect_identical(result$provenance$`base-query`, original_base)
  expect_identical(result$provenance$`replacement-query`, original_replacement)
  expect_identical(result$diagnostics$base_query_reporting_years, 2024L)
  expect_identical(result$diagnostics$replacement_query_reporting_years, 2017:2026)
  expect_identical(result$diagnostics$assembly_reporting_years, 2024L)
  expect_true(all(result$data$query_id[result$data$geo_id == "target"] ==
                    "replacement-query"))
  expect_true(all(result$data$query_id[result$data$geo_id != "target"] ==
                    "base-query"))
})

test_that("multi-year observation subsets may assemble across different query scopes", {
  pair <- incidence_query_pair()
  make_two_years <- function(x) {
    later <- x
    later$reporting_year <- 2025L
    later$date <- later$date + 364
    rbind(x, later)
  }
  pair$base_data <- make_two_years(pair$base_data)
  pair$replacement_data <- make_two_years(pair$replacement_data)
  pair$base_provenance$reporting_years <- c(2024L, 2025L)
  pair$base_provenance$reporting_week_coverage <- list(
    `2024` = 1:2, `2025` = 1:2
  )
  pair$replacement_provenance$reporting_years <- 2017:2026
  pair$replacement_provenance$reporting_week_coverage <- stats::setNames(
    rep(list(1:52), 10L), as.character(2017:2026)
  )

  result <- assemble_surveillance_incidence(
    pair$base_data, pair$replacement_data,
    pair$base_provenance, pair$replacement_provenance,
    synthetic_incidence_spec()
  )
  expect_identical(result$diagnostics$assembly_reporting_years, c(2024L, 2025L))
  expect_identical(result$diagnostics$temporal_groups, 4L)
  expect_identical(nrow(result$data), 12L)
})

test_that("observed temporal coverage rejects missing and unexpected periods", {
  pair <- incidence_query_pair()
  call <- function(replacement) {
    assemble_surveillance_incidence(
      pair$base_data, replacement,
      pair$base_provenance, pair$replacement_provenance,
      synthetic_incidence_spec()
    )
  }
  expect_error(call(pair$replacement_data[-1L, ]), "temporal coverage")

  unexpected_week <- pair$replacement_data[1L, ]
  unexpected_week$reporting_week <- 3L
  unexpected_week$date <- as.Date("2024-01-15")
  expect_error(call(rbind(pair$replacement_data, unexpected_week)), "temporal coverage")

  unexpected_year <- pair$replacement_data[1L, ]
  unexpected_year$reporting_year <- 2025L
  unexpected_year$date <- as.Date("2024-12-30")
  expect_error(call(rbind(pair$replacement_data, unexpected_year)), "temporal coverage")

  wrong_date <- pair$replacement_data
  wrong_date$date[1L] <- wrong_date$date[1L] + 1
  expect_error(call(wrong_date), "temporal coverage")
})

test_that("assembly fails on incompatible query metadata and strict data status", {
  pair <- incidence_query_pair()
  changes <- list(
    pathogen = list(value = "Other", pattern = "pathogen"),
    reference_definition = list(value = FALSE, pattern = "reference_definition"),
    reporting_path = list(value = "other", pattern = "reporting_path"),
    epidemiological_filters = list(
      value = list(sex = "female"), pattern = "epidemiological_filters"
    ),
    source_version = list(value = "other", pattern = "source_version"),
    measure = list(value = "cases", pattern = "non-additive incidence"),
    time_unit = list(value = "day", pattern = "time_unit"),
    data_status = list(
      value = pair$replacement_provenance$data_status + 1,
      pattern = "data_status"
    )
  )
  for (field in names(changes)) {
    changed <- pair$replacement_provenance
    changed[[field]] <- changes[[field]]$value
    expect_error(
      assemble_surveillance_incidence(
        pair$base_data, pair$replacement_data, pair$base_provenance, changed,
        synthetic_incidence_spec()
      ),
      changes[[field]]$pattern,
      fixed = TRUE
    )
  }
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
