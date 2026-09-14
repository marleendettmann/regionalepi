analysis_incidence_bundle <- function(years = 2022L) {
  dates <- as.Date(sprintf("%d-01-03", years))
  data <- expand.grid(
    geo_id = c("01001", "07315"), date = dates,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  data$geo_name <- c("Flensburg", "Mainz")
  data$geo_level <- "district"
  data$geo_vintage <- as.Date(NA)
  data$time_unit <- "week"
  data$pathogen <- "Disease"
  data$incidence <- c(NA_real_, 20)
  data$source <- "SurvStat@RKI"
  data$source_version <- "SurvStat@RKI 2.0"
  data$reporting_year <- as.integer(format(data$date, "%Y"))
  data$reporting_week <- as.integer(format(data$date, "%V"))
  data$query_id <- "incidence-query"
  data$cases <- c(0, 10)
  data$count_query_id <- "count-query"
  query <- list(data_status = "synthetic")
  list(
    data = data, diagnostics = list(exact_key_equality = TRUE),
    provenance = list(incidence = list(incidence = query),
                      counts = list(counts = query))
  )
}

analysis_population <- function(years = 2022L) {
  data <- expand.grid(
    geo_id = c("01001", "07315"), year = years,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  data$geo_name <- c("Flensburg", "Mainz")
  data$geo_level <- "district"
  data$geo_vintage <- as.Date(NA)
  data$population <- c(100, 1000)
  data$population_measure <- "annual_average_population"
  data$population_reference <- "reporting_year_annual_average"
  data$population_basis <- "census_2022"
  data$source <- "Regionaldatenbank Deutschland"
  data$source_table <- "12411-05-01-4"
  data$retrieved_at <- as.POSIXct("2026-09-14", tz = "UTC")
  data$data_status <- "synthetic-population"
  data$provenance_id <- "population-provenance"
  list(data = data, diagnostics = list(returned_observation_count = nrow(data)),
       provenance = list(population = list(data_status = "synthetic-population")))
}

test_that("analysis incidence uses the derived measure and preserves source incidence", {
  source <- analysis_incidence_bundle()
  result <- prepare_analysis_incidence(source, analysis_population())

  expect_identical(result$data$incidence, result$data$incidence_annual_average)
  expect_identical(result$data$incidence_source, source$data$incidence)
  expect_identical(result$source_incidence, source)
  expect_identical(result$data$incidence, c(0, 1000))
  expect_true(is.na(result$data$incidence_source[[1L]]))
  expect_true(all(result$data$incidence_status == "final"))
  expect_true(all(result$data$population_year == 2022L))
  expect_identical(result$diagnostics$primary_incidence,
                   "incidence_annual_average")
  expect_identical(result$diagnostics$structural_zero_policy,
                   "reviewed_structural_zero")
})

test_that("analysis incidence distinguishes explicit missing cases from structural zero", {
  source <- analysis_incidence_bundle()
  source$data$cases[[2L]] <- NA_real_
  result <- prepare_analysis_incidence(source, analysis_population())
  expect_identical(result$data$incidence[[1L]], 0)
  expect_true(is.na(result$data$incidence[[2L]]))

  attached <- result$data
  attached$period_set_id <- "test"
  attached$period_id <- "test"
  attached$period_definition_version <- "test_v1"
  attached$period_review_status <- "reviewed"
  attached$typology_id <- "test"
  attached$typology_definition_version <- "test_v1"
  attached$cluster_id <- "C01"
  summary <- summarize_incidence_by_typology(attached)$data
  expect_identical(summary$median_incidence, 0)
  expect_identical(summary$observed_non_missing_count, 1L)
})

test_that("final analysis refuses unavailable population without fallback", {
  source <- analysis_incidence_bundle(2026L)
  expect_error(
    prepare_analysis_incidence(source, analysis_population(2025L)),
    "must be explicitly provisional"
  )
  expect_error(
    prepare_analysis_incidence(
      analysis_incidence_bundle(2022L), analysis_population(2022L),
      c("2022" = 2021L)
    ),
    "only reporting year 2026"
  )
  expect_error(
    prepare_analysis_incidence(
      analysis_incidence_bundle(2021L), analysis_population(2021L)
    ),
    "only for reporting years 2022-2025"
  )
  expect_error(
    prepare_analysis_incidence(
      analysis_incidence_bundle(2026L), analysis_population(2026L)
    ),
    "must be explicitly provisional"
  )
})

test_that("explicit 2026 provisional analysis records the 2025 denominator", {
  result <- prepare_analysis_incidence(
    analysis_incidence_bundle(2026L), analysis_population(2025L),
    c("2026" = 2025L)
  )
  expect_true(all(result$data$reporting_year == 2026L))
  expect_true(all(result$data$population_year == 2025L))
  expect_true(all(result$data$incidence_status == "provisional"))
  expect_identical(result$diagnostics$status, "provisional")
  expect_identical(result$provenance$incidence[[1L]]$denominator$years, 2025L)

  population <- analysis_population(2025:2026)
  population$data$population[population$data$year == 2026L] <- 9999
  mixed <- prepare_analysis_incidence(
    analysis_incidence_bundle(2025:2026), population,
    c("2026" = 2025L)
  )
  expect_identical(
    mixed$diagnostics$status_by_reporting_year$incidence_status,
    c("final", "provisional")
  )
  expect_identical(
    mixed$diagnostics$status_by_reporting_year$population_year,
    c(2025L, 2025L)
  )
  expect_false(any(mixed$data$annual_average_population == 9999))
})

test_that("peak week is not assumed invariant across incidence definitions", {
  source <- analysis_incidence_bundle()
  second <- source$data
  second$date <- as.Date("2022-01-10")
  second$reporting_week <- 2L
  source$data <- rbind(source$data, second)
  source$data$incidence <- c(20, 20, 10, 10)
  source$data$cases <- c(1, 10, 2, 9)
  result <- prepare_analysis_incidence(source, analysis_population())

  source_week <- aggregate(incidence_source ~ reporting_week, result$data,
                           stats::median)
  derived_week <- aggregate(incidence ~ reporting_week, result$data,
                            stats::median)
  expect_identical(source_week$reporting_week[which.max(source_week$incidence_source)],
                   1L)
  expect_identical(derived_week$reporting_week[which.max(derived_week$incidence)],
                   2L)
})
