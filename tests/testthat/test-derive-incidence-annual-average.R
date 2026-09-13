annual_incidence_cases <- function() {
  data <- data.frame(
    geo_id = rep(c("01001", "07315"), each = 4L),
    geo_name = rep(c("Flensburg", "Mainz"), each = 4L),
    geo_level = "district", geo_vintage = as.Date(NA),
    date = rep(as.Date(c(
      "2022-01-03", "2022-01-10", "2023-01-02", "2023-01-09"
    )), 2L),
    time_unit = "week", pathogen = "Disease",
    cases = c(0, 1, 1, NA, 3, 7, 11, 13),
    source = "SurvStat@RKI", source_version = "SurvStat@RKI 2.0",
    reporting_year = rep(c(2022L, 2022L, 2023L, 2023L), 2L),
    reporting_week = rep(c(1L, 2L, 1L, 2L), 2L),
    query_id = "cases-query", stringsAsFactors = FALSE
  )
  list(
    data = data,
    diagnostics = list(operation = "synthetic cases"),
    provenance = list(list(
      query_id = "cases-query", measure = "cases",
      source = "SurvStat@RKI"
    ))
  )
}

annual_incidence_population <- function() {
  data <- data.frame(
    geo_id = rep(c("01001", "07315"), each = 2L),
    geo_name = rep(c("Flensburg", "Mainz, kreisfreie Stadt"), each = 2L),
    geo_level = "district", geo_vintage = as.Date(NA),
    year = rep(2022:2023, 2L),
    population = c(3, 7, 11, 13),
    population_measure = "annual_average_population",
    population_reference = "reporting_year_annual_average",
    population_basis = "census_2022",
    source = "Regionaldatenbank Deutschland",
    source_table = "12411-05-01-4",
    retrieved_at = as.POSIXct("2026-09-14", tz = "UTC"),
    data_status = "synthetic",
    provenance_id = "regional_average_population_12411-05-01-4_bev028",
    stringsAsFactors = FALSE
  )
  list(
    data = data,
    diagnostics = list(operation = "synthetic population"),
    provenance = list(
      regional_average_population_12411_05_01_4_bev028 = list(
        source_table = "12411-05-01-4", source_measure = "BEV028"
      )
    )
  )
}

test_that("annual-average incidence uses exact full-precision arithmetic", {
  result <- derive_incidence_annual_average(
    annual_incidence_cases(), annual_incidence_population()
  )
  data <- result$data
  expected <- data$cases / data$annual_average_population * 100000
  expect_identical(data$incidence_annual_average, expected)
  expect_identical(data$incidence_annual_average[[1L]], 0)
  expect_true(is.na(data$incidence_annual_average[[4L]]))
  expect_identical(
    data$incidence_annual_average[[2L]], 1 / 3 * 100000
  )
  expect_identical(validate_annual_average_incidence(data), data)
  expect_false("incidence" %in% names(data))
  expect_true(all(data$incidence_status == "final"))
  expect_identical(
    unique(data$incidence_definition_version),
    "annual_average_population_v1"
  )
  expect_false(result$diagnostics$rounding_performed)
  expect_identical(result$diagnostics$weighting, "none")
  expect_false(result$diagnostics$age_or_sex_standardization)
})

test_that("annual-average incidence joins exactly by leading-zero AGS and year", {
  cases <- annual_incidence_cases()
  population <- annual_incidence_population()
  population$data <- population$data[c(4L, 2L, 3L, 1L), ]
  population$data$geo_name <- paste("Different source label", seq_len(4L))
  result <- derive_incidence_annual_average(cases, population)
  expect_identical(
    result$data$annual_average_population,
    c(3, 3, 7, 7, 11, 11, 13, 13)
  )
  expect_identical(result$data$geo_name, cases$data$geo_name)
  expect_true("01001" %in% result$data$geo_id)
  expect_identical(
    result$provenance[[1L]]$join_keys,
    c("geo_id", "reporting_year")
  )
})

test_that("annual-average incidence rejects missing denominator keys", {
  cases <- annual_incidence_cases()
  population <- annual_incidence_population()
  population$data <- population$data[-1L, ]
  expect_error(
    derive_incidence_annual_average(cases, population),
    "matching denominator"
  )
  population <- annual_incidence_population()
  population$data$year[population$data$geo_id == "01001" &
      population$data$year == 2022L] <- 2024L
  expect_error(
    derive_incidence_annual_average(cases, population),
    "matching denominator"
  )
})

test_that("annual-average incidence permits unused population years and districts", {
  population <- annual_incidence_population()
  extra <- population$data[1L, ]
  extra$geo_id <- "09999"
  extra$year <- 2024L
  extra$population <- 99
  population$data <- rbind(population$data, extra)
  result <- derive_incidence_annual_average(
    annual_incidence_cases(), population
  )
  expect_equal(nrow(result$data), 8L)
  expect_identical(
    result$provenance[[1L]]$denominator$years,
    2022:2023
  )
  expect_identical(result$diagnostics$unused_population_keys, 1L)
})

test_that("annual-average incidence rejects invalid denominators and duplicates", {
  for (bad in list(0, -1, Inf, NA_real_)) {
    population <- annual_incidence_population()
    population$data$population[[1L]] <- bad
    expect_error(
      derive_incidence_annual_average(annual_incidence_cases(), population),
      "population"
    )
  }
  population <- annual_incidence_population()
  population$data <- rbind(population$data, population$data[1L, ])
  expect_error(
    derive_incidence_annual_average(annual_incidence_cases(), population),
    "unique"
  )
})

test_that("annual-average incidence rejects unsafe input and definition collisions", {
  cases <- annual_incidence_cases()
  cases$data$reporting_year[[1L]] <- NA_integer_
  expect_error(
    derive_incidence_annual_average(cases, annual_incidence_population()),
    "reporting_year"
  )
  cases <- annual_incidence_cases()
  cases$data$incidence <- 1
  expect_error(
    derive_incidence_annual_average(cases, annual_incidence_population()),
    "not incidence"
  )
  cases <- annual_incidence_cases()
  cases$data$incidence_annual_average <- 1
  expect_error(
    derive_incidence_annual_average(cases, annual_incidence_population()),
    "already contains"
  )
  expect_error(
    derive_incidence_annual_average(
      annual_incidence_cases(), annual_incidence_population(), "v2"
    ),
    "annual_average_population_v1"
  )
  cases <- annual_incidence_cases()
  cases$data$source <- "other source"
  expect_error(
    derive_incidence_annual_average(cases, annual_incidence_population()),
    "source-provided SurvStat@RKI cases"
  )
})

test_that("derived contract verifies formula and final status", {
  data <- derive_incidence_annual_average(
    annual_incidence_cases(), annual_incidence_population()
  )$data
  data$incidence_annual_average[[2L]] <- 1
  expect_error(validate_annual_average_incidence(data), "does not equal")
  data <- derive_incidence_annual_average(
    annual_incidence_cases(), annual_incidence_population()
  )$data
  data$incidence_status <- "provisional"
  expect_error(validate_annual_average_incidence(data), "must be final")
  data <- derive_incidence_annual_average(
    annual_incidence_cases(), annual_incidence_population()
  )$data
  data$source <- "other source"
  expect_error(
    validate_annual_average_incidence(data),
    "source-provided SurvStat@RKI cases"
  )
})

test_that("source incidence remains untouched and historically separate", {
  source <- annual_incidence_cases()$data
  source$incidence <- c(0, 1, 2, NA, 3, 4, 5, 6)
  source$cases <- NULL
  source$period_set_id <- "historical-test-periods"
  source$period_id <- "historical-test-period"
  source$period_definition_version <- "dissertation_v1"
  source$period_review_status <- "reviewed"
  source$typology_id <- "dissertation_v1"
  source$typology_definition_version <- "dissertation_v1"
  source$cluster_id <- ifelse(source$geo_id == "01001", "ClD", "ClA")
  source_before <- source
  historical <- summarize_incidence_by_typology(source)$data
  derived <- derive_incidence_annual_average(
    annual_incidence_cases(), annual_incidence_population()
  )$data

  expect_identical(source, source_before)
  expect_identical(
    historical$median_incidence,
    summarize_incidence_by_typology(source_before)$data$median_incidence
  )
  expect_error(summarize_incidence_by_typology(derived), "incidence")
  expect_error(summarize_period_incidence_by_district(derived), "incidence")
  expect_false("incidence_annual_average" %in%
    names(dissertation_influenza_periods()$periods))
  expect_false("incidence_annual_average" %in%
    names(dissertation_covid_welle2_periods()$periods))
})
