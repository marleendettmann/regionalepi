count_query <- function(id, role, status = as.POSIXct("2026-08-31 10:54:58", tz = "UTC"),
                        pathogen = "Disease", reference = TRUE,
                        reporting_path = "\u00dcber Gesundheitsamt und Landesstelle") {
  list(query_id = id, query_role = role, source = "SurvStat@RKI",
    source_version = "SurvStat@RKI 2.0", measure = "cases",
    source_measure_id = "[Measures].[FallCount_71_Web]",
    source_measure_label = "Anzahl.71s", value_semantics = "additive",
    pathogen = pathogen, reporting_years = 2024L, time_unit = "week",
    source_geography_dimension = if (grepl("kreis", role)) "survstat_kreis" else "survstat_bundesland",
    row_dimension = "Meldewoche", column_dimension = "Kreis",
    reference_definition = reference, reporting_path = reporting_path,
    epidemiological_filters = list(), retrieved_at = status,
    data_status = status, reporting_week_coverage = list(`2024` = 1L))
}

incidence_query <- function(id, role, ...) {
  x <- count_query(id, role, ...)
  x$measure <- "incidence"
  x$value_semantics <- "non_additive"
  x$source_measure_id <- "[Measures].[Inzidenz_71_Web]"
  x$source_measure_label <- "Inzidenz 7.1s"
  x
}

count_data <- function(ids, values, query_id, incidence = FALSE) {
  data.frame(geo_id = ids, geo_name = paste("Unit", ids), geo_level = "district",
    geo_vintage = as.Date(NA), date = as.Date("2024-01-01"), time_unit = "week",
    pathogen = "Disease", source = "SurvStat@RKI",
    source_version = "SurvStat@RKI 2.0", reporting_year = 2024L,
    reporting_week = 1L, query_id = query_id,
    stringsAsFactors = FALSE) |>
    transform(value = values) |>
    setNames(c("geo_id", "geo_name", "geo_level", "geo_vintage", "date",
      "time_unit", "pathogen", "source", "source_version", "reporting_year",
      "reporting_week", "query_id", if (incidence) "incidence" else "cases"))
}

test_that("count assembly applies reviewed replacement and compares additive sum", {
  base <- count_data(c("a", "b", "x"), c(2, 3, 7), "base")
  replacement <- count_data("11000", 5, "replacement")
  result <- assemble_surveillance_cases(base, replacement,
    count_query("base", "kreis_cases"), count_query("replacement", "bundesland_cases"),
    c("a", "b"), "11000", expected_output_units = 2L)
  expect_identical(sort(result$data$geo_id), c("11000", "x"))
  expect_true(result$diagnostics$all_comparable_sums_equal)
  expect_identical(result$data$cases[result$data$geo_id == "11000"], 5)
})

test_that("count assembly rejects duplicates and incomplete reviewed exclusions", {
  base <- count_data(c("a", "b", "x"), c(2, 3, 7), "base")
  replacement <- count_data("11000", 5, "replacement")
  args <- list(base, replacement, count_query("base", "kreis_cases"),
    count_query("replacement", "bundesland_cases"), c("a", "b"), "11000", 2L)
  expect_error(do.call(assemble_surveillance_cases, replace(args, 1L,
    list(rbind(base, base[1, ])))), "unique")
  expect_error(do.call(assemble_surveillance_cases, replace(args, 5L,
    list(c("a", "missing")))), "exclusion")
})

test_that("incidence and counts combine exactly without rate calculation", {
  ids <- c("01001", "11000")
  incidence <- list(data = count_data(ids, c(1.2, NA), "inc", TRUE),
    provenance = list(incidence_query("inc", "kreis_incidence")))
  counts <- list(data = count_data(ids, c(4, 0), "count"),
    provenance = list(count_query("count", "kreis_cases")))
  result <- combine_surveillance_incidence_counts(incidence, counts)
  expect_identical(result$data$incidence, incidence$data$incidence)
  expect_identical(result$data$cases, counts$data$cases)
  expect_false(result$diagnostics$rate_calculation_performed)
  expect_named(result$provenance, c("incidence", "counts"))
})

test_that("incidence/count compatibility rejects semantic and key mismatches", {
  ids <- c("01001", "11000")
  incidence <- list(data = count_data(ids, c(1.2, 2), "inc", TRUE),
    provenance = list(incidence_query("inc", "kreis_incidence")))
  counts <- list(data = count_data(ids, c(4, 5), "count"),
    provenance = list(count_query("count", "kreis_cases")))
  bad <- counts
  bad$data$geo_id[[1L]] <- "99999"
  expect_error(combine_surveillance_incidence_counts(incidence, bad), "key sets")
  bad <- counts
  bad$provenance[[1L]]$pathogen <- "Other"
  expect_error(combine_surveillance_incidence_counts(incidence, bad), "pathogen")
  bad <- counts
  bad$provenance[[1L]]$reference_definition <- FALSE
  expect_error(combine_surveillance_incidence_counts(incidence, bad), "reference_definition")
  bad <- counts
  bad$provenance[[1L]]$reporting_path <- "Other"
  expect_error(combine_surveillance_incidence_counts(incidence, bad), "reporting_path")
  bad <- counts
  bad$provenance[[1L]]$data_status <- bad$provenance[[1L]]$data_status + 1
  expect_error(combine_surveillance_incidence_counts(incidence, bad), "data_status")
})
