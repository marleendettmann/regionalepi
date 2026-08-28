survstat_incidence_kreis_lines <- function(
    weeks = c("01", "02"), geographies = c("LK Alpha", "SK Beta"),
    values = matrix(c("1,25", "", "0,00", "2,50"), nrow = 2L, byrow = TRUE)) {
  quoted <- function(x) paste0('"', x, '"')
  rows <- c(
    paste(quoted(c("Kreis", "Meldewoche")), collapse = "\t"),
    paste(quoted(c("", "Gesamt", weeks)), collapse = "\t"),
    paste(quoted(c("Gesamt", "3,75", rep("1,00", length(weeks)))), collapse = "\t")
  )
  for (index in seq_along(geographies)) {
    rows <- c(rows, paste(
      quoted(c(geographies[[index]], "1,00", values[index, ])),
      collapse = "\t"
    ))
  }
  c(rows, paste(quoted(c("Unbekannt", "", rep("", length(weeks)))), collapse = "\t"))
}

survstat_incidence_year_lines <- function(
    weeks = c("01", "02", "53"), years = c("2024", "2020"),
    values = list(
      `2024` = c("1,10", "0,00", ""),
      `2020` = c("2,20", "", "3,30")
    )) {
  quoted <- function(x) paste0('"', x, '"')
  rows <- c(
    paste(quoted(c("Meldejahr", "Meldewoche")), collapse = "\t"),
    paste(quoted(c("", "Gesamt", weeks)), collapse = "\t"),
    paste(quoted(c("Gesamt", "6,60", rep("1,00", length(weeks)))), collapse = "\t")
  )
  for (year in years) {
    rows <- c(rows, paste(quoted(c(year, "3,30", values[[year]])), collapse = "\t"))
  }
  rows
}

test_that("Kreis incidence layout preserves decimal, zero, and blank values", {
  file <- write_utf16_survstat_fixture(survstat_incidence_kreis_lines())
  retrieved <- as.POSIXct("2026-08-28 10:00:00", tz = "UTC")
  status <- as.POSIXct("2026-08-28 08:00:00", tz = "UTC")
  result <- read_survstat_incidence(
    file, "kreis_rows", "Influenza, saisonal", 2024,
    "kreis-2024", "kreis_incidence",
    retrieved_at = retrieved, data_status = status,
    reference_definition = TRUE,
    reporting_path = "Über Gesundheitsamt und Landesstelle",
    epidemiological_filters = list(definition = "reviewed"),
    empty_rows_and_columns = TRUE, totals = TRUE
  )
  expect_named(result, c("data", "diagnostics", "provenance"),
               ignore.order = FALSE)
  expect_identical(validate_surveillance_incidence(result$data), result$data)
  expect_identical(result$data$incidence, c(1.25, NA, 0, 2.5))
  expect_identical(result$data$geo_level, rep("survstat_kreis", 4L))
  expect_identical(result$data$query_id, rep("kreis-2024", 4L))
  expect_identical(result$diagnostics$blank_incidence_cells, 1L)
  expect_identical(result$diagnostics$numeric_zero_incidence_cells, 1L)
  expect_true(result$diagnostics$presentation_totals_not_reconciled)
  expect_identical(result$provenance$query_role, "kreis_incidence")
  expect_identical(result$provenance$reporting_years, 2024L)
})

test_that("Meldejahr layout supports multiple years and applicable ISO weeks", {
  file <- write_utf16_survstat_fixture(survstat_incidence_year_lines())
  source_geography <- list(
    geo_id = NA_character_, geo_name = "Reviewed source state",
    geo_level = "survstat_bundesland"
  )
  result <- read_survstat_incidence(
    file, "reporting_year_rows", "Influenza, saisonal", c(2020, 2024),
    "state-2020-2024", "bundesland_incidence",
    source_geography = source_geography,
    data_status = as.POSIXct("2026-08-28 08:00:00", tz = "UTC")
  )
  expect_identical(result$provenance$reporting_years, c(2020L, 2024L))
  expect_identical(names(result$provenance$reporting_week_coverage),
                   c("2020", "2024"))
  expect_identical(result$provenance$reporting_week_coverage$`2020`,
                   c(1L, 2L, 53L))
  expect_identical(result$provenance$reporting_week_coverage$`2024`, c(1L, 2L))
  expect_identical(nrow(result$data), 5L)
  expect_true(any(result$data$reporting_year == 2020L &
                    result$data$reporting_week == 53L))
  expect_false(any(result$data$reporting_year == 2024L &
                     result$data$reporting_week == 53L))
  expect_true(any(is.na(result$data$incidence)))
  expect_true(any(result$data$incidence == 0, na.rm = TRUE))
  expect_true(all(result$data$geo_name == "Reviewed source state"))
  expect_true(all(result$data$geo_level == "survstat_bundesland"))
})

test_that("incidence source geography and evidenced layouts are strict", {
  year_file <- write_utf16_survstat_fixture(survstat_incidence_year_lines())
  arguments <- list(
    file = year_file, layout = "reporting_year_rows", pathogen = "Disease",
    reporting_years = c(2020, 2024), query_id = "q", query_role = "replacement"
  )
  expect_error(do.call(read_survstat_incidence, arguments), "named list")
  arguments$source_geography <- list(
    geo_id = NA_character_, geo_name = "", geo_level = "survstat_bundesland"
  )
  expect_error(do.call(read_survstat_incidence, arguments), "geo_name")
  arguments$source_geography <- list(
    geo_id = NA_character_, geo_name = "State", geo_level = "district"
  )
  expect_error(do.call(read_survstat_incidence, arguments), "survstat_bundesland")
  kreis_file <- write_utf16_survstat_fixture(survstat_incidence_kreis_lines())
  expect_error(
    read_survstat_incidence(
      kreis_file, "kreis_rows", "Disease", c(2020, 2024), "q", "base"
    ),
    "exactly one reporting year"
  )
  expect_error(
    read_survstat_incidence(kreis_file, "arbitrary", "Disease", 2024, "q", "base"),
    "layout"
  )
})

test_that("incidence decimal parser rejects decimal points", {
  lines <- survstat_incidence_kreis_lines(
    weeks = "01", geographies = "LK Alpha",
    values = matrix("1.25", nrow = 1L)
  )
  expect_error(
    read_survstat_incidence(
      write_utf16_survstat_fixture(lines), "kreis_rows", "Disease", 2024, "q", "base"
    ),
    "decimal-comma"
  )
})

test_that("inapplicable ISO week values cannot be silently omitted", {
  lines <- survstat_incidence_year_lines(
    years = "2024", values = list(`2024` = c("1,00", "2,00", "3,00"))
  )
  expect_error(
    read_survstat_incidence(
      write_utf16_survstat_fixture(lines), "reporting_year_rows", "Disease",
      2024, "q", "replacement",
      source_geography = list(
        geo_id = NA_character_, geo_name = "State",
        geo_level = "survstat_bundesland"
      )
    ),
    "inapplicable ISO week"
  )
})

test_that("geography resolution accepts incidence without weakening count contract", {
  data <- surveillance_incidence_example()
  data$geo_name <- "LK Alpha"
  data$source <- "SurvStat@RKI"
  data$source_version <- "SurvStat@RKI 2.0"
  geography <- data.frame(
    geo_id = "01001", geo_name = "Alpha", geo_level = "district",
    valid_from = as.Date("2020-01-01"), valid_to = as.Date(NA),
    source = "synthetic", canonical_type = "Land", stringsAsFactors = FALSE
  )
  result <- resolve_geography(
    data, geography, as.Date("2024-12-31"), "SurvStat@RKI", "canonical_type"
  )
  expect_identical(result$data$geo_id, "01001")
  expect_identical(validate_surveillance_incidence(result$data), result$data)
  expect_false("cases" %in% names(result$data))
})
