write_survstat_fixture <- function(lines, bom = TRUE) {
  path <- tempfile(fileext = ".csv")
  connection <- file(path, open = "wb")
  on.exit(close(connection))
  if (bom) {
    writeBin(as.raw(c(0xff, 0xfe)), connection)
  }
  bytes <- iconv(paste0(lines, collapse = "\r\n"),
                 from = "UTF-8", to = "UTF-16LE", toRaw = TRUE)[[1L]]
  writeBin(bytes, connection)
  path
}

survstat_lines <- function(
    weeks = c("01", "02"),
    geographies = c("Köln, Stadt", "Märkischer Kreis"),
    values = matrix(c("1", "", "2", "3"), nrow = 2L, byrow = TRUE),
    national = c("3", "3"),
    unknown = rep("", length(weeks))) {
  stopifnot(length(weeks) == ncol(values), length(geographies) == nrow(values))
  quoted <- function(x) paste0('"', x, '"')
  rows <- c(
    paste(quoted(c("Kreis", "Meldewoche")), collapse = "\t"),
    paste(quoted(c("", "Gesamt", weeks)), collapse = "\t"),
    paste(quoted(c("Gesamt", sum(suppressWarnings(as.numeric(national))), national)),
          collapse = "\t")
  )
  for (i in seq_along(geographies)) {
    total <- sum(suppressWarnings(as.numeric(values[i, ])), na.rm = TRUE)
    rows <- c(rows, paste(quoted(c(geographies[i], total, values[i, ])), collapse = "\t"))
  }
  if (!is.null(unknown)) {
    rows <- c(rows, paste(quoted(c("Unbekannt", "", unknown)), collapse = "\t"))
  }
  rows
}

test_that("read_survstat returns validated canonical data and diagnostics", {
  file <- write_survstat_fixture(survstat_lines())
  retrieved <- as.POSIXct("2024-02-01 10:00:00", tz = "UTC")
  status <- as.POSIXct("2024-01-31 00:00:00", tz = "UTC")
  result <- read_survstat(
    file, "Influenza", 2024,
    retrieved_at = retrieved,
    data_status = status,
    reference_definition = TRUE,
    reporting_path = "Infektionsschutz > Influenza"
  )

  expect_named(result, c("data", "diagnostics"), ignore.order = FALSE)
  expect_s3_class(result$data, "data.frame")
  expect_identical(class(result), "list")
  expect_identical(validate_surveillance(result$data), result$data)
  expect_identical(result$data$geo_name, rep(c("Köln, Stadt", "Märkischer Kreis"), each = 2L))
  expect_identical(result$data$geo_id, rep(NA_character_, 4L))
  expect_identical(result$data$geo_level, rep("survstat_kreis", 4L))
  expect_true(all(is.na(result$data$geo_vintage)))
  expect_identical(result$data$date, as.Date(c("2024-01-01", "2024-01-08",
                                               "2024-01-01", "2024-01-08")))
  expect_identical(result$data$cases, c(1, 0, 2, 3))
  expect_identical(result$data$reporting_week, c(1L, 2L, 1L, 2L))
  expect_identical(result$data$retrieved_at, rep(retrieved, 4L))
  expect_identical(result$data$data_status, rep(status, 4L))
  expect_identical(result$data$reference_definition, rep(TRUE, 4L))
  expect_identical(result$data$reporting_path,
                   rep("Infektionsschutz > Influenza", 4L))

  expect_identical(result$diagnostics$source_file, basename(file))
  expect_identical(result$diagnostics$encoding, "UTF-16LE")
  expect_identical(result$diagnostics$delimiter, "tab")
  expect_identical(result$diagnostics$source_geographic_units, 2L)
  expect_identical(result$diagnostics$observation_rows, 4L)
  expect_identical(result$diagnostics$blank_observation_cells, 1L)
  expect_identical(result$diagnostics$blanks_converted_to_zero, 1L)
  expect_true(result$diagnostics$unknown_row_present)
  expect_true(result$diagnostics$national_totals_reconciled)
  expect_true(all(result$diagnostics$national_weekly_totals$reconciled))
  expect_true(result$diagnostics$geographic_ids_unresolved)
  expect_true(result$diagnostics$geographic_vintage_unresolved)
  expect_false(grepl(dirname(file), result$diagnostics$source_file, fixed = TRUE))
})

test_that("terminal blank lines do not become source geography rows", {
  file <- write_survstat_fixture(c(survstat_lines(), ""))
  result <- read_survstat(file, "synthetic", 2024)

  expect_identical(result$diagnostics$source_geographic_units, 2L)
  expect_false(any(result$data$geo_name == ""))
})

test_that("read_survstat preserves supplied geography vintage and logical provenance", {
  file <- write_survstat_fixture(survstat_lines())
  result <- read_survstat(
    file, "Influenza", 2024, geo_vintage = as.Date("2024-01-01"),
    reference_definition = FALSE
  )
  expect_true(all(result$data$geo_vintage == as.Date("2024-01-01")))
  expect_true(all(result$data$reference_definition == FALSE))
  expect_false(result$diagnostics$geographic_vintage_unresolved)

  unknown <- read_survstat(file, "Influenza", 2024)
  expect_type(unknown$data$reference_definition, "logical")
  expect_true(all(is.na(unknown$data$reference_definition)))
})

test_that("blank handling is explicit", {
  file <- write_survstat_fixture(survstat_lines())
  expect_error(
    read_survstat(file, "Influenza", 2024, blank_is_zero = FALSE),
    "blank geographic weekly cell"
  )
  lines <- survstat_lines(national = c("", "3"))
  file <- write_survstat_fixture(lines)
  expect_error(read_survstat(file, "Influenza", 2024),
               "national weekly totals must not contain blank")
})

test_that("national weekly totals are reconciled exactly", {
  lines <- survstat_lines(national = c("4", "3"))
  file <- write_survstat_fixture(lines)
  expect_error(read_survstat(file, "Influenza", 2024),
               "W01.*declared 4.*imported 3")

  lines <- survstat_lines(unknown = c("1", ""), national = c("4", "3"))
  file <- write_survstat_fixture(lines)
  expect_error(read_survstat(file, "Influenza", 2024), "national weekly total mismatch")
})

test_that("case count validation rejects malformed values", {
  fixture <- function(value, national = value) {
    write_survstat_fixture(survstat_lines(
      weeks = "01", geographies = "A", values = matrix(value, nrow = 1L),
      national = national, unknown = NULL
    ))
  }
  expect_error(read_survstat(fixture("x"), "Disease", 2024), "unexpected non-numeric")
  expect_error(read_survstat(fixture("1,5"), "Disease", 2024), "unexpected non-numeric")
  expect_error(read_survstat(fixture("1.5"), "Disease", 2024), "whole-valued")
  expect_error(read_survstat(fixture("-1"), "Disease", 2024), "non-negative")
  expect_error(read_survstat(fixture("Inf"), "Disease", 2024), "finite")
  expect_error(read_survstat(fixture("NaN"), "Disease", 2024), "finite")
})

test_that("ISO week conversion handles year boundaries and week 53", {
  make <- function(year, weeks) {
    values <- matrix(rep("0", length(weeks)), nrow = 1L)
    file <- write_survstat_fixture(survstat_lines(
      weeks = weeks, geographies = "A", values = values,
      national = rep("0", length(weeks)), unknown = NULL
    ))
    read_survstat(file, "Disease", year)$data$date
  }
  expect_identical(make(2019, "01"), as.Date("2018-12-31"))
  expect_identical(make(2020, "53"), as.Date("2020-12-28"))
  expect_identical(make(2021, "01"), as.Date("2021-01-04"))
  expect_error(make(2024, "53"), "invalid ISO week")
  expect_error(make(2024, "00"), "invalid ISO week")
})

test_that("supported SurvStat presentation structure is enforced", {
  mutate_line <- function(lines, row, pattern, replacement) {
    lines[row] <- sub(pattern, replacement, lines[row], fixed = TRUE)
    write_survstat_fixture(lines)
  }
  lines <- survstat_lines()
  expect_error(read_survstat(mutate_line(lines, 1L, "Kreis", "Gebiet"),
                             "Disease", 2024), "first header")
  expect_error(read_survstat(mutate_line(lines, 2L, "Gesamt", "Summe"),
                             "Disease", 2024), "second header")
  expect_error(read_survstat(mutate_line(lines, 2L, '"02"', '"01"'),
                             "Disease", 2024), "weekly headers.*unique")
  expect_error(read_survstat(mutate_line(lines, 3L, "Gesamt", "Bund"),
                             "Disease", 2024), "exactly one national")
  duplicate_total <- append(lines, lines[3L], after = 3L)
  expect_error(read_survstat(write_survstat_fixture(duplicate_total),
                             "Disease", 2024), "exactly one national")
  expect_error(read_survstat(mutate_line(lines, 4L, "Köln, Stadt", "Märkischer Kreis"),
                             "Disease", 2024), "labels must be unique")
  expect_error(read_survstat(mutate_line(lines, 4L, "Köln, Stadt", ""),
                             "Disease", 2024), "labels must not be blank")
})

test_that("encoding, file, and scalar arguments are validated", {
  no_bom <- write_survstat_fixture(survstat_lines(), bom = FALSE)
  expect_error(read_survstat(no_bom, "Disease", 2024), "UTF-16LE BOM")
  expect_error(read_survstat(tempfile(), "Disease", 2024), "existing regular file")
  file <- write_survstat_fixture(survstat_lines())
  expect_error(read_survstat(file, "", 2024), "pathogen")
  expect_error(read_survstat(file, "Disease", 2024.5), "reporting_year")
  expect_error(read_survstat(file, "Disease", 2024, geo_vintage = "2024-01-01"),
               "geo_vintage")
  expect_error(read_survstat(file, "Disease", 2024, retrieved_at = Sys.Date()),
               "retrieved_at")
  expect_error(read_survstat(file, "Disease", 2024, data_status = Sys.Date()),
               "data_status")
  expect_error(read_survstat(file, "Disease", 2024, reference_definition = "yes"),
               "reference_definition")
  expect_error(read_survstat(file, "Disease", 2024, reporting_path = TRUE),
               "reporting_path")
  expect_error(read_survstat(file, "Disease", 2024, blank_is_zero = NA),
               "blank_is_zero")
})

test_that("zero-geography inputs produce typed zero-row canonical data", {
  lines <- survstat_lines(
    weeks = c("01", "02"), geographies = character(),
    values = matrix(character(), nrow = 0L, ncol = 2L),
    national = c("0", "0"), unknown = c("", "")
  )
  result <- read_survstat(write_survstat_fixture(lines), "Disease", 2024)
  expect_equal(nrow(result$data), 0L)
  expect_identical(validate_surveillance(result$data), result$data)
  expect_s3_class(result$data$geo_vintage, "Date")
  expect_s3_class(result$data$date, "Date")
  expect_s3_class(result$data$retrieved_at, "POSIXct")
  expect_type(result$data$reference_definition, "logical")
  expect_identical(result$diagnostics$source_geographic_units, 0L)
  expect_false(result$diagnostics$geographic_ids_unresolved)
})

test_that("read_survstat is reproducible for identical inputs", {
  file <- write_survstat_fixture(survstat_lines())
  arguments <- list(
    file = file, pathogen = "Influenza", reporting_year = 2024,
    retrieved_at = as.POSIXct("2024-02-01", tz = "UTC")
  )
  expect_identical(do.call(read_survstat, arguments), do.call(read_survstat, arguments))
})
