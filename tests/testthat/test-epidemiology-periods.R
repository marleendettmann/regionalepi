period_row <- function(id = "wave-1", start = "2020-12-28", end = "2021-01-10") {
  data.frame(
    period_set_id = "set-1", period_id = id, pathogen = "Disease",
    season_id = "2020/21", period_type = "influenza_wave", label = id,
    start_date = as.Date(start), end_date = as.Date(end),
    definition_version = "v1", variant_context = NA_character_,
    historical_context = "synthetic", evidence_class = "REVIEWED_TEST_PERIOD",
    source_reference = "synthetic",
    review_status = "reviewed", note = NA_character_, stringsAsFactors = FALSE
  )
}

test_that("period contract accepts cross-year and week-53-compatible intervals", {
  x <- period_row()
  expect_invisible(validate_epidemiological_periods(x))
  expect_true(as.Date("2021-01-01") >= x$start_date)
  expect_true(as.Date("2021-01-01") <= x$end_date)
})

test_that("reviewed COVID activity waves and display metadata are exact", {
  periods <- rki_covid_activity_waves()$periods
  expect_identical(periods$period_set_id, rep("covid_rki_activity_waves_v1", 2L))
  expect_identical(format(periods$start_date), c("2023-10-02", "2024-05-27"))
  expect_identical(format(periods$end_date), c("2024-01-28", "2025-01-26"))
  expect_identical(periods$evidence_class, rep("REVIEWED_RKI_ACTIVITY_WAVE", 2L))
  expect_true(all(grepl("Bulletin 35/2025", periods$source_reference, fixed = TRUE)))
  expect_false(any(grepl("Phase 8|Wave 6|Welle 6", periods$label)))
  display <- format_epidemiological_period(periods[1, ])
  expect_identical(display$title, "COVID-19 · RKI-Aktivitätswelle 2023/24")
  expect_identical(display$subtitle, "02.10.2023–28.01.2024 · 2023-KW 40–2024-KW 04")
  same_year <- format_epidemiological_period(period_row(start = "2020-03-02", end = "2020-05-17"))
  expect_identical(same_year$subtitle, "02.03.2020–17.05.2020 · 2020-KW 10–20")
})

test_that("period contract rejects reversal, duplicate IDs, and overlap", {
  x <- period_row(start = "2021-01-02", end = "2021-01-01")
  expect_error(validate_epidemiological_periods(x), "after")
  x <- rbind(period_row(), period_row())
  expect_error(validate_epidemiological_periods(x), "unique")
  x <- rbind(period_row(), period_row("wave-2", "2021-01-10", "2021-02-01"))
  expect_error(validate_epidemiological_periods(x), "overlap")
})

test_that("season review represents zero, one, and multiple waves", {
  resource <- rki_influenza_periods_2017_2026()
  expect_identical(resource$season_review$wave_count, c(1L, 1L, 1L, 0L, 1L, 2L, 1L, 1L, 1L))
  expect_false("2020/21" %in% resource$periods$season_id)
  expect_identical(sum(resource$periods$season_id == "2022/23"), 2L)
})

test_that("assignment uses inclusive dates and reports outside observations", {
  observations <- data.frame(
    date = as.Date(c("2020-12-28", "2021-01-10", "2021-01-11")),
    pathogen = "Disease", value = 1:3, stringsAsFactors = FALSE
  )
  original <- observations
  result <- assign_epidemiological_periods(observations, period_row())
  expect_identical(result$data$period_id, c("wave-1", "wave-1", NA_character_))
  expect_identical(result$diagnostics$assigned_count, 2L)
  expect_identical(result$diagnostics$outside_period_count, 1L)
  expect_identical(observations, original)
})

test_that("assignment rejects pathogen mismatch and overlapping assignment", {
  observations <- data.frame(date = as.Date("2021-01-01"), pathogen = "Other")
  expect_error(assign_epidemiological_periods(observations, period_row()), "pathogen")
  overlapping <- rbind(period_row(), period_row("wave-2", "2021-01-01", "2021-01-02"))
  expect_error(assign_epidemiological_periods(
    transform(observations, pathogen = "Disease"), overlapping
  ), "overlap")
})

test_that("assignment requires one explicit period set", {
  periods <- rbind(
    period_row("wave-1", "2020-01-01", "2020-01-02"),
    transform(period_row("wave-2", "2020-02-01", "2020-02-02"),
      period_set_id = "set-2")
  )
  observations <- data.frame(date = as.Date("2020-01-01"), pathogen = "Disease")
  expect_error(assign_epidemiological_periods(observations, periods), "exactly one period set")
})

test_that("frozen dissertation boundaries are exact", {
  flu <- dissertation_influenza_periods()$periods
  expect_identical(format(flu$start_date), c("2017-12-25", "2019-01-07", "2020-01-06"))
  expect_identical(format(flu$end_date), c("2018-04-08", "2019-04-07", "2020-03-22"))
  covid <- dissertation_covid_welle2_periods()$periods
  expect_identical(covid$period_id, paste0("covid_wave_", c("1", "2", "3", "4a", "4b", "5a", "5b")))
  expect_identical(format(covid$start_date), c(
    "2020-03-02", "2020-09-28", "2021-03-01", "2021-08-02",
    "2021-10-04", "2021-12-27", "2022-02-28"
  ))
  expect_identical(format(covid$end_date), c(
    "2020-05-17", "2021-02-28", "2021-06-13", "2021-10-03",
    "2021-12-26", "2022-02-27", "2022-05-29"
  ))
})
