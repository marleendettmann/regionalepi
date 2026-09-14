golden_incidence_fixture <- function() {
  ids <- sprintf("%05d", 1:4)
  dates <- as.Date("2020-12-21") + 7L * 0:2
  x <- expand.grid(
    geo_id = ids, date = dates,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  x$geo_name <- paste("District", x$geo_id)
  x$geo_level <- "district"
  x$geo_vintage <- as.Date(NA)
  x$time_unit <- "week"
  x$pathogen <- "Disease"
  x$incidence <- c(0, 2, 8, 10, 0, 2, 0, 2, NA, NA, NA, NA)
  x$source <- "synthetic"
  x$query_id <- "golden-query"
  x$period_set_id <- "golden-periods-v1"
  x$period_id <- "golden-period"
  x$period_definition_version <- "v1"
  x$period_review_status <- "reviewed"
  x$typology_id <- "golden-typology"
  x$typology_definition_version <- "v1"
  x$cluster_id <- c("C01", "C01", "C02", "C02")[match(x$geo_id, ids)]
  x$cases <- NA_real_
  x
}

canonical_historical_assignment <- function(assignments) {
  x <- assignments[c("geo_id", "display_cluster_id")]
  names(x)[[2L]] <- "cluster_code"
  expect_identical(nrow(x), 401L)
  expect_identical(anyDuplicated(x$geo_id), 0L)
  expect_true(all(grepl("^[0-9]{5}$", x$geo_id)))
  expect_setequal(unique(x$cluster_code), c("ClD", "ClJ", "ClA"))
  x <- x[order(x$geo_id, method = "radix"), , drop = FALSE]

  # Scientific membership only: UTF-8 `geo_id|cluster_code` lines, sorted by
  # geo_id, joined by LF, with exactly one trailing LF. Changing the reviewed
  # literal below requires explicit scientific review of the full partition.
  lines <- enc2utf8(paste(x$geo_id, x$cluster_code, sep = "|"))
  list(data = x, bytes = charToRaw(paste0(paste(lines, collapse = "\n"), "\n")))
}

test_that("golden hash protects the complete historical typology assignment", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary
  fit <- regionalepi:::.shiny_fit_typology(summary, "dissertation", 3L)
  canonical <- canonical_historical_assignment(fit$assignments)
  assignments <- canonical$data

  expect_identical(
    as.integer(table(factor(assignments$cluster_code,
      levels = c("ClD", "ClJ", "ClA")))),
    c(63L, 213L, 125L)
  )
  expect_identical(assignments$cluster_code[assignments$geo_id == "11000"], "ClD")
  expect_identical(assignments$cluster_code[assignments$geo_id == "16056"], "ClA")
  expect_identical(assignments$cluster_code[assignments$geo_id == "16063"], "ClA")
  expect_identical(
    digest::digest(canonical$bytes, algo = "sha256", serialize = FALSE),
    "a80278e916ff30b4c35b70ae2790e00531a981ed7a1ab1f4f68374d26be95dcf"
  )

  map_join <- regionalepi:::.shiny_map_assignments(regionalepi_map_geometry(), fit)
  current <- regionalepi:::.shiny_current_display_assignments(map_join)
  expected <- assignments[assignments$geo_id != "16056", , drop = FALSE]
  current <- current[order(current$geo_id, method = "radix"),
    c("geo_id", "cluster_id"), drop = FALSE]
  names(current)[[2L]] <- "cluster_code"
  rownames(expected) <- NULL
  rownames(current) <- NULL
  expect_identical(nrow(current), 400L)
  expect_identical(map_join$typology_only_geo_ids, "16056")
  expect_identical(current, expected)
})

test_that("golden weekly estimand preserves zero NA and Core Shiny equality", {
  x <- golden_incidence_fixture()
  core <- summarize_incidence_by_typology(x)$data
  fit <- list(
    assignments = unique(data.frame(
      geo_id = x$geo_id, display_cluster_id = x$cluster_id
    )),
    provenance = list(fit_id = "golden-fit"),
    indicator_set = list(definition_version = "v1")
  )
  range <- list(start_date = min(x$date), end_date = max(x$date))
  shiny <- regionalepi:::.shiny_exploration_summaries(
    list(data = x), fit, range
  )$weekly
  joined <- merge(
    core, shiny, by = c("date", "cluster_id"),
    suffixes = c("_core", "_shiny"), sort = TRUE
  )

  expect_identical(joined$median_incidence_core, joined$median_incidence_shiny)
  expect_identical(joined$q1_incidence_core, joined$q1_incidence_shiny)
  expect_identical(joined$q3_incidence_core, joined$q3_incidence_shiny)
  expect_identical(
    joined$observed_non_missing_count, joined$observed_districts
  )
  expect_identical(joined$missing_count, joined$missing_districts)

  first_c01 <- joined$date == min(joined$date) & joined$cluster_id == "C01"
  expect_identical(joined$median_incidence_core[first_c01], 1)
  expect_identical(joined$q1_incidence_core[first_c01], 0.5)
  expect_identical(joined$q3_incidence_core[first_c01], 1.5)
  second_date <- sort(unique(x$date))[[2L]]
  expect_identical(core$zero_count[core$date == second_date], c(1L, 1L))
  expect_true(all(is.na(joined$median_incidence_core[joined$date == max(x$date)])))
  expect_identical(core$observed_non_missing_count[core$date == max(x$date)], c(0L, 0L))
  expect_identical(core$missing_count[core$date == max(x$date)], c(2L, 2L))
  expect_identical(core$completeness_proportion[core$date == max(x$date)], c(0, 0))
})

test_that("golden district period medians retain zero and omit missing weeks", {
  result <- summarize_period_incidence_by_district(
    golden_incidence_fixture()
  )$data
  result <- result[match(sprintf("%05d", 1:4), result$geo_id), ]

  expect_identical(result$median_period_incidence, c(0, 2, 4, 6))
  expect_identical(result$expected_week_count, rep(3L, 4L))
  expect_identical(result$observed_week_count, rep(2L, 4L))
  expect_identical(result$missing_week_count, rep(1L, 4L))
  expect_identical(result$zero_week_count, c(2L, 0L, 1L, 0L))
  expect_equal(result$completeness_proportion, rep(2 / 3, 4L))

  all_missing <- golden_incidence_fixture()
  all_missing$incidence[all_missing$geo_id == "00004"] <- NA_real_
  result <- summarize_period_incidence_by_district(all_missing)$data
  district <- result[result$geo_id == "00004", ]
  expect_true(is.na(district$median_period_incidence))
  expect_identical(district$observed_week_count, 0L)
  expect_identical(district$missing_week_count, 3L)
  expect_identical(district$completeness_proportion, 0)
})

test_that("golden national relative activity is the contemporaneous median contrast", {
  x <- golden_incidence_fixture()
  fit <- list(
    assignments = unique(data.frame(
      geo_id = x$geo_id, display_cluster_id = x$cluster_id
    )),
    provenance = list(fit_id = "golden-fit"),
    indicator_set = list(definition_version = "v1")
  )
  range <- list(start_date = min(x$date), end_date = max(x$date))
  weekly <- regionalepi:::.shiny_exploration_summaries(
    list(data = x), fit, range
  )$weekly
  weekly <- weekly[order(weekly$date, weekly$cluster_id), ]

  expect_identical(weekly$all_district_median, c(5, 5, 1, 1, NA, NA))
  expect_identical(weekly$relative_activity, c(-4, 4, 0, 0, NA, NA))
  expect_identical(
    weekly$relative_activity,
    weekly$median_incidence - weekly$all_district_median
  )
})

test_that("golden dissertation periods contain only real ISO weeks", {
  flu <- dissertation_influenza_periods()$periods
  expected_counts <- c(15L, 13L, 11L)
  observed_counts <- vapply(seq_len(nrow(flu)), function(index) {
    length(seq(flu$start_date[[index]], flu$end_date[[index]], by = "week"))
  }, integer(1L))
  expect_identical(observed_counts, expected_counts)

  first_dates <- seq(flu$start_date[[1L]], flu$end_date[[1L]], by = "week")
  expect_false("2017-53" %in% format(first_dates, "%G-%V"))
  expect_error(
    regionalepi:::.iso_week_monday(2017L, 53L, "golden ISO week"),
    "invalid ISO week"
  )
  expect_identical(
    regionalepi:::.iso_week_monday(2020L, 53L, "golden ISO week"),
    as.Date("2020-12-28")
  )

  covid <- dissertation_covid_welle2_periods()$periods
  cross_year <- covid[covid$period_id == "covid_wave_2", ]
  dates <- seq(cross_year$start_date, cross_year$end_date, by = "week")
  expect_true("2020-53" %in% format(dates, "%G-%V"))
  expect_identical(length(dates), 22L)
})
