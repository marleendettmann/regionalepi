test_that("reviewed district-to-state crosswalk is exact", {
  x <- regionalepi_district_state_crosswalk()
  expect_invisible(validate_district_state_crosswalk(x))
  expect_identical(names(x), c(
    "geo_id", "state_id", "state_name", "reference_date", "source",
    "definition_version"
  ))
  expect_identical(nrow(x), 400L)
  expect_identical(length(unique(x$state_id)), 16L)
  expect_identical(sum(x$state_name == "Berlin"), 1L)
  expect_identical(sum(x$state_name == "Hamburg"), 1L)
  expect_identical(sum(x$state_name == "Bremen"), 2L)
  expect_true(all(x$reference_date == as.Date("2024-12-31")))
  expect_error(regionalepi_district_state_crosswalk("latest"), "Unsupported")

  bad <- x; bad$state_id[[1L]] <- "99"
  expect_error(validate_district_state_crosswalk(bad), "inconsistent")
  bad <- x[-1L, ]
  expect_error(validate_district_state_crosswalk(bad), "exactly 400")
})

test_that("state composition uses district fractions and explicit absences", {
  crosswalk <- regionalepi_district_state_crosswalk()
  assignments <- data.frame(
    geo_id = crosswalk$geo_id,
    cluster_id = ifelse(seq_len(nrow(crosswalk)) %% 3L == 0L, "C03",
      ifelse(seq_len(nrow(crosswalk)) %% 2L == 0L, "C02", "C01")),
    stringsAsFactors = FALSE
  )
  # Make the one-district city states explicit single-cluster compositions.
  assignments$cluster_id[crosswalk$state_id %in% c("02", "11")] <- "C03"
  result <- summarize_cluster_composition_by_state(assignments, crosswalk)
  expect_identical(nrow(result), 48L)
  expect_equal(as.numeric(tapply(
    result$district_fraction, result$state_id, sum
  )), rep(1, 16L))
  expect_identical(sum(result$n_districts), 400L)
  expect_true(any(result$n_districts == 0L))
  berlin <- result[result$state_name == "Berlin", ]
  hamburg <- result[result$state_name == "Hamburg", ]
  bremen <- result[result$state_name == "Bremen", ]
  expect_identical(unique(berlin$state_total_districts), 1L)
  expect_identical(unique(hamburg$state_total_districts), 1L)
  expect_identical(unique(bremen$state_total_districts), 2L)
  expect_identical(sum(berlin$n_districts), 1L)

  expect_error(
    summarize_cluster_composition_by_state(assignments[-1L, ], crosswalk),
    "exactly the reviewed 400"
  )
})

test_that("state incidence comparison retains district-level estimand", {
  crosswalk <- regionalepi_district_state_crosswalk()
  ids <- crosswalk$geo_id[1:3]
  dates <- as.Date(c("2024-01-01", "2024-01-08", "2024-01-15"))
  data <- expand.grid(date = dates, geo_id = ids, stringsAsFactors = FALSE)
  data$geo_name <- paste("District", data$geo_id)
  data$cluster_id <- rep(c("C01", "C01", "C02"), each = length(dates))
  data$incidence <- c(1, 3, 5, 2, NA, 6, 4, 8, 12)
  data$cases <- c(1, 2, 3, 4, NA, 6, NA, NA, NA)
  result <- summarize_state_cluster_incidence(data, crosswalk)
  expect_identical(nrow(result), 3L)
  first <- result[result$geo_id == ids[[1L]], ]
  expect_identical(first$period_median_incidence, 3)
  expect_identical(first$cumulative_reported_cases, 6)
  second <- result[result$geo_id == ids[[2L]], ]
  expect_identical(second$observed_weeks, 2L)
  expect_identical(second$missing_weeks, 1L)
  third <- result[result$geo_id == ids[[3L]], ]
  expect_true(is.na(third$cumulative_reported_cases))
  expect_false(any(grepl("state_incidence|weighted", names(result))))

  duplicate <- rbind(data, data[1L, ])
  expect_error(summarize_state_cluster_incidence(duplicate, crosswalk),
               "unique")
})

test_that("state comparison display rules are deterministic", {
  crosswalk <- regionalepi_district_state_crosswalk()
  state_ids <- c(rep("01", 5L), rep("03", 4L), rep("04", 2L), "11")
  ids <- unlist(lapply(split(state_ids, state_ids), function(id) {
    head(crosswalk$geo_id[crosswalk$state_id == id[[1L]]], length(id))
  }), use.names = FALSE)
  rows <- crosswalk[match(ids, crosswalk$geo_id), ]
  summary <- data.frame(
    geo_id = rows$geo_id, geo_name = paste("District", rows$geo_id),
    state_id = rows$state_id, state_name = rows$state_name,
    cluster_id = "C01", period_median_incidence = seq_along(ids),
    observed_weeks = 3L, expected_weeks = 4L, missing_weeks = 1L,
    cumulative_reported_cases = seq_along(ids), stringsAsFactors = FALSE
  )
  display <- regionalepi:::.shiny_state_comparison_display(
    summary, "C01", crosswalk
  )
  rule <- stats::setNames(display$groups$display_rule,
                          display$groups$state_id)
  expect_identical(rule[["01"]], "points_and_boxplot")
  expect_identical(rule[["03"]], "points_and_median")
  expect_identical(rule[["04"]], "points_and_median")
  expect_identical(rule[["11"]], "point_only")
  expect_identical(rule[["02"]], "unavailable")
})

test_that("state composition supports every current typology k and reference", {
  crosswalk <- regionalepi_district_state_crosswalk()
  map_ids <- regionalepi_map_geometry()$features$geo_id
  for (years in list(2017:2020, 2022:2024)) {
    summary <- regionalepi:::.shiny_fetch_snapshot_demography(years)$summary
    for (k in 2:5) {
      fit <- fit_dynamic_typology(summary, k = k)
      assignments <- fit$assignments[fit$assignments$geo_id %in% map_ids,
        c("geo_id", "display_cluster_id")]
      names(assignments)[[2L]] <- "cluster_id"
      composition <- summarize_cluster_composition_by_state(assignments, crosswalk)
      expect_equal(as.numeric(tapply(
        composition$district_fraction, composition$state_id, sum
      )), rep(1, 16L))
    }
  }
  historical <- regionalepi:::.shiny_fit_typology(
    regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary,
    "dissertation", 3L
  )
  assignments <- historical$assignments[
    historical$assignments$geo_id %in% map_ids,
    c("geo_id", "display_cluster_id")
  ]
  names(assignments)[[2L]] <- "cluster_id"
  expect_identical(nrow(
    summarize_cluster_composition_by_state(assignments, crosswalk)
  ), 48L)
})
