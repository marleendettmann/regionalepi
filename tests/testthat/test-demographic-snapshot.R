test_that("reviewed demographic snapshot contract and resource are complete", {
  snapshot <- regionalepi_demographic_snapshot()
  expect_invisible(validate_demographic_snapshot(snapshot))
  expect_identical(names(snapshot$data),
    c("mean_age", "youth_dependency", "population", "area"))
  expect_identical(snapshot$provenance$source,
                   "Regionaldatenbank Deutschland")
  expect_match(snapshot$provenance$snapshot_id,
               "^regionalepi_demography_[0-9a-f]{16}$")
  expect_match(snapshot$provenance$content_checksum, "^[0-9a-f]{32}$")
  expect_identical(snapshot$provenance$source_status_compatibility,
                   "same_calendar_date_within_15_minutes")
  expect_identical(snapshot$provenance$covered_reference_dates,
    as.Date(c(sprintf("%d-12-31", 2017:2020),
              sprintf("%d-12-31", 2022:2024))))
  expect_identical(unname(snapshot$diagnostics$component_row_counts),
                   rep(2804L, 4L))
  expect_identical(unname(snapshot$diagnostics$geographic_unit_counts),
                   c(rep(401L, 4L), rep(400L, 3L)))
})

test_that("snapshot geography is exact across components and dates", {
  snapshot <- regionalepi_demographic_snapshot()
  for (date in snapshot$provenance$covered_reference_dates) {
    sets <- lapply(snapshot$data, function(data) {
      rows <- data$reference_date == date
      sort(data$geo_id[rows])
    })
    expect_true(all(vapply(sets[-1L], identical, logical(1L), sets[[1L]])))
    expect_true(all(grepl("^[0-9]{5}$", sets[[1L]])))
    expect_true(all(c("02000", "11000") %in% sets[[1L]]))
    if (date <= as.Date("2020-12-31")) {
      expect_true("16056" %in% sets[[1L]])
    } else {
      expect_false("16056" %in% sets[[1L]])
    }
  }
})

test_that("snapshot rejects content, status, and geography corruption", {
  snapshot <- regionalepi_demographic_snapshot()
  changed <- snapshot
  changed$data$population$population[[1L]] <-
    changed$data$population$population[[1L]] + 1
  expect_error(validate_demographic_snapshot(changed), "checksum")

  duplicate <- snapshot
  duplicate$data$area <- rbind(duplicate$data$area,
                               duplicate$data$area[1L, , drop = FALSE])
  expect_error(validate_demographic_snapshot(duplicate), "unique")

  missing <- snapshot
  missing$data$mean_age <- missing$data$mean_age[-1L, , drop = FALSE]
  expect_error(validate_demographic_snapshot(missing),
               "diagnostics|sets must match")

  status <- snapshot
  status$provenance$source_data_status[["area"]] <- "02.09.2026 / 11:09:13"
  expect_error(validate_demographic_snapshot(status), "one calendar date")
})

test_that("snapshot accessor is explicit, network-free, and non-mutating", {
  testthat::local_mocked_bindings(
    .regional_table_transport = function(...) stop("network used"),
    .regional_population_transport = function(...) stop("network used"),
    .package = "regionalepi"
  )
  first <- regionalepi_demographic_snapshot()
  second <- regionalepi_demographic_snapshot("reviewed_default")
  expect_identical(first, second)
  first$data$population$population[[1L]] <- -1
  expect_gt(second$data$population$population[[1L]], 0)
  expect_error(regionalepi_demographic_snapshot("latest"), "unsupported")
})

test_that("snapshot feeds the existing demographic analytical pipeline", {
  snapshot <- regionalepi_demographic_snapshot()
  current <- regionalepi:::.demographic_snapshot_period(snapshot, 2022:2024)
  historical <- regionalepi:::.demographic_snapshot_period(snapshot, 2017:2020)
  expect_identical(nrow(current$annual), 3600L)
  expect_identical(nrow(current$summary$data), 1200L)
  expect_identical(nrow(historical$annual), 4812L)
  expect_identical(nrow(historical$summary$data), 1203L)
  expect_identical(sort(unique(current$annual$indicator_id)), c(
    "mean_age", "population_density", "youth_dependency_ratio"))
  current_fit <- fit_dynamic_typology(current$summary, k = 3L)
  historical_fit <- fit_dynamic_typology(historical$summary, k = 3L)
  expect_identical(nrow(current_fit$assignments), 400L)
  expect_identical(nrow(historical_fit$assignments), 401L)
  expect_identical(current$source_mode, "snapshot")
})
