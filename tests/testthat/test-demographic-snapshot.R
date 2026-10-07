test_that("reviewed demographic snapshot contract and resource are complete", {
  snapshot <- regionalepi_demographic_snapshot()
  expect_invisible(validate_demographic_snapshot(snapshot))
  expect_identical(names(snapshot$data),
    c("mean_age", "youth_dependency", "population", "area",
      "annual_average_population"))
  expect_identical(snapshot$provenance$source,
                   "Regionaldatenbank Deutschland")
  expect_match(snapshot$provenance$snapshot_id,
               "^regionalepi_demography_[0-9a-f]{16}$")
  expect_match(snapshot$provenance$content_checksum, "^[0-9a-f]{32}$")
  expect_identical(snapshot$provenance$source_status_compatibility,
    paste0("typology_components_same_calendar_date_within_15_minutes;",
           "annual_average_population_independently_reviewed"))
  expect_identical(snapshot$provenance$covered_reference_dates,
    as.Date(c(sprintf("%d-12-31", 2017:2020),
              sprintf("%d-12-31", 2022:2025))))
  expect_identical(unname(snapshot$diagnostics$component_row_counts),
                   c(rep(3204L, 4L), 1600L))
  expect_identical(unname(snapshot$diagnostics$geographic_unit_counts),
                   c(rep(401L, 4L), rep(400L, 4L)))
  expect_identical(snapshot$provenance$covered_reporting_years, 2022:2025)
  expect_identical(unname(
    snapshot$diagnostics$annual_average_population_counts), rep(400L, 4L))
})

test_that("snapshot geography is exact across components and dates", {
  snapshot <- regionalepi_demographic_snapshot()
  for (date in snapshot$provenance$covered_reference_dates) {
    sets <- lapply(snapshot$data[c(
      "mean_age", "youth_dependency", "population", "area")], function(data) {
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

test_that("snapshot v3 preserves its predecessor and historical observations", {
  current <- regionalepi_demographic_snapshot()
  prior <- get("regionalepi_demographic_snapshot_v2",
               envir = asNamespace("regionalepi"))
  expect_invisible(validate_demographic_snapshot(prior))
  expect_identical(current$provenance$prior_snapshot_id,
                   prior$provenance$snapshot_id)
  historical_dates <- as.Date(sprintf("%d-12-31", 2017:2020))
  for (component in c("mean_age", "youth_dependency", "population", "area")) {
    current_rows <- current$data[[component]]$reference_date %in%
      historical_dates
    prior_rows <- prior$data[[component]]$reference_date %in%
      historical_dates
    expect_identical(current$data[[component]][current_rows, , drop = FALSE],
                     prior$data[[component]][prior_rows, , drop = FALSE])
  }
})

test_that("snapshot v3 identity reflects normalized deterministic content", {
  current <- regionalepi_demographic_snapshot()
  prior <- get("regionalepi_demographic_snapshot_v2",
               envir = asNamespace("regionalepi"))
  expect_identical(current$provenance$content_checksum,
                   "996c38455f53c59ce7be766c035e2246")
  expect_identical(current$provenance$snapshot_id,
                   "regionalepi_demography_996c38455f53c59c")
  expect_identical(current$provenance$prior_snapshot_id,
                   prior$provenance$snapshot_id)
  expect_identical(current$provenance$source_data_status, c(
    mean_age = "06.10.2026 / 23:57:53",
    youth_dependency = "06.10.2026 / 23:58:08",
    population = "06.10.2026 / 23:58:23",
    area = "06.10.2026 / 23:58:37",
    annual_average_population = "06.10.2026 / 23:58:49"
  ))
  expect_true(all(vapply(current$data, function(x) {
    identical(rownames(x), as.character(seq_len(nrow(x))))
  }, logical(1L))))
  expect_invisible(validate_demographic_snapshot(current))
})

test_that("snapshot annual-average population is reviewed and distinct", {
  snapshot <- regionalepi_demographic_snapshot()
  result <- regionalepi:::.demographic_snapshot_average_population(
    snapshot, 2022:2025
  )
  expect_identical(validate_annual_average_population(result$data), result$data)
  expect_identical(sort(unique(result$data$year)), 2022:2025)
  expect_identical(as.integer(table(result$data$year)), rep(400L, 4L))
  expect_true(all(result$data$source_table == "12411-05-01-4"))
  expect_true(all(result$data$population_measure ==
    "annual_average_population"))
  expect_true(all(result$data$population_reference ==
    "reporting_year_annual_average"))
  expect_identical(result$diagnostics$source_measure, "BEV028")
  expect_identical(result$diagnostics$source_mode, "snapshot")
  expect_identical(result$diagnostics$snapshot_id,
    snapshot$provenance$snapshot_id)
  year_end <- snapshot$data$population[
    snapshot$data$population$reference_date == as.Date("2022-12-31"), ]
  average_2022 <- result$data[result$data$year == 2022L, ]
  expect_true(any(average_2022$population != year_end$population[
    match(average_2022$geo_id, year_end$geo_id)]))
  provenance <- result$provenance[[
    "regional_average_population_12411-05-01-4_bev028"]]
  expect_identical(provenance$source_measure, "BEV028")
  expect_identical(provenance$data_status, "06.10.2026 / 23:58:49")
  expect_s3_class(provenance$retrieved_at, "POSIXct")
  expect_match(provenance$copyright,
    "Datenlizenz Deutschland", fixed = TRUE)
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
  current <- regionalepi:::.demographic_snapshot_period(snapshot, 2022:2025)
  historical <- regionalepi:::.demographic_snapshot_period(snapshot, 2017:2020)
  expect_identical(nrow(current$annual), 4800L)
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

test_that("public snapshot preparation exactly preserves the reviewed bundle", {
  snapshot <- regionalepi_demographic_snapshot()
  for (years in list(2022:2025, 2017:2020)) {
    public <- prepare_demographic_snapshot(snapshot, years)
    internal <- regionalepi:::.demographic_snapshot_period(snapshot, years)
    expect_identical(public, internal)
    expect_identical(names(public), c(
      "annual", "summary", "source", "source_mode", "snapshot_provenance"
    ))
    expect_identical(names(public$source), c(
      "population", "area", "mean_age", "youth_dependency"
    ))
    expect_identical(public$source_mode, "snapshot")
    expect_identical(public$snapshot_provenance, snapshot$provenance)
    expect_identical(public$snapshot_provenance$snapshot_id,
                     snapshot$provenance$snapshot_id)
    expect_identical(public$snapshot_provenance$source_data_status,
                     snapshot$provenance$source_data_status)
    source_status <- vapply(public$source, function(x) {
      x$diagnostics$data_status
    }, character(1L))
    expect_identical(unname(source_status), unname(
      snapshot$provenance$source_data_status[names(source_status)]
    ))
    expect_identical(
      unname(vapply(public$source, function(x) {
        x$diagnostics$snapshot_id
      }, character(1L))),
      rep(snapshot$provenance$snapshot_id, 4L)
    )
  }
})

test_that("public snapshot preparation preserves dimensions and indicators", {
  snapshot <- regionalepi_demographic_snapshot()
  current <- prepare_demographic_snapshot(snapshot, 2022:2025)
  historical <- prepare_demographic_snapshot(snapshot, 2017:2020)

  expect_identical(nrow(current$annual), 4800L)
  expect_identical(nrow(current$summary$data), 1200L)
  expect_identical(nrow(historical$annual), 4812L)
  expect_identical(nrow(historical$summary$data), 1203L)

  observed <- unique(current$annual[c(
    "indicator_id", "definition_version", "indicator_unit"
  )])
  observed <- observed[order(observed$indicator_id), , drop = FALSE]
  expected <- do.call(rbind, lapply(
    demographic_structure_spec()$indicators,
    function(x) data.frame(
      indicator_id = x$indicator_id,
      definition_version = x$definition_version,
      indicator_unit = x$indicator_unit,
      stringsAsFactors = FALSE
    )
  ))
  expected <- expected[order(expected$indicator_id), , drop = FALSE]
  rownames(observed) <- rownames(expected) <- NULL
  expect_identical(observed, expected)
})

test_that("public snapshot preparation retains strict existing failures", {
  snapshot <- regionalepi_demographic_snapshot()
  expect_error(prepare_demographic_snapshot(snapshot, 2021L), "not covered")
  expect_error(prepare_demographic_snapshot(snapshot, c(2022L, NA_integer_)),
               "not covered")
  expect_error(prepare_demographic_snapshot(snapshot, c(2022L, 2022L)),
               "unique")
  expect_error(prepare_demographic_snapshot("reviewed_default", 2022:2025),
               "snapshot must contain")

  malformed <- snapshot
  malformed$data$population$population[[1L]] <-
    malformed$data$population$population[[1L]] + 1
  expect_error(prepare_demographic_snapshot(malformed, 2022:2025), "checksum")
})

test_that("public snapshot preparation leaves dynamic fits unchanged", {
  snapshot <- regionalepi_demographic_snapshot()
  public <- prepare_demographic_snapshot(snapshot, 2022:2025)
  internal <- regionalepi:::.demographic_snapshot_period(snapshot, 2022:2025)
  fit_public <- fit_dynamic_typology(public$summary, k = 3L)
  fit_internal <- fit_dynamic_typology(internal$summary, k = 3L)

  expect_identical(fit_public$provenance$fit_id,
                   fit_internal$provenance$fit_id)
  for (field in c(
    "assignments", "profiles", "cluster_diagnostics", "matrix",
    "diagnostics", "provenance", "indicator_set", "fitting_specification"
  )) expect_identical(fit_public[[field]], fit_internal[[field]])
})
