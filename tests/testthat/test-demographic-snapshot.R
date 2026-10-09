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
    as.Date(sprintf("%d-12-31", 2017:2025)))
  expect_identical(unname(snapshot$diagnostics$component_row_counts),
                   c(rep(3604L, 4L), 3600L))
  expect_identical(unname(snapshot$diagnostics$geographic_unit_counts),
                   c(rep(401L, 4L), rep(400L, 5L)))
  expect_identical(snapshot$provenance$covered_reporting_years, 2017:2025)
  expect_identical(unname(
    snapshot$diagnostics$annual_average_population_counts), rep(400L, 9L))
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

test_that("reviewed default is immutable snapshot v5 with the v4 predecessor", {
  snapshot <- regionalepi_demographic_snapshot()
  prior <- regionalepi:::.regionalepi_package_data(
    "regionalepi_demographic_snapshot_v4")
  expect_identical(snapshot$provenance$snapshot_id,
    "regionalepi_demography_47bd90242e6c148e")
  expect_identical(snapshot$provenance$content_checksum,
    "47bd90242e6c148e0390d93aae4dc738")
  expect_identical(snapshot$provenance$prior_snapshot_id,
    prior$provenance$snapshot_id)
  expect_identical(snapshot$provenance$inherited_snapshot_checksum,
    prior$provenance$content_checksum)
  for (name in c("mean_age", "youth_dependency", "population", "area")) {
    inherited <- snapshot$data[[name]][
      snapshot$data[[name]]$reference_date != as.Date("2021-12-31"), ,
      drop = FALSE]
    rownames(inherited) <- NULL
    prior_data <- prior$data[[name]]
    rownames(prior_data) <- NULL
    expect_identical(inherited, prior_data)
  }
  expect_identical(snapshot$data$annual_average_population,
    prior$data$annual_average_population)
  for (years in list(2017:2020, 2022:2025)) {
    v5_fit <- fit_dynamic_typology(
      prepare_demographic_snapshot(snapshot, years)$summary, k = 3L)
    v4_fit <- fit_dynamic_typology(
      prepare_demographic_snapshot(prior, years)$summary, k = 3L)
    expect_identical(v5_fit$assignments, v4_fit$assignments)
    expect_identical(v5_fit$profiles, v4_fit$profiles)
  }
})

test_that("snapshot preparation accepts only supported contiguous geography regimes", {
  snapshot <- regionalepi_demographic_snapshot()
  for (years in list(2017L, 2018:2020, 2021L, 2021:2022, 2023:2025)) {
    prepared <- prepare_demographic_snapshot(snapshot, years)
    expect_identical(prepared$summary$diagnostics$reference_years,
      as.integer(years))
    expect_no_error(fit_dynamic_typology(prepared$summary, k = 3L))
  }
  expect_error(prepare_demographic_snapshot(snapshot, 2020:2021),
    "within 2017-2020 or within 2021-2025", fixed = TRUE)
  expect_error(prepare_demographic_snapshot(snapshot, c(2021L, 2023L)),
    "contiguous", fixed = TRUE)
  expect_error(prepare_demographic_snapshot(snapshot, 2016:2019),
    "within 2017-2020 or within 2021-2025", fixed = TRUE)
})

test_that("snapshot v4 preserves v3 and historical observations", {
  current <- regionalepi:::.regionalepi_package_data(
    "regionalepi_demographic_snapshot_v4")
  prior <- get("regionalepi_demographic_snapshot_v3",
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

  current_fit <- fit_dynamic_typology(
    prepare_demographic_snapshot(current, 2022:2025)$summary, k = 3L
  )
  prior_fit <- fit_dynamic_typology(
    prepare_demographic_snapshot(prior, 2022:2025)$summary, k = 3L
  )
  expect_identical(current_fit$assignments, prior_fit$assignments)
  expect_identical(current_fit$profiles, prior_fit$profiles)
})

test_that("immutable predecessor snapshot identities remain unchanged", {
  expected <- c(
    regionalepi_demographic_snapshot_v1 =
      "226cad40cdae697965a9723917477541",
    regionalepi_demographic_snapshot_v2 =
      "9b340e48a4a716826d7615a0e6888012",
    regionalepi_demographic_snapshot_v3 =
      "996c38455f53c59ce7be766c035e2246"
  )
  for (name in names(expected)) {
    snapshot <- get(name, envir = asNamespace("regionalepi"))
    expect_identical(snapshot$provenance$content_checksum, expected[[name]])
    expect_invisible(validate_demographic_snapshot(snapshot))
  }
})

test_that("extended snapshot preserves typology coverage and adds reviewed denominators", {
  prior <- regionalepi:::.regionalepi_package_data(
    "regionalepi_demographic_snapshot_v4")
  reconstructed <- regionalepi:::.snapshot_reconstruct_components(prior)
  annual <- regionalepi:::.demographic_snapshot_average_population(
    prior, 2022:2025
  )
  template <- annual$data[annual$data$year == 2022L, , drop = FALSE]
  historical <- do.call(rbind, lapply(2017:2021, function(year) {
    value <- template
    value$year <- year
    value$population_basis <- "census_2011"
    if (year <= 2020L) {
      eisenach <- value[value$geo_id == "16063", , drop = FALSE]
      eisenach$geo_id <- "16056"
      eisenach$geo_name <- "Eisenach, kreisfreie Stadt"
      eisenach$population <- 42000 + year - 2017L
      value <- rbind(value, eisenach)
    }
    value
  }))
  annual$data <- rbind(historical, annual$data)
  rownames(annual$data) <- NULL
  relations <- data.frame(
    year = 2017:2020, from_geo_id = "16056", to_geo_id = "16063",
    relation_type = "historical_merge", source = "synthetic",
    note = "synthetic reviewed relation", stringsAsFactors = FALSE
  )
  target <- prior$data$population[
    prior$data$population$reference_date == as.Date("2025-12-31"),
    c("geo_id", "geo_name"), drop = FALSE
  ]
  source_counts <- table(annual$data$year)
  source_totals <- tapply(annual$data$population, annual$data$year, sum)
  expect_identical(as.integer(source_counts), c(rep(401L, 4L), rep(400L, 5L)))
  annual <- regionalepi:::.harmonize_reviewed_annual_average_population(
    annual, relations, target
  )
  expect_identical(as.integer(table(annual$data$year)), rep(400L, 9L))
  expect_identical(tapply(annual$data$population, annual$data$year, sum),
                   source_totals)
  expect_false("16056" %in% annual$data$geo_id)
  expect_identical(
    annual$data$population[
      annual$data$year == 2021L & annual$data$geo_id == "16063"
    ],
    template$population[template$geo_id == "16063"]
  )
  annual_sets <- split(annual$data$geo_id, annual$data$year)
  expect_true(all(vapply(annual_sets, setequal, logical(1L), y = target$geo_id)))
  components <- list(
    mean_age = reconstructed$mean_age,
    youth_dependency = reconstructed$youth,
    population = reconstructed$population,
    area = reconstructed$area,
    annual_average_population = annual
  )
  build <- function() regionalepi:::.build_demographic_snapshot(
    components,
    snapshot_version = "synthetic_extended_v4",
    prior_snapshot_id = prior$provenance$snapshot_id
  )
  first <- build()
  second <- build()
  expect_identical(validate_demographic_snapshot(first), first)
  expect_identical(first$provenance$covered_reporting_years, 2017:2025)
  expect_identical(first$provenance$prior_snapshot_id,
                   prior$provenance$snapshot_id)
  expect_identical(first$provenance$content_checksum,
                   second$provenance$content_checksum)
  expect_identical(
    first$provenance$covered_reference_dates,
    prior$provenance$covered_reference_dates
  )
  expect_identical(
    unname(first$diagnostics$annual_average_population_counts),
    rep(400L, 9L)
  )
})

test_that("snapshot v4 identity reflects normalized deterministic content", {
  current <- regionalepi:::.regionalepi_package_data(
    "regionalepi_demographic_snapshot_v4")
  prior <- get("regionalepi_demographic_snapshot_v3",
               envir = asNamespace("regionalepi"))
  expect_identical(current$provenance$content_checksum,
                   "d16ef9d0b03bb97035ef179a964839b8")
  expect_identical(current$provenance$snapshot_id,
                   "regionalepi_demography_d16ef9d0b03bb970")
  expect_identical(current$provenance$prior_snapshot_id,
                   prior$provenance$snapshot_id)
  expect_identical(current$provenance$source_data_status, c(
    mean_age = "06.10.2026 / 23:57:53",
    youth_dependency = "06.10.2026 / 23:58:08",
    population = "06.10.2026 / 23:58:23",
    area = "06.10.2026 / 23:58:37",
    annual_average_population = "08.10.2026 / 13:28:08"
  ))
  expect_true(all(vapply(current$data, function(x) {
    identical(rownames(x), as.character(seq_len(nrow(x))))
  }, logical(1L))))
  expect_invisible(validate_demographic_snapshot(current))
})

snapshot_2021_fixture <- function(offset_minutes = c(0, 3, 6, 9)) {
  prior <- regionalepi:::.regionalepi_package_data(
    "regionalepi_demographic_snapshot_v4")
  old <- regionalepi:::.snapshot_reconstruct_components(prior)
  date <- as.Date("2021-12-31")
  status <- sprintf("09.10.2026 / 10:%02d:00", offset_minutes)
  retrieved <- as.POSIXct("2026-10-09 08:00:00", tz = "UTC") +
    offset_minutes * 60
  census_notes <- paste(
    "Die Berichtsjahre 2011 bis 2021 basieren auf den Ergebnissen des",
    "Zensus vom 09. Mai 2011.")
  indicator <- function(component, status_value, retrieved_value) {
    component$data <- component$data[
      component$data$reference_date == as.Date("2025-12-31"), , drop = FALSE]
    component$data$reference_date <- date
    component$data$population_basis <- "census_2011"
    component$diagnostics$source_quality_markers <- "-"
    component$provenance <- lapply(component$provenance, function(x) {
      x$data_status <- status_value
      x$retrieved_at <- retrieved_value
      x$source_notes <- census_notes
      x
    })
    component
  }
  population <- old$population
  population$data <- population$data[
    population$data$reference_date == as.Date("2025-12-31"), , drop = FALSE]
  population$data$reference_date <- date
  population$data$population_basis <- "census_2011"
  population$data$data_status <- status[[3L]]
  population$data$retrieved_at <- retrieved[[3L]]
  population$diagnostics$source_quality_markers <- "-"
  population$provenance$data_status <- status[[3L]]
  population$provenance$retrieved_at <- retrieved[[3L]]
  population$provenance$source_notes <- census_notes
  area <- old$area
  area$data <- area$data[
    area$data$reference_date == as.Date("2025-12-31"), , drop = FALSE]
  area$data$reference_date <- date
  area$data$data_status <- status[[4L]]
  area$data$retrieved_at <- retrieved[[4L]]
  area$diagnostics$source_quality_markers <- "-"
  area$provenance$data_status <- status[[4L]]
  area$provenance$retrieved_at <- retrieved[[4L]]
  list(
    mean_age = indicator(old$mean_age, status[[1L]], retrieved[[1L]]),
    youth_dependency = indicator(old$youth, status[[2L]], retrieved[[2L]]),
    population = population, area = area
  )
}

test_that("v5 extension adds only reviewed 2021 observations", {
  prior <- regionalepi:::.regionalepi_package_data(
    "regionalepi_demographic_snapshot_v4")
  first <- regionalepi:::.extend_demographic_snapshot_v5(
    prior, snapshot_2021_fixture())
  second <- regionalepi:::.extend_demographic_snapshot_v5(
    prior, snapshot_2021_fixture())
  expect_invisible(validate_demographic_snapshot(first))
  expect_identical(unname(first$diagnostics$component_row_counts),
    c(rep(3604L, 4L), 3600L))
  expect_identical(unname(first$diagnostics$geographic_unit_counts),
    c(rep(401L, 4L), rep(400L, 5L)))
  expect_identical(first$provenance$covered_reference_dates,
    as.Date(sprintf("%d-12-31", 2017:2025)))
  expect_identical(first$provenance$prior_snapshot_id,
    prior$provenance$snapshot_id)
  expect_identical(first$provenance$inherited_snapshot_checksum,
    prior$provenance$content_checksum)
  expect_identical(first$provenance$content_checksum,
    second$provenance$content_checksum)
  observation_provenance <- first$provenance$component_observation_provenance
  expect_identical(unname(vapply(
    observation_provenance[1:4], length, integer(1L))),
    rep(2L, 4L))
  expect_identical(unname(vapply(observation_provenance[1:4], function(x) {
    x[[2L]]$origin
  }, character(1L))), rep("authenticated_2021_extension", 4L))
  expect_true(all(vapply(observation_provenance[1:4], function(x) {
    !identical(x[[1L]]$retrieved_at, x[[2L]]$retrieved_at)
  }, logical(1L))))
  expect_false("16056" %in% first$data$population$geo_id[
    first$data$population$reference_date == as.Date("2021-12-31")])
  expect_identical(sum(first$data$population$geo_id == "16063" &
    first$data$population$reference_date == as.Date("2021-12-31")), 1L)
  for (name in c("mean_age", "youth_dependency", "population", "area")) {
    inherited <- first$data[[name]][
      first$data[[name]]$reference_date != as.Date("2021-12-31"), , drop = FALSE]
    rownames(inherited) <- NULL
    prior_data <- prior$data[[name]]
    rownames(prior_data) <- NULL
    expect_identical(inherited, prior_data)
  }
  expect_identical(first$data$annual_average_population,
    prior$data$annual_average_population)
  for (years in list(2017:2020, 2022:2025)) {
    old_fit <- fit_dynamic_typology(
      prepare_demographic_snapshot(prior, years)$summary, k = 3L)
    new_fit <- fit_dynamic_typology(
      prepare_demographic_snapshot(first, years)$summary, k = 3L)
    expect_identical(new_fit$assignments, old_fit$assignments)
    expect_identical(new_fit$profiles, old_fit$profiles)
  }
})

test_that("v5 2021 gate rejects geography and retrieval defects", {
  prior <- regionalepi:::.regionalepi_package_data(
    "regionalepi_demographic_snapshot_v4")
  missing <- snapshot_2021_fixture()
  missing$mean_age$data <- missing$mean_age$data[-1L, , drop = FALSE]
  expect_error(regionalepi:::.extend_demographic_snapshot_v5(prior, missing),
    "exact 2021 coverage")

  duplicate <- snapshot_2021_fixture()
  duplicate$area$data$geo_id[[2L]] <- duplicate$area$data$geo_id[[1L]]
  expect_error(regionalepi:::.extend_demographic_snapshot_v5(prior, duplicate),
    "exact 2021 coverage|unique")

  eisenach <- snapshot_2021_fixture()
  eisenach$population$data$geo_id[[1L]] <- "16056"
  expect_error(regionalepi:::.extend_demographic_snapshot_v5(prior, eisenach),
    "exact 2021 coverage")

  bad_notes <- snapshot_2021_fixture()
  bad_notes$youth_dependency$provenance <- lapply(
    bad_notes$youth_dependency$provenance, function(x) {
      x$source_notes <- "unrelated note"; x
    })
  expect_error(regionalepi:::.extend_demographic_snapshot_v5(prior, bad_notes),
    "Census 2011 basis")

  bad_name <- snapshot_2021_fixture()
  bad_name$population$data$geo_name[[1L]] <- "Nicht der Registername"
  expect_error(regionalepi:::.extend_demographic_snapshot_v5(prior, bad_name),
    "district names differ")

  bad_marker <- snapshot_2021_fixture()
  bad_marker$area$diagnostics$source_quality_markers <- "X"
  expect_error(regionalepi:::.extend_demographic_snapshot_v5(prior, bad_marker),
    "unexpected quality marker")

  late <- snapshot_2021_fixture(c(0, 3, 6, 20))
  expect_error(regionalepi:::.extend_demographic_snapshot_v5(prior, late),
    "15-minute window")
})

test_that("snapshot annual-average population is reviewed and distinct", {
  snapshot <- regionalepi_demographic_snapshot()
  result <- regionalepi:::.demographic_snapshot_average_population(
    snapshot, 2017:2025
  )
  expect_identical(validate_annual_average_population(result$data), result$data)
  expect_identical(sort(unique(result$data$year)), 2017:2025)
  expect_identical(as.integer(table(result$data$year)), rep(400L, 9L))
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
  expect_identical(provenance$data_status, "08.10.2026 / 13:28:08")
  expect_s3_class(provenance$retrieved_at, "POSIXct")
  expect_match(provenance$copyright,
    "Datenlizenz Deutschland", fixed = TRUE)
})

test_that("snapshot rejects content, status, and geography corruption", {
  snapshot <- regionalepi_demographic_snapshot()
  changed <- snapshot
  changed$data$population$population[[1L]] <-
    changed$data$population$population[[1L]] + 1
  expect_error(validate_demographic_snapshot(changed),
    "does not preserve v4 observations|checksum")

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
  expect_no_error(prepare_demographic_snapshot(snapshot, 2021L))
  expect_error(prepare_demographic_snapshot(snapshot, c(2022L, NA_integer_)),
               "whole values")
  expect_error(prepare_demographic_snapshot(snapshot, c(2022L, 2022L)),
               "unique")
  expect_error(prepare_demographic_snapshot("reviewed_default", 2022:2025),
               "snapshot must contain")

  malformed <- snapshot
  malformed$data$population$population[[1L]] <-
    malformed$data$population$population[[1L]] + 1
  expect_error(prepare_demographic_snapshot(malformed, 2022:2025),
    "does not preserve v4 observations|checksum")
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
