test_that("Shiny PoC defaults and reviewed period choices are stable", {
  periods <- regionalepi:::.shiny_demographic_periods()
  expect_identical(names(periods), c("2022–2024", "2017–2020"))
  expect_identical(periods[[1L]], 2022:2024)

  influenza <- regionalepi:::.shiny_period_choices("Influenza, saisonal")
  expect_false(any(grepl("2020/21", names(influenza), fixed = TRUE)))
  expect_identical(sum(grepl("2022/23", names(influenza), fixed = TRUE)), 2L)
  expect_true(all(c("Welle 1", "Welle 2") %in%
                    sub(".*– ", "", names(influenza)[grepl("2022/23", names(influenza))])))

  covid <- regionalepi:::.shiny_period_choices("COVID-19")
  expect_identical(length(covid), 7L)
  expect_true(all(grepl("covid_wave_", unname(covid), fixed = TRUE)))
})

test_that("Shiny cache keys respect reactive source boundaries", {
  demographic <- regionalepi:::.shiny_demographic_cache_key("2022–2024")
  expect_identical(demographic, "demography:2022-2023-2024")
  expect_identical(
    regionalepi:::.shiny_demographic_cache_key("2022–2024"), demographic
  )
  expect_false(grepl("Influenza|COVID|k", demographic))

  surveillance <- regionalepi:::.shiny_surveillance_cache_key(
    "Influenza, saisonal", c(2023, 2022)
  )
  expect_identical(
    surveillance,
    regionalepi:::.shiny_surveillance_cache_key("Influenza, saisonal", 2022:2023)
  )
  expect_false(grepl("2017–2020|2022–2024|k", surveillance))

  first <- regionalepi:::.shiny_summary_cache_key(surveillance, "wave-1", "fit-a")
  second <- regionalepi:::.shiny_summary_cache_key(surveillance, "wave-2", "fit-a")
  expect_false(identical(first, second))
})

test_that("map join is identifier-only and handles reviewed historical difference", {
  map <- regionalepi_map_geometry()
  assignments <- data.frame(
    fit_id = "fit", geo_id = map$features$geo_id, raw_cluster = 1L,
    display_cluster_id = "C01", stringsAsFactors = FALSE
  )
  fit <- list(
    assignments = assignments,
    provenance = list(fit_id = "fit"),
    indicator_set = demographic_structure_spec()
  )
  joined <- regionalepi:::.shiny_map_assignments(map, fit)
  expect_identical(nrow(joined$data), 400L)
  expect_identical(joined$data$geo_id, map$features$geo_id)
  expect_length(joined$typology_only_geo_ids, 0L)

  fit$assignments <- rbind(assignments, data.frame(
    fit_id = "fit", geo_id = "16056", raw_cluster = 1L,
    display_cluster_id = "C01", stringsAsFactors = FALSE
  ))
  historical <- regionalepi:::.shiny_map_assignments(map, fit)
  expect_identical(historical$typology_only_geo_ids, "16056")
  compatibility <- regionalepi:::.shiny_dynamic_compatibility(
    fit, map$features$geo_id
  )
  expect_identical(compatibility$expected_typology_only_geo_ids, "16056")
  expect_identical(compatibility$expected_surveillance_count, 400L)
})

test_that("launcher finds the packaged app and dependency failures are clear", {
  expect_identical(regionalepi:::.shiny_missing_dependencies(), character())
  fake <- function(package, quietly = FALSE) package != "leaflet"
  expect_identical(regionalepi:::.shiny_missing_dependencies(fake), "leaflet")

  directory <- regionalepi:::.shiny_app_dir()
  expect_true(nzchar(directory))
  expect_true(file.exists(file.path(directory, "app.R")))
  ui_environment <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(directory, "ui.R"), envir = ui_environment)
  expect_identical(
    unname(ui_environment$initial_period_choices[[length(
      ui_environment$initial_period_choices
    )]]),
    "influenza_2025_26_1"
  )
  expect_match(paste(deparse(body(run_regionalepi_app)), collapse = "\n"),
               "optional package")
})

test_that("server error callback resolves its formatter from the package namespace", {
  skip_if_not_installed("shiny")
  app <- regionalepi:::.shiny_app_dir()
  server_environment <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(app, "server.R"), envir = server_environment)

  testthat::local_mocked_bindings(
    .shiny_fetch_demography = function(years) {
      stop("synthetic Regionaldatenbank failure", call. = FALSE)
    },
    .package = "regionalepi"
  )
  shiny::testServer(server_environment$server, {
    session$setInputs(
      pathogen = "Influenza, saisonal",
      period_id = "influenza_2023_24_1",
      demographic_period = "2022–2024", typology_mode = "dynamic",
      k = "3", load_analysis = 1
    )
    session$flushReact()
    status <- paste(as.character(output$load_status), collapse = "")
    expect_match(status, "Die Live-Analyse konnte nicht geladen werden", fixed = TRUE)
    expect_match(status, "synthetic Regionaldatenbank failure", fixed = TRUE)
    expect_false(grepl("app_format_error", status, fixed = TRUE))
  })
})

test_that("installed app files do not rely on unqualified app helpers", {
  app <- regionalepi:::.shiny_app_dir()
  files <- file.path(app, c("app.R", "server.R", "ui.R"))
  expect_true(all(file.exists(files)))
  text <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
  unqualified <- gregexpr("(?<![:.])\\bapp_[A-Za-z0-9_]+", text, perl = TRUE)
  matches <- regmatches(text, unqualified)[[1L]]
  expect_identical(matches, character())
  expect_false(file.exists(file.path(app, "helpers.R")))
})

test_that("Shiny provenance formats POSIXct source statuses safely", {
  status <- as.POSIXct("2026-08-28 08:20:20", tz = "UTC")
  source <- list(population = list(
    diagnostics = list(data_status = status), provenance = list()
  ))
  queries <- list(
    kreis = list(data_status = status), berlin = list(data_status = status)
  )
  expect_identical(
    regionalepi:::.shiny_app_source_status(source),
    "2026-08-28 08:20:20"
  )
  expect_identical(
    unname(regionalepi:::.shiny_app_query_status(queries)),
    rep("2026-08-28 08:20:20", 2L)
  )
})

test_that("map and fitting provenance required by the PoC are complete", {
  map <- regionalepi_map_geometry()
  expect_true(all(c(
    "source_organization", "source_product", "source_vintage", "license",
    "license_url", "attribution", "change_notice"
  ) %in% names(map$provenance)))
  expect_true(nzchar(map$provenance$attribution))

  indicator_set <- demographic_structure_spec()
  fitting <- dynamic_kmeans_spec()
  expect_identical(indicator_set$indicator_set_id, "demographic_structure_v1")
  expect_identical(fitting$fitting_specification_id, "dynamic_kmeans_v1")
  expect_identical(fitting$seed, 20241231L)
})

test_that("Shiny period orchestration removes explicitly unassigned rows", {
  source("helper-examples.R", local = TRUE)
  surveillance <- surveillance_incidence_example()[rep(1L, 4L), , drop = FALSE]
  surveillance$geo_id <- rep(c("01001", "02000"), each = 2L)
  surveillance$geo_name <- rep(c("Flensburg", "Hamburg"), each = 2L)
  surveillance$geo_level <- "district"
  surveillance$date <- rep(as.Date(c("2024-01-01", "2024-02-05")), 2L)
  surveillance$pathogen <- "Influenza, saisonal"
  surveillance$source <- "SurvStat@RKI"
  surveillance$incidence <- c(1, 2, 3, 4)
  surveillance$reporting_year <- 2024L
  surveillance$reporting_week <- rep(c(1L, 6L), 2L)
  surveillance$query_id <- "synthetic"
  periods <- data.frame(
    period_set_id = "test", period_id = "selected", pathogen = "Influenza, saisonal",
    season_id = "test", period_type = "influenza_wave", label = "Test",
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-01-07"),
    definition_version = "test_v1", source_reference = "synthetic",
    review_status = "reviewed", note = NA_character_, stringsAsFactors = FALSE
  )
  fit <- list(
    assignments = data.frame(
      geo_id = c("01001", "02000"), display_cluster_id = c("C01", "C02"),
      stringsAsFactors = FALSE
    ),
    provenance = list(fit_id = "fit"),
    indicator_set = demographic_structure_spec()
  )
  selected <- list(resource = periods, row = periods)
  result <- regionalepi:::.shiny_analyse_period(
    list(data = surveillance), selected, fit
  )
  expect_false(anyNA(result$attached$data$geo_id))
  expect_true(all(result$attached$data$period_id == "selected"))
  expect_identical(nrow(result$attached$data), 2L)
})
