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

  index <- list(
    broad = list(key = "broad", pathogen = "COVID-19",
                 reporting_years = 2020:2022),
    narrow = list(key = "narrow", pathogen = "COVID-19",
                  reporting_years = 2021:2022)
  )
  expect_identical(regionalepi:::.shiny_surveillance_cache_match(
    index, "COVID-19", 2021
  ), "narrow")
  expect_null(regionalepi:::.shiny_surveillance_cache_match(
    index, "Influenza, saisonal", 2021
  ))
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
    expect_match(status, "demographischen Daten konnten nicht geladen werden",
                 fixed = TRUE)
    expect_false(grepl("synthetic Regionaldatenbank failure", status, fixed = TRUE))
    expect_false(grepl("app_format_error", status, fixed = TRUE))
    expect_false(state$busy)
    expect_match(state$technical_error, "synthetic Regionaldatenbank failure",
                 fixed = TRUE)
  })
})

test_that("Shiny user errors hide authentication and source internals", {
  missing <- simpleError(paste(
    "Regionaldatenbank retrieval failed: Invalid Regionaldatenbank",
    "authentication: required environment variables are missing or empty"
  ))
  text <- regionalepi:::.shiny_app_format_error(missing, "Die Live-Analyse")
  expect_match(text, "fehlen die Zugangsdaten", fixed = TRUE)
  expect_false(grepl("environment|authentication", text, ignore.case = TRUE))

  survstat <- regionalepi:::.shiny_app_format_error(
    simpleError("SurvStat@RKI retrieval failed: <SOAP secret>"),
    "Die Live-Analyse"
  )
  expect_match(survstat, "SurvStat-Daten konnten nicht geladen werden",
               fixed = TRUE)
  expect_false(grepl("SOAP secret", survstat, fixed = TRUE))
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

  bounds <- regionalepi:::.shiny_app_map_bounds(map)
  expect_named(bounds, c("lng1", "lat1", "lng2", "lat2"))
  expect_true(all(is.finite(bounds)))
  expect_lt(bounds[["lng1"]], bounds[["lng2"]])
  expect_lt(bounds[["lat1"]], bounds[["lat2"]])

  assignments <- data.frame(
    geo_id = map$features$geo_id,
    display_cluster_id = rep("C01", nrow(map$features)),
    stringsAsFactors = FALSE
  )
  widget <- regionalepi:::.shiny_app_leaflet_map(map, assignments)
  calls <- vapply(widget$x$calls, `[[`, character(1L), "method")
  expect_false("addProviderTiles" %in% calls)
  expect_length(widget$x$fitBounds, 5L)
  expect_equal(unlist(widget$x$fitBounds[1:4]),
    unname(bounds[c("lat1", "lng1", "lat2", "lng2")]))
})

test_that("Shiny indicator presentation uses German labels and units", {
  display <- regionalepi:::.shiny_app_indicator_display()
  expect_identical(display$indicator_id, c(
    "population_density", "mean_age", "youth_dependency_ratio"
  ))
  expect_identical(display$label, c(
    "Bevölkerungsdichte", "Durchschnittsalter", "Jugendquotient"
  ))
  expect_identical(display$unit, c(
    "Einwohner je km²", "Jahre",
    "Unter-20-Jährige je 100 Personen im Alter 20–64"
  ))
})

test_that("Shiny app exposes staged progress and caches fitted map widgets", {
  app <- regionalepi:::.shiny_app_dir()
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE),
                       collapse = "\n")
  ui_text <- paste(readLines(file.path(app, "ui.R"), warn = FALSE),
                   collapse = "\n")
  expect_match(server_text, "Demographische Regionaldaten werden geladen",
               fixed = TRUE)
  expect_match(server_text, "SurvStat-Kreisdaten werden geladen", fixed = TRUE)
  expect_match(server_text, "Kartendarstellung wird vorbereitet", fixed = TRUE)
  expect_match(server_text, "map-widget:", fixed = TRUE)
  expect_match(server_text, ".shiny_surveillance_cache_match", fixed = TRUE)
  expect_match(ui_text, "Erweiterte Einstellungen", fixed = TRUE)
  expect_match(ui_text,
    "Regionale Infektionsepidemiologie im demographischen Kontext",
    fixed = TRUE)
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
