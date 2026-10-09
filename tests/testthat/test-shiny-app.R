test_that("Shiny PoC defaults and reviewed period choices are stable", {
  periods <- regionalepi:::.shiny_demographic_periods()
  expect_identical(names(periods), c("2022–2025", "2017–2020"))
  expect_identical(periods[[1L]], 2022:2025)

  influenza <- regionalepi:::.shiny_period_choices("Influenza, saisonal")
  expect_named(influenza, "AGI-Influenzawellen")
  influenza_flat <- influenza[[1L]]
  expect_false(any(grepl("2020/21", names(influenza_flat), fixed = TRUE)))
  expect_identical(sum(grepl("2022/23", names(influenza_flat), fixed = TRUE)), 2L)

  covid <- regionalepi:::.shiny_period_choices("COVID-19")
  expect_identical(names(covid), c(
    "Retrospektive Phaseneinteilung der Pandemie (RKI)", "Definierte COVID-19-Wellen (RKI)"))
  expect_identical(length(covid[[1L]]), 7L)
  expect_identical(length(covid[[2L]]), 2L)
  registry <- regionalepi:::.shiny_pathogen_registry()
  expect_identical(names(registry), c(
    "Influenza, saisonal", "COVID-19", "Norovirus-Gastroenteritis"
  ))
  expect_identical(
    registry[["Norovirus-Gastroenteritis"]]$survstat_member,
    "Norovirus-Gastroenteritis"
  )
  expect_length(registry[["Norovirus-Gastroenteritis"]]$period_factories, 0L)
})

test_that("demographic reference selection supports reviewed custom regimes", {
  current <- regionalepi:::.shiny_demographic_reference_selection("2022–2025")
  expect_identical(current$years, 2022:2025)
  expect_identical(current$selection_type, "preset")
  expect_identical(current$configuration_type, "preset")
  expect_identical(current$solution_review_status, "reviewed_reference")
  expect_false(current$exploratory)

  annual <- regionalepi:::.shiny_demographic_reference_selection(
    "Benutzerdefinierter Referenzzeitraum", 2021L, 2021L)
  expect_identical(annual$years, 2021L)
  expect_true(annual$exploratory)
  expect_true(annual$annual_typology)
  expect_identical(annual$configuration_type, "custom")
  expect_identical(annual$solution_review_status,
    "not_individually_reviewed")
  expect_identical(annual$geography_regime, "current_400_districts")

  mixed_basis <- regionalepi:::.shiny_demographic_reference_selection(
    "Benutzerdefinierter Referenzzeitraum", 2021L, 2022L)
  expect_true(mixed_basis$census_comparability_notice)
  expect_identical(mixed_basis$census_bases,
    c("census_2011", "census_2022"))

  historical <- regionalepi:::.shiny_demographic_reference_selection(
    "Benutzerdefinierter Referenzzeitraum", 2018L, 2020L)
  expect_identical(historical$years, 2018:2020)
  expect_identical(historical$geography_regime,
    "historical_401_districts")
  expect_error(regionalepi:::.shiny_demographic_reference_selection(
    "Benutzerdefinierter Referenzzeitraum", 2020L, 2021L),
    "within 2017-2020")

  reconciled <- regionalepi:::.shiny_reconcile_custom_demographic_years(
    2020L, 2025L)
  expect_identical(reconciled$end_year, 2020L)
  expect_identical(reconciled$end_choices, 2020L)
})

test_that("historical pandemic frame exposes all reviewed dissertation periods", {
  window <- regionalepi:::.shiny_selected_window(
    "COVID-19", "covid19_pandemic_2020_22")
  periods <- regionalepi:::.shiny_periods_in_window(window)
  dissertation <- periods[periods$period_system ==
    "Retrospektive Phaseneinteilung der Pandemie (RKI)", ]
  expect_identical(nrow(dissertation), 7L)
  expect_identical(dissertation$period_id, paste0("covid_wave_", c(
    "1", "2", "3", "4a", "4b", "5a", "5b")))
  expect_true(all(dissertation$start_date >= window$start_date))
  expect_true(all(dissertation$end_date <= window$end_date))
  choices <- regionalepi:::.shiny_period_choices_in_window(window)
  expect_identical(names(choices), c(
    "Retrospektive Phaseneinteilung der Pandemie (RKI)", "Definierte COVID-19-Wellen (RKI)")[1L])
  expect_identical(length(choices[[1L]]), 7L)
  for (mode in c("dissertation", "dynamic")) {
    expect_identical(regionalepi:::.shiny_periods_in_window(window)$period_id,
      periods$period_id)
  }
})

test_that("updated typology supports reviewed historical Influenza windows", {
  ids <- paste0("influenza_", 2017:2021, "_", 18:22)
  for (id in ids) {
    window <- regionalepi:::.shiny_selected_window("Influenza, saisonal", id)
    years <- seq.int(
      as.integer(format(window$start_date, "%Y")),
      as.integer(format(window$end_date, "%Y"))
    )
    population <- regionalepi:::.shiny_population_for_reporting_years(
      years, "snapshot"
    )
    expect_identical(population$supported_years, years)
    expect_length(population$unsupported_years, 0L)
    expect_length(population$provisional, 0L)
  }
  demography <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)
  expect_identical(demography$summary$diagnostics$reference_years, 2022:2025)
})

test_that("period context is metadata driven and makes typology explicit", {
  window <- regionalepi:::.shiny_selected_window(
    "COVID-19", "covid19_pandemic_2020_22")
  period <- regionalepi:::.shiny_periods_in_window(window)
  period <- period[period$period_id == "covid_wave_3", ]
  range <- regionalepi:::.shiny_select_analysis_range(window, "reviewed", period)
  dynamic <- regionalepi:::.shiny_period_context(
    "COVID-19", range, period, "dynamic", 2017:2020, 4L)
  expect_identical(dynamic$title, "COVID-19 · 3. Welle / Alpha")
  expect_identical(dynamic$subtitle,
    "01.03.2021–13.06.2021 · 2021-KW09–KW23")
  expect_identical(dynamic$typology,
    "Typologie: Aktualisiert · Referenzzeitraum 2017–2020 · k=4")
  reference <- regionalepi:::.shiny_period_context(
    "COVID-19", range, period, "dissertation", 2017:2020, 3L)
  expect_identical(reference$typology,
    "Typologie: Historische Referenztypologie (2017–2020)")
})

test_that("loaded analysis context preserves typology and incidence semantics", {
  result <- list(
    typology_mode = "dynamic",
    display_metadata = data.frame(
      display_cluster_id = c("C01", "C02", "C03", "C04")
    ),
    bundle = list(diagnostics = list(status_by_reporting_year = data.frame(
      reporting_year = 2026L, population_year = 2025L,
      incidence_status = "provisional"
    ))),
    loaded_selection = list(
      typology_mode = "dynamic", demographic_period = "2022–2025",
      k = 4L, demographic_source = "snapshot"
    )
  )
  expect_identical(
    regionalepi:::.shiny_loaded_analysis_context(result),
    paste0("Demografische Typologie · 2022–2025 · k=4 · ",
      "Inzidenz auf Basis der durchschnittlichen Jahresbevölkerung")
  )
  expect_true(regionalepi:::.shiny_loaded_analysis_is_provisional(result))
  expect_false(regionalepi:::.shiny_loaded_selection_changed(
    result, "dynamic", "2022–2025", "4", NULL
  ))
  expect_true(regionalepi:::.shiny_loaded_selection_changed(
    result, "dynamic", "2022–2025", "3", "snapshot"
  ))
  expect_true(regionalepi:::.shiny_loaded_selection_changed(
    result, "dynamic", "2017–2020", "4", "snapshot"
  ))
  result$loaded_selection$demographic_years <- 2022:2025
  expect_true(regionalepi:::.shiny_loaded_selection_changed(
    result, "dynamic", "2022–2025", "4", "snapshot", 2021:2025
  ))
  expect_true(regionalepi:::.shiny_loaded_selection_changed(
    result, "dissertation", "2022–2025", "4", "snapshot"
  ))
  expect_true(regionalepi:::.shiny_loaded_selection_changed(
    result, "dynamic", "2022–2025", "4", "live"
  ))

  result$typology_mode <- "dissertation"
  result$display_metadata$display_cluster_id <- c("ClD", "ClJ", "ClA", NA)
  result$display_metadata <- result$display_metadata[1:3, , drop = FALSE]
  expect_identical(
    regionalepi:::.shiny_loaded_analysis_context(result),
    paste0(
      "Historische Referenztypologie (ClD / ClJ / ClA) · ",
      "Historische SurvStat-Inzidenz"
    )
  )
  result$bundle$diagnostics$status_by_reporting_year$incidence_status <- "final"
  expect_false(regionalepi:::.shiny_loaded_analysis_is_provisional(result))
})

test_that("Shiny cache keys respect reactive source boundaries", {
  demographic <- regionalepi:::.shiny_demographic_cache_key("2022–2025")
  expect_identical(demographic, "demography:snapshot:2022-2023-2024-2025")
  expect_identical(
    regionalepi:::.shiny_demographic_cache_key("2022–2025"), demographic
  )
  expect_false(grepl("Influenza|COVID|k", demographic))
  expect_false(identical(demographic,
    regionalepi:::.shiny_demographic_cache_key("2022–2025", "live")))

  surveillance <- regionalepi:::.shiny_surveillance_cache_key(
    "Influenza, saisonal", c(2023, 2022)
  )
  expect_identical(
    surveillance,
    regionalepi:::.shiny_surveillance_cache_key("Influenza, saisonal", 2022:2023)
  )
  expect_false(grepl("2017–2020|2022–2025|k", surveillance))

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
  bundle <- regionalepi:::.shiny_surveillance_bundle_cache_key(
    "Influenza, saisonal", 2025:2026)
  expect_match(bundle, "source_survstat_incidence", fixed = TRUE)
  expect_match(bundle, "reference-definition", fixed = TRUE)
  derived_bundle <- regionalepi:::.shiny_surveillance_bundle_cache_key(
    "Influenza, saisonal", 2025:2026,
    incidence_definition = "annual_average_population_v1",
    surveillance_data_status = "survstat-status",
    population_data_status = "population-status",
    population_source_mode = "live", incidence_status = "provisional",
    provisional_denominator_year = 2025L
  )
  expect_false(identical(bundle, derived_bundle))
  expect_match(derived_bundle, "population-status", fixed = TRUE)
  expect_match(derived_bundle, "provisional:2025", fixed = TRUE)
  snapshot_bundle <- regionalepi:::.shiny_surveillance_bundle_cache_key(
    "Influenza, saisonal", 2025:2026,
    incidence_definition = "annual_average_population_v1",
    surveillance_data_status = "survstat-status",
    population_data_status = "population-status",
    population_source_mode = "snapshot", incidence_status = "provisional",
    provisional_denominator_year = 2025L
  )
  expect_false(identical(snapshot_bundle, derived_bundle))
  expect_match(snapshot_bundle, "snapshot", fixed = TRUE)
  index$derived <- list(
    key = derived_bundle, pathogen = "COVID-19", reporting_years = 2021:2022,
    incidence_definition = "annual_average_population_v1",
    population_source_mode = "live"
  )
  expect_identical(regionalepi:::.shiny_surveillance_cache_match(
    index, "COVID-19", 2022, "annual_average_population_v1", "live"
  ), derived_bundle)
  expect_identical(regionalepi:::.shiny_surveillance_cache_match(
    index, "COVID-19", 2022, "source_survstat_incidence"
  ), "narrow")
})

test_that("normal Shiny assembly uses annual-average population with explicit 2026 status", {
  population_years <- NULL
  provisional <- NULL
  source <- list(timings = numeric(), resolution = list())
  testthat::local_mocked_bindings(
    .shiny_fetch_surveillance_bundle = function(...) source,
    fetch_regional_average_population = function(years, ...) {
      population_years <<- years
      list(data = data.frame(year = 2025L), provenance = list())
    },
    prepare_analysis_incidence = function(surveillance,
                                          annual_average_population,
                                          provisional_denominator_years) {
      provisional <<- provisional_denominator_years
      list(data = data.frame(), diagnostics = list(status = "provisional"),
           provenance = list(), source_incidence = surveillance)
    },
    .shiny_regional_credentials_available = function() TRUE,
    .package = "regionalepi"
  )
  result <- regionalepi:::.shiny_fetch_analysis_bundle(
    "Influenza, saisonal", 2025:2026, list(), "live"
  )
  expect_identical(population_years, 2024:2026)
  expect_identical(provisional, c("2026" = 2025L))
  expect_identical(result$diagnostics$status, "provisional")
  expect_identical(result$source_incidence, source)
  expect_identical(result$diagnostics$population_source_mode, "live")
  historical <- regionalepi:::.shiny_population_for_reporting_years(
    2017:2022, "snapshot"
  )
  expect_identical(historical$supported_years, 2017:2022)
  expect_length(historical$unsupported_years, 0L)
})

test_that("running analysis cutoff excludes future years and preserves source status", {
  window <- regionalepi:::.shiny_selected_window(
    "Influenza, saisonal", "influenza_2026_27"
  )
  nominal <- regionalepi:::.shiny_select_analysis_range(window, "window")
  effective <- regionalepi:::.shiny_effective_analysis_range(
    nominal, as.Date("2026-10-06")
  )
  requested_years <- NULL
  source <- list(
    data = data.frame(date = as.Date("2026-10-05"), reporting_year = 2026L),
    provenance = list(
      incidence = list(query = list(data_status = "survstat-source-status")),
      counts = list(query = list(data_status = "survstat-source-status"))
    ), timings = numeric(), resolution = list()
  )
  testthat::local_mocked_bindings(
    .shiny_fetch_surveillance_bundle = function(pathogen, years, resources) {
      requested_years <<- years
      source
    },
    prepare_analysis_incidence = function(surveillance,
        annual_average_population, provisional_denominator_years) {
      list(data = surveillance$data,
        diagnostics = list(status = "provisional",
          status_by_reporting_year = data.frame(
            reporting_year = 2026L, population_year = 2025L,
            incidence_status = "provisional")),
        provenance = list(source_incidence = surveillance$provenance$incidence,
          counts = surveillance$provenance$counts,
          population = annual_average_population$provenance),
        source_incidence = surveillance)
    },
    .package = "regionalepi"
  )
  result <- regionalepi:::.shiny_fetch_analysis_bundle(
    "Influenza, saisonal", 2026L, list(), "snapshot", effective
  )
  expect_identical(requested_years, 2026L)
  expect_false(2027L %in% requested_years)
  expect_identical(result$provenance$analysis_range$analysis_as_of_date,
                   as.Date("2026-10-06"))
  expect_identical(result$provenance$analysis_range$observation_window_end,
                   as.Date("2027-05-23"))
  expect_identical(
    result$provenance$source_incidence[[1L]]$data_status,
    "survstat-source-status"
  )
})

test_that("snapshot and live denominator selection enforce one-year availability", {
  snapshot <- regionalepi:::.shiny_population_for_reporting_years(
    c(2026L, 2027L), "snapshot"
  )
  expect_identical(snapshot$supported_years, 2026L)
  expect_identical(snapshot$unsupported_years, 2027L)
  expect_identical(snapshot$provisional, c("2026" = 2025L))

  testthat::local_mocked_bindings(
    fetch_regional_average_population = function(years, ...) list(
      data = data.frame(year = 2026L), provenance = list()
    ),
    .package = "regionalepi"
  )
  live <- regionalepi:::.shiny_population_for_reporting_years(2027L, "live")
  expect_identical(live$supported_years, 2027L)
  expect_identical(live$provisional, c("2027" = 2026L))
})

test_that("typology modes retain distinct identities and palettes", {
  reference <- regionalepi:::.shiny_typology_config("dissertation")
  expect_identical(reference$years, 2017:2020)
  expect_identical(reference$k, 3L)
  expect_identical(regionalepi:::.shiny_app_cluster_colours(
    c("ClD", "ClJ", "ClA"), "dissertation"),
    c(ClD = "#bc5e21", ClJ = "#748c61", ClA = "#274f66"))
  dynamic <- regionalepi:::.shiny_app_cluster_colours(sprintf("C%02d", 1:5))
  expect_identical(length(unique(dynamic)), 5L)
  expect_false(any(dynamic %in% c("#bc5e21", "#748c61", "#274f66")))
  expect_identical(regionalepi:::.shiny_typology_cache_key(
    list(data = data.frame()), 3L, "dissertation"),
    "typology:dissertation_v1:historical_reference")
})

test_that("profile-aligned k3 colours depend on profiles rather than C IDs", {
  ids <- c("population_density", "mean_age", "youth_dependency_ratio")
  reference <- data.frame(display_cluster_id=rep(c("ClD","ClJ","ClA"),each=3),
    indicator_id=rep(ids,3),standardized_center=c(2,0,-1,-1,-1,2,0,2,-1))
  dynamic <- data.frame(display_cluster_id=rep(c("C01","C02","C03"),each=3),
    indicator_id=rep(ids,3),standardized_center=c(0,2,-1,2,0,-1,-1,-1,2))
  alignment <- regionalepi:::.shiny_profile_palette_alignment(dynamic,reference)
  expect_identical(alignment$mapping,c(C01="ClA",C02="ClD",C03="ClJ"))
  expect_false(alignment$ambiguous)
  expect_gt(alignment$margin,0)
  expect_identical(regionalepi:::.shiny_app_cluster_colours(
    c("C01","C02","C03"),"dynamic","profile_aligned",alignment),
    c(C01="#274f66",C02="#bc5e21",C03="#748c61"))
  ambiguous <- dynamic; ambiguous$standardized_center <- 0
  expect_error(regionalepi:::.shiny_profile_palette_alignment(ambiguous,reference),
    "ambiguous")
})

test_that("current dynamic colours follow profile continuity for k3 to k5", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)$summary
  reference <- regionalepi:::.shiny_fit_typology(
    regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary,
    "dissertation",3L)
  anchor <- regionalepi:::.shiny_fit_typology(summary,"dynamic",3L)
  expected <- list(
    `3`=c(C01="#274f66",C02="#748c61",C03="#bc5e21"),
    `4`=c(C01="#274f66",C02="#748c61",C03="#A94F74",C04="#bc5e21"),
    `5`=c(C01="#274f66",C02="#7B61A8",C03="#748c61",
          C04="#A94F74",C05="#bc5e21"))
  for(k in 3:5) {
    fit <- regionalepi:::.shiny_fit_typology(summary,"dynamic",k)
    assignments_before <- fit$assignments
    policy <- regionalepi:::.shiny_dynamic_colour_policy(fit,reference,anchor)
    metadata <- regionalepi:::.shiny_cluster_display_metadata(
      fit,"dynamic","profile_aligned",policy)
    expect_identical(policy$colours,expected[[as.character(k)]])
    expect_identical(length(unique(policy$colours)),k)
    expect_identical(fit$assignments, assignments_before)
    expect_identical(metadata$raw_cluster_id,
      assignments_before$raw_cluster[match(metadata$display_cluster_id,
        assignments_before$display_cluster_id)])
  }
  k4 <- regionalepi:::.shiny_dynamic_colour_policy(
    regionalepi:::.shiny_fit_typology(summary,"dynamic",4L),reference,anchor)
  expect_identical(k4$continuation,c(C01="C01",C02="C02",C03="C04"))
  expect_identical(k4$branch_alignment,c(C03="C03"))
  k5 <- regionalepi:::.shiny_dynamic_colour_policy(
    regionalepi:::.shiny_fit_typology(summary,"dynamic",5L),reference,anchor)
  expect_identical(k5$continuation,c(C01="C01",C02="C03",C03="C05"))
  expect_identical(k5$branch_alignment,c(C04="C03"))
})

test_that("k2 assigns a dense anchor and an explicit mixed-profile colour", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)$summary
  historical <- regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary
  reference <- regionalepi:::.shiny_fit_typology(historical,"dissertation",3L)
  anchor <- regionalepi:::.shiny_fit_typology(summary,"dynamic",3L)
  fit <- regionalepi:::.shiny_fit_typology(summary,"dynamic",2L)
  policy <- regionalepi:::.shiny_dynamic_colour_policy(fit,reference,anchor)
  expect_identical(policy$continuation,c(C03="C02"))
  expect_identical(policy$colours,c(C01="#7B61A8",C02="#bc5e21"))
  expect_gt(policy$target_anchor_fraction["C03","C02"],.97)
  expect_lt(policy$center_distances["C03","C02"],.06)
  expect_lt(max(policy$target_anchor_fraction[,"C01"]),.8)
  metadata <- regionalepi:::.shiny_cluster_display_metadata(
    fit,"dynamic","profile_aligned",policy,
    regionalepi_map_geometry()$features$geo_id)
  expect_identical(metadata$profile_anchor,c(NA_character_,"dense"))
  expect_identical(metadata$display_colour,c("#7B61A8","#bc5e21"))
  expect_identical(length(unique(metadata$display_colour)),2L)
})

test_that("custom ranges handle exact boundaries one week and empty subsets", {
  window <- regionalepi:::.shiny_selected_window("COVID-19","covid19_2020_21")
  exact <- regionalepi:::.shiny_select_analysis_range(window,"custom",
    custom_dates=as.Date(c("2020-05-11","2021-05-23")))
  expect_identical(exact$start_date,window$start_date)
  expect_identical(exact$end_date,window$end_date)
  expect_identical(length(seq(exact$start_date,exact$end_date,by="week")),54L)
  one_week <- regionalepi:::.shiny_select_analysis_range(window,"custom",
    custom_dates=as.Date(c("2020-05-11","2020-05-17")))
  expect_identical(one_week$end_date-one_week$start_date,as.difftime(6,units="days"))
  expect_error(regionalepi:::.shiny_select_analysis_range(window,"custom",
    custom_dates=as.Date(c("2020-05-04","2020-05-10"))),"au\u00dferhalb")

  fit <- list(assignments=data.frame(geo_id=c("00001","00002"),
    display_cluster_id=c("C01","C02")),provenance=list(fit_id="fit"),
    indicator_set=list(definition_version="v1"))
  observations <- data.frame(geo_id=rep(c("00001","00002"),2),
    geo_name=rep(c("A","B"),2),date=rep(as.Date(c("2020-05-11","2020-05-18")),each=2),
    reporting_year=2020L,reporting_week=rep(c(20L,21L),each=2),
    incidence=c(0,NA,2,3),cases=c(0,NA,2,3),stringsAsFactors=FALSE)
  bundle <- list(data=observations)
  attached <- regionalepi:::.shiny_attach_typology_range(bundle,fit,one_week)
  expect_identical(nrow(attached),2L)
  expect_identical(attached$incidence,c(0,NA_real_))
  expect_no_error(regionalepi:::.shiny_exploration_summaries(bundle,fit,one_week))
  empty_range <- list(start_date=as.Date("2020-06-01"),end_date=as.Date("2020-06-08"))
  empty <- regionalepi:::.shiny_attach_typology_range(bundle,fit,empty_range)
  expect_identical(nrow(empty),0L)
  expect_true(all(c("cluster_id","typology_id","typology_definition_version")%in%names(empty)))
  expect_error(regionalepi:::.shiny_exploration_summaries(bundle,fit,empty_range),
    "keine darstellbaren Beobachtungen")
})

test_that("one display metadata contract fixes identity order labels and colours", {
  current <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)$summary
  historical <- regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary
  reference <- regionalepi:::.shiny_fit_typology(historical, "dissertation", 3L)
  map_ids <- regionalepi_map_geometry()$features$geo_id
  reference_metadata <- regionalepi:::.shiny_cluster_display_metadata(
    reference, "dissertation", display_geo_ids = map_ids)
  expect_identical(reference_metadata$display_cluster_id, c("ClD", "ClJ", "ClA"))
  expect_identical(reference_metadata$display_colour,
    c("#bc5e21", "#748c61", "#274f66"))
  expect_identical(reference_metadata$display_label, c(
    "ClD \u00b7 dichte Regionen", "ClJ \u00b7 familiengepr\u00e4gte Regionen",
    "ClA \u00b7 \u00e4ltere, l\u00e4ndliche Regionen"))
  expect_identical(sum(reference_metadata$n_districts), 400L)
  map <- regionalepi_map_geometry()
  joined <- regionalepi:::.shiny_map_assignments(map, reference)$data
  widget <- regionalepi:::.shiny_app_leaflet_geojson(
    map$browser_geojson, regionalepi:::.shiny_app_map_bounds(map), joined,
    "dissertation", regionalepi_state_boundaries()$browser_geojson,
    regionalepi:::.shiny_germany_outline_geojson(),
    display_metadata = reference_metadata)
  map_display <- widget$jsHooks$render[[1L]]$data
  representatives <- c(ClD="01001",ClJ="01004",ClA="01003")
  expect_identical(unlist(map_display$clusters[representatives],use.names=FALSE),
    names(representatives))
  expect_identical(unlist(map_display$colours[names(representatives)],use.names=FALSE),
    c("#bc5e21", "#748c61", "#274f66"))

  for (years_summary in list(current, historical)) {
    anchor <- regionalepi:::.shiny_fit_typology(years_summary, "dynamic", 3L)
    policy <- regionalepi:::.shiny_dynamic_colour_policy(anchor, reference, anchor)
    metadata <- regionalepi:::.shiny_cluster_display_metadata(
      anchor, "dynamic", "profile_aligned", policy, map_ids)
    expect_identical(metadata$display_cluster_id, c("C01", "C02", "C03"))
    expect_identical(metadata$profile_anchor,
      c("older_low_density", "family_youth", "dense"))
    expect_identical(metadata$display_colour,
      c("#274f66", "#748c61", "#bc5e21"))
    applied <- regionalepi:::.shiny_apply_display_metadata(
      data.frame(cluster_id = rev(metadata$display_cluster_id)), metadata)
    expect_identical(levels(applied$cluster_id), metadata$display_cluster_id)
    expect_identical(as.character(applied$display_colour),
      metadata$display_colour[match(as.character(applied$cluster_id),
                                    metadata$display_cluster_id)])
  }
})

test_that("historical Shiny semantics survive raw k-means label permutation", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary
  plain <- fit_dissertation_typology(summary, label_mapping = "none")
  expected <- regionalepi:::.shiny_historical_semantic_mapping(plain)

  permutation <- c(`1` = 1L, `2` = 3L, `3` = 2L)
  permuted <- plain
  permuted$data$raw_cluster <- unname(
    permutation[as.character(permuted$data$raw_cluster)]
  )
  permuted$diagnostics$centers <- plain$diagnostics$centers[
    c("1", "3", "2"), , drop = FALSE
  ]
  rownames(permuted$diagnostics$centers) <- c("1", "2", "3")
  observed <- regionalepi:::.shiny_historical_semantic_mapping(permuted)

  expect_identical(expected$cluster_code, c("ClD", "ClJ", "ClA"))
  expect_identical(expected$raw_cluster, c(1L, 2L, 3L))
  expect_identical(observed$cluster_code, c("ClD", "ClJ", "ClA"))
  expect_identical(observed$raw_cluster, c(1L, 3L, 2L))
  expected_code <- expected$cluster_code[match(
    plain$data$raw_cluster, expected$raw_cluster
  )]
  observed_code <- observed$cluster_code[match(
    permuted$data$raw_cluster, observed$raw_cluster
  )]
  expect_identical(observed_code, expected_code)
})

test_that("dynamic profile descriptions lead with the strongest feature", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)$summary
  display <- regionalepi:::.shiny_app_indicator_display()
  for (k in 2:5) {
    fit <- regionalepi:::.shiny_fit_typology(summary, "dynamic", k)
    descriptions <- regionalepi:::.shiny_profile_descriptions(fit$profiles)
    for (cluster in unique(fit$profiles$display_cluster_id)) {
      profile <- fit$profiles[fit$profiles$display_cluster_id == cluster, ]
      strongest <- profile$indicator_id[
        order(-abs(profile$standardized_center),
              match(profile$indicator_id, display$indicator_id))[[1L]]]
      if (max(abs(profile$standardized_center)) >= 0.5) {
        first_label <- display$label[match(strongest, display$indicator_id)]
        description <- descriptions$profile_description[
          descriptions$display_cluster_id == cluster]
        expect_match(description, first_label, fixed = TRUE)
        expect_lt(regexpr(first_label, description, fixed = TRUE)[[1L]],
                  regexpr(",", paste0(description, ","), fixed = TRUE)[[1L]])
      }
    }
  }
  k3 <- regionalepi:::.shiny_fit_typology(summary, "dynamic", 3L)
  k3_description <- regionalepi:::.shiny_profile_descriptions(k3$profiles)
  expect_match(k3_description$profile_description[
    k3_description$display_cluster_id == "C03"], "Bev\u00f6lkerungsdichte",
    fixed = TRUE)

  k4 <- regionalepi:::.shiny_profile_descriptions(
    regionalepi:::.shiny_fit_typology(summary, "dynamic", 4L)$profiles)
  expect_match(k4$profile_description[k4$display_cluster_id == "C03"],
    "Bev\u00f6lkerungsdichte", fixed = TRUE)
  k5 <- regionalepi:::.shiny_profile_descriptions(
    regionalepi:::.shiny_fit_typology(summary, "dynamic", 5L)$profiles)
  for (id in c("C03", "C04", "C05")) {
    expect_match(k5$profile_description[k5$display_cluster_id == id],
      "Bev\u00f6lkerungsdichte", fixed = TRUE)
  }
  expect_false(grepl("Jugendquotient",
    k5$profile_description[k5$display_cluster_id == "C02"], fixed = TRUE))
  expect_false(any(k5$profile_description == "durchschnittliches Profil"))

  neutral <- regionalepi:::.shiny_fit_typology(summary, "dynamic", 3L)$profiles
  neutral$standardized_center <- 0.1
  expect_true(all(regionalepi:::.shiny_profile_descriptions(neutral)$
    profile_description == "durchschnittliches Profil"))
})

test_that("partition comparison reports ARI overlap and center distances", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)$summary
  fits <- lapply(3:5,function(k)regionalepi:::.shiny_fit_typology(summary,"dynamic",k))
  three_four <- regionalepi:::.shiny_compare_partitions(fits[[1L]],fits[[2L]])
  four_five <- regionalepi:::.shiny_compare_partitions(fits[[2L]],fits[[3L]])
  expect_equal(three_four$ari,0.9461448,tolerance=1e-7)
  expect_equal(four_five$ari,0.4953524,tolerance=1e-7)
  expect_identical(sum(three_four$contingency),400L)
  expect_identical(dim(three_four$center_distances),c(3L,4L))
  expect_named(three_four$transition,c("source_k","source_cluster","target_k",
    "target_cluster","n_shared","source_fraction","target_fraction",
    "source_center_distance"))
})

test_that("typology stability contract uses membership and conserves fractions", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)$summary
  fits <- lapply(2:5,function(k)
    regionalepi:::.shiny_fit_typology(summary,"dynamic",k))
  stability <- regionalepi:::.shiny_typology_stability(fits)
  expect_identical(stability$reference_k,3L)
  expect_identical(stability$summary$target_k,c(2L,4L,5L))
  expect_equal(stability$summary$ari,
    c(0.3908490,0.9461448,0.4617140),tolerance=1e-7)
  required <- c("reference_fit_id","reference_cluster","target_fit_id",
    "target_cluster","n_shared","reference_n","target_n",
    "reference_fraction","target_fraction","reference_center_distance",
    "target_k")
  expect_named(stability$transitions,required)
  for (target_k in c(2L,4L,5L)) {
    x <- stability$transitions[stability$transitions$target_k==target_k,]
    expect_equal(tapply(x$n_shared,x$reference_cluster,sum),
      tapply(x$reference_n,x$reference_cluster,unique))
    expect_equal(as.vector(tapply(x$reference_fraction,x$reference_cluster,sum)),
      rep(1,3))
    expect_equal(as.vector(tapply(x$target_fraction,x$target_cluster,sum)),
      rep(1,target_k))
  }
  expect_match(regionalepi:::.shiny_stability_interpretation(stability),
    "grobe Zweiteilung",fixed=TRUE)
  broken <- fits[[2L]]
  broken$assignments <- rbind(broken$assignments,broken$assignments[1L,])
  expect_error(regionalepi:::.shiny_typology_transition(broken,fits[[1L]]),
    "unique geo_id")
})

test_that("stability and pairwise sections use reviewed visual hierarchy", {
  ui_text <- paste(readLines(file.path(regionalepi:::.shiny_app_dir(), "ui.R"),
    warn=FALSE),collapse="\n")
  expect_match(ui_text,"Wie verändert sich die Drei-Cluster-Referenz",fixed=TRUE)
  expect_match(ui_text,'tabPanel("Paarweise Clusterunterschiede"',fixed=TRUE)
  expect_match(ui_text,"A − B bezeichnet den wöchentlichen Median",fixed=TRUE)
  body <- paste(deparse(body(regionalepi:::.shiny_app_leaflet_geojson)),collapse="\n")
  expect_match(body,'color = "#F7F7F3"',fixed=TRUE)
  expect_match(body,"weight = 1.2",fixed=TRUE)
  expect_match(body,"opacity = 0.98",fixed=TRUE)
  expect_match(body,'color = "#27313A"',fixed=TRUE)
  expect_match(body,"weight = 2.1",fixed=TRUE)
})

test_that("stability alluvials compare k2 k4 and k5 independently with k3", {
  skip_if_not_installed("plotly")
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)$summary
  fits <- lapply(2:5,function(k)
    regionalepi:::.shiny_fit_typology(summary,"dynamic",k))
  stability <- regionalepi:::.shiny_typology_stability(fits)
  metadata <- regionalepi:::.shiny_cluster_display_metadata(fits[[2L]])
  widget <- regionalepi:::.shiny_stability_alluvial_widget(stability,metadata)
  built <- plotly::plotly_build(widget)
  expect_identical(length(built$x$data),3L)
  expect_true(all(vapply(built$x$data,function(x)x$type,character(1L))=="sankey"))
  expect_identical(vapply(built$x$data,function(x)sum(x$link$value),integer(1L)),
    c(400L,400L,400L))
  expect_identical(vapply(built$x$data,function(x)as.integer(sub("k=([245]).*","\\1",
    tail(x$node$label,1L))),integer(1L)),c(2L,4L,5L))
  expect_true(all(vapply(built$x$data,function(x)
    all(grepl("^k=3 ",head(x$node$label,3L))),logical(1L))))
  expect_match(paste(vapply(built$x$layout$annotations,`[[`,character(1L),"text"),
    collapse=" "),"k=3 → k=2 k=3 → k=4 k=3 → k=5",fixed=TRUE)
})

test_that("population-density scaling is display-only and log-safe", {
  ui_text <- paste(readLines(file.path(regionalepi:::.shiny_app_dir(),"ui.R"),
    warn=FALSE),collapse="\n")
  server_text <- paste(readLines(file.path(regionalepi:::.shiny_app_dir(),
    "server.R"),warn=FALSE),collapse="\n")
  expect_match(ui_text,"Skalierung Bevölkerungsdichte",fixed=TRUE)
  expect_match(ui_text,'c("Original" = "original", "Logarithmisch" = "log10")',
    fixed=TRUE)
  expect_match(ui_text,paste("Die Skalierung betrifft ausschließlich die",
    "Bevölkerungsdichte. Durchschnittsalter und Jugendquotient werden stets",
    "auf der Originalskala dargestellt."),fixed=TRUE)
  expect_match(server_text,"scale_y_log10",fixed=TRUE)
  expect_match(server_text,"positive finite values",fixed=TRUE)
  expect_match(server_text,"Darstellung: logarithmische Achse",fixed=TRUE)
  expect_false(grepl("density_scale",paste(c(
    deparse(body(regionalepi:::.shiny_demographic_cache_key)),
    deparse(body(regionalepi:::.shiny_typology_cache_key)),
    deparse(body(regionalepi:::.shiny_surveillance_cache_key))),collapse="\n")))
  for(i in seq_along(list(2017:2020,2022:2025))) {
    years<-list(2017:2020,2022:2025)[[i]]
    x<-regionalepi:::.shiny_fetch_snapshot_demography(years)$summary$data
    x<-x$indicator_value[x$indicator_id=="population_density"]
    expect_length(x,c(401L,400L)[[i]])
    expect_true(all(is.finite(x)))
    expect_true(all(x>0))
  }
})

test_that("finite-n display geometry follows the reviewed thresholds", {
  expected <- c(
    `0`="unavailable", `1`="point_only", `2`="points_and_median",
    `4`="points_and_median", `5`="points_and_boxplot",
    `9`="points_and_boxplot", `10`="points_boxplot_and_violin"
  )
  for (n in as.integer(names(expected))) {
    values <- if (n) c(seq_len(n) - 1, NA_real_, NaN, Inf, -Inf) else
      c(NA_real_, NaN, Inf, -Inf)
    rule <- regionalepi:::.shiny_finite_n_display(values)
    expect_identical(rule$n_finite, n)
    expect_identical(rule$display_rule, unname(expected[as.character(n)]))
    expect_identical(rule$show_points, n > 0L)
    expect_identical(rule$show_median, n >= 2L && n <= 4L)
    expect_identical(rule$show_boxplot, n >= 5L)
    expect_identical(rule$show_violin, n >= 10L)
  }

  sizes <- c(A=1L,B=2L,C=4L,D=5L,E=9L,F=10L)
  data <- do.call(rbind,lapply(names(sizes),function(id)data.frame(
    cluster_id=id,value=seq_len(sizes[[id]])-1,
    geo_id=sprintf("%s%02d",id,seq_len(sizes[[id]])),
    stringsAsFactors=FALSE)))
  data <- rbind(data,data.frame(cluster_id="F",value=NA_real_,geo_id="FNA"))
  original <- data
  layers <- regionalepi:::.shiny_adaptive_distribution_layers(
    data,"cluster_id","value")
  expect_identical(data,original)
  expect_identical(stats::setNames(layers$rules$n_finite,
    layers$rules$cluster_id),sizes)
  expect_setequal(unique(layers$violins$cluster_id),"F")
  expect_setequal(unique(layers$boxplots$cluster_id),c("D","E","F"))
  expect_setequal(layers$medians$cluster_id,c("B","C"))
  expect_identical(layers$medians$.display_median,c(.5,1.5))
  expect_identical(nrow(layers$points),sum(sizes))
  expect_false(any(!is.finite(layers$points$value)))
})

test_that("weekly district distributions preserve ISO week support and missingness", {
  data<-data.frame(
    date=as.Date(rep(c("2026-07-20","2026-07-27"),each=6)),
    geo_id=rep(sprintf("%05d",1:6),2),
    geo_name=rep(paste("Kreis",1:6),2),
    cluster_id=rep(c("C01","C01","C02","C02","C03","C03"),2),
    incidence=c(0,1,NA,3,4,5,10,11,12,13,14,15),
    cases=c(0,1,NA,3,4,5,10,11,12,13,14,15),
    stringsAsFactors=FALSE)
  result<-regionalepi:::.shiny_week_distribution(data,
    as.Date("2026-07-20"),c("C01","C02","C03"))
  expect_identical(result$date,as.Date("2026-07-20"))
  expect_identical(nrow(result$data),6L)
  expect_identical(result$support$observed_districts,c(2L,1L,2L))
  expect_identical(result$support$missing_districts,c(0L,1L,0L))
  expect_identical(result$support$zero_districts,c(1L,0L,0L))
  expect_true(0%in%result$data$incidence)
  expect_true(anyNA(result$data$incidence))
  expect_identical(levels(result$data$cluster_display),
    c("C01 (n=2)","C02 (n=1)","C03 (n=2)"))
  expect_error(regionalepi:::.shiny_week_distribution(data,
    as.Date("2026-08-03"),c("C01","C02","C03")),"not part")
})

test_that("final distribution and weekly heatmap grammar is explicit", {
  expect_identical(regionalepi:::.shiny_weekly_heatmap_xgap(0L),1)
  expect_identical(regionalepi:::.shiny_weekly_heatmap_xgap(60L),1)
  expect_identical(regionalepi:::.shiny_weekly_heatmap_xgap(61L),0)
  expect_error(regionalepi:::.shiny_weekly_heatmap_xgap(1.5),"whole number")
  grid <- regionalepi:::.shiny_heatmap_grid(data.frame(
    cluster_id=c("C01","C01","C02"),
    date=as.Date(c("2024-01-01","2024-01-08","2024-01-01")),
    value=c(0,NA_real_,50),stringsAsFactors=FALSE),
    "cluster_id","date","value")
  expect_identical(grid$z[1L,1L],0)
  expect_true(is.na(grid$z[1L,2L]))

  app <- regionalepi:::.shiny_app_dir()
  ui_text <- paste(readLines(file.path(app,"ui.R"),warn=FALSE),collapse="\n")
  server_text <- paste(readLines(file.path(app,"server.R"),warn=FALSE),
    collapse="\n")
  expect_match(server_text,".shiny_adaptive_distribution_layers",fixed=TRUE)
  expect_match(server_text,"Deutschlandreferenz<br>Nationaler Regionaltyp",
    fixed=TRUE)
  expect_match(server_text,"shape=23,size=3.5,fill=\"white\"",fixed=TRUE)
  expect_match(server_text,".shiny_weekly_heatmap_xgap(length(g$columns))",
    fixed=TRUE)
  expect_identical(regionalepi:::.shiny_signed_heatmap_colours(),
    c("#5F627B", "#F7F7F7", "#C86600"))
  expect_match(server_text,".shiny_signed_heatmap_colours()",fixed=TRUE)
  expect_match(server_text,'zmin=-limit,zmax=limit,zmid=0',fixed=TRUE)
  expect_match(server_text,'plot_bgcolor="#D1D5DB"',fixed=TRUE)
  expect_false(grepl("shape=95",server_text,fixed=TRUE))
})

test_that("canonical pair keys respect display order and remain unique for k2 to k5", {
  dates <- as.Date("2020-09-28") + 7*0:3
  for (k in 2:5) {
    ids <- sprintf("C%02d",seq_len(k))
    weekly <- expand.grid(date=dates,cluster_id=ids,KEEP.OUT.ATTRS=FALSE,
      stringsAsFactors=FALSE)
    weekly$median_incidence <- seq_len(nrow(weekly))
    pairs <- regionalepi:::.shiny_pairwise_differences(weekly,ids)
    expect_equal(nrow(pairs),length(dates)*choose(k,2L))
    expect_identical(anyDuplicated(paste(pairs$date,pairs$pair_key)),0L)
    expect_true(all(table(pairs$date)==choose(k,2L)))
    expect_equal(pairs$difference,pairs$median_a-pairs$median_b)
  }

  ids <- c("ClD","ClJ","ClA")
  weekly <- expand.grid(date=dates,cluster_id=rev(ids),KEEP.OUT.ATTRS=FALSE,
    stringsAsFactors=FALSE)
  weekly$median_incidence <- match(weekly$cluster_id,ids)*10+
    match(weekly$date,dates)
  pairs <- regionalepi:::.shiny_pairwise_differences(weekly,ids)
  expect_identical(unique(pairs$pair_label),
    c("ClD \u2212 ClJ","ClD \u2212 ClA","ClJ \u2212 ClA"))
  expect_identical(unique(pairs$pair_key),
    c("ClD\rClJ","ClD\rClA","ClJ\rClA"))
  expect_identical(nrow(pairs),12L)
  expect_identical(anyDuplicated(paste(pairs$date,pairs$pair_key)),0L)
  expect_true(all(pairs$difference==pairs$median_a-pairs$median_b))
  grid <- regionalepi:::.shiny_heatmap_grid(
    pairs,"pair_key","date","difference","pair_label")
  widget <- plotly::plotly_build(plotly::plot_ly(x=grid$columns,
    y=unique(pairs$pair_label),z=grid$z,type="heatmap",zmid=0,
    colors=regionalepi:::.shiny_signed_heatmap_colours(),text=grid$text,
    hoverinfo="text",connectgaps=FALSE))
  expect_identical(length(widget$x$data[[1L]]$y),3L)
  expect_identical(dim(widget$x$data[[1L]]$z),c(3L,4L))
})

test_that("advanced pairwise UI and server retain a live Plotly output contract", {
  skip_if_not_installed("plotly")
  app <- regionalepi:::.shiny_app_dir()
  ui_text <- paste(readLines(file.path(app,"ui.R"),warn=FALSE),collapse="\n")
  server_text <- paste(readLines(file.path(app,"server.R"),warn=FALSE),collapse="\n")
  expect_match(ui_text,'tabsetPanel(id="weekly_comparison_view"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Relative Aktivität"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Paarweise Clusterunterschiede"',fixed=TRUE)
  expect_false(grepl('tags$summary("Erweiterte paarweise Vergleiche")',
    ui_text,fixed=TRUE))
  expect_match(ui_text,'plotly::plotlyOutput("pairwise_heatmap"',fixed=TRUE)
  expect_match(ui_text,"A − B bezeichnet den wöchentlichen Median",fixed=TRUE)
  expect_match(server_text,"output$pairwise_heatmap <- plotly::renderPlotly",fixed=TRUE)
  expect_match(server_text,"length(metadata$display_cluster_id)>=2L",fixed=TRUE)
  expect_false(grepl('outputOptions(output,"pairwise_heatmap"',server_text,
    fixed=TRUE))

  dates <- as.Date("2020-09-28") + 7L * 0:21
  for (k in 2:5) {
    ids <- if (k == 3L) c("ClD","ClJ","ClA") else sprintf("C%02d",seq_len(k))
    weekly <- expand.grid(date=dates,cluster_id=ids,KEEP.OUT.ATTRS=FALSE,
      stringsAsFactors=FALSE)
    weekly$median_incidence <- seq_len(nrow(weekly))/10
    pairwise <- regionalepi:::.shiny_pairwise_differences(weekly,ids)
    metadata <- data.frame(display_cluster_id=ids,stringsAsFactors=FALSE)
    widget <- regionalepi:::.shiny_pairwise_heatmap_widget(pairwise,metadata)
    built <- plotly::plotly_build(widget)
    expect_s3_class(widget,"plotly")
    expect_identical(nrow(pairwise),as.integer(22L*choose(k,2L)))
    expect_identical(anyDuplicated(paste(pairwise$date,pairwise$pair_key)),0L)
    expect_identical(dim(built$x$data[[1L]]$z),c(as.integer(choose(k,2L)),22L))
    expect_identical(built$x$data[[1L]]$xgap,1)
    expect_equal(pairwise$difference,pairwise$median_a-pairwise$median_b)
  }
  long <- expand.grid(date=as.Date("2020-01-06")+7L*0:60,
    cluster_id=c("C01","C02"),stringsAsFactors=FALSE)
  long$median_incidence<-seq_len(nrow(long))
  long_widget<-plotly::plotly_build(regionalepi:::.shiny_pairwise_heatmap_widget(
    regionalepi:::.shiny_pairwise_differences(long,c("C01","C02")),
    data.frame(display_cluster_id=c("C01","C02"))))
  expect_identical(long_widget$x$data[[1L]]$xgap,0)
  period <- data.frame(start_date = min(dates), end_date = max(dates))
  bounded <- plotly::plotly_build(regionalepi:::.shiny_pairwise_heatmap_widget(
    pairwise, metadata, period))
  expect_identical(length(bounded$x$layout$shapes), 2L)
  expect_identical(vapply(bounded$x$layout$shapes, `[[`, character(1L), "x0"),
    format(c(period$start_date, period$end_date)))
})

test_that("final weekly widget hides every IQR legend trace", {
  skip_if_not_installed("plotly");skip_if_not_installed("ggplot2")
  x<-data.frame(date=rep(as.Date("2025-01-06")+0:2,3),
    cluster=rep(c("C01","C02","C03"),each=3),median=1:9,q1=0:8,q3=2:10)
  graph<-ggplot2::ggplot(x,ggplot2::aes(date,median,colour=cluster,fill=cluster,
    group=cluster))+ggplot2::geom_ribbon(ggplot2::aes(ymin=q1,ymax=q3),alpha=.15,
      colour=NA,show.legend=FALSE)+ggplot2::geom_line()+ggplot2::geom_line(
        data=x[x$cluster=="C01",],ggplot2::aes(date,median,group=1),
        inherit.aes=FALSE,colour="#111111")
  widget<-regionalepi:::.shiny_finalize_incidence_legend(plotly::ggplotly(graph))
  ribbons<-vapply(widget$x$data,function(trace)identical(trace$fill,"toself"),logical(1L))
  visible<-vapply(widget$x$data,function(trace)isTRUE(trace$showlegend),logical(1L))
  expect_true(all(!visible[ribbons]))
  expect_setequal(vapply(widget$x$data[visible],`[[`,character(1L),"name"),
    c("C01","C02","C03","Ausgewählter Kreis"))
})

test_that("weekly Plotly traces bind cluster values to named display colours", {
  skip_if_not_installed("plotly");skip_if_not_installed("ggplot2")
  rendered_traces <- function(values, colours) {
    ids <- names(values)
    metadata <- data.frame(
      display_cluster_id = ids,
      display_colour = unname(colours[ids]),
      display_label = ids,
      display_order = seq_along(ids),
      stringsAsFactors = FALSE
    )
    x <- expand.grid(
      date = as.Date(c("2026-01-26", "2026-02-02")),
      cluster_id = ids, stringsAsFactors = FALSE
    )
    x$median <- unname(values[as.character(x$cluster_id)])
    x$q1 <- x$median - 1
    x$q3 <- x$median + 1
    x <- regionalepi:::.shiny_apply_display_metadata(x, metadata)
    palette <- stats::setNames(
      metadata$display_colour, metadata$display_cluster_id
    )
    graph <- ggplot2::ggplot(
      x, ggplot2::aes(
        date, median, colour = cluster_id, fill = cluster_id,
        group = cluster_id
      )
    ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q1, ymax = q3), alpha = .15,
        colour = NA, show.legend = FALSE
      ) +
      ggplot2::geom_line() +
      ggplot2::scale_colour_manual(values = palette) +
      ggplot2::scale_fill_manual(values = palette, guide = "none")
    regionalepi:::.shiny_finalize_incidence_legend(
      plotly::ggplotly(graph)
    )$x$data
  }
  assert_binding <- function(traces, values, line_colours, fill_colours) {
    lines <- traces[vapply(traces, function(trace) {
      !identical(trace$fill, "toself") && isTRUE(trace$showlegend)
    }, logical(1L))]
    expect_setequal(vapply(lines, `[[`, character(1L), "name"), names(values))
    for (id in names(values)) {
      trace <- lines[[which(vapply(lines, function(x) identical(x$name, id),
        logical(1L)))]]
      expect_equal(tail(trace$y, 1L), unname(values[[id]]), tolerance = 1e-12)
      expect_identical(trace$line$color, unname(line_colours[[id]]))
    }
    ribbons <- traces[vapply(traces, function(trace) {
      identical(trace$fill, "toself")
    }, logical(1L))]
    for (id in names(values)) {
      trace <- ribbons[[which(vapply(ribbons, function(x) {
        grepl(id, x$name, fixed = TRUE)
      }, logical(1L)))]]
      expect_identical(trace$fillcolor, unname(fill_colours[[id]]))
      expect_false(isTRUE(trace$showlegend))
    }
  }

  historical_values <- c(ClD = 20.26, ClJ = 22.27, ClA = 48.35)
  historical_colours <- c(
    ClD = "#bc5e21", ClJ = "#748c61", ClA = "#274f66"
  )
  assert_binding(
    rendered_traces(historical_values, historical_colours),
    historical_values,
    c(ClD = "rgba(188,94,33,1)", ClJ = "rgba(116,140,97,1)",
      ClA = "rgba(39,79,102,1)"),
    c(ClD = "rgba(188,94,33,0.15)", ClJ = "rgba(116,140,97,0.15)",
      ClA = "rgba(39,79,102,0.15)")
  )

  updated_values <- c(C01 = 47.83, C02 = 23.06, C03 = 20.70)
  updated_colours <- c(
    C01 = "#274f66", C02 = "#748c61", C03 = "#bc5e21"
  )
  assert_binding(
    rendered_traces(updated_values, updated_colours),
    updated_values,
    c(C01 = "rgba(39,79,102,1)", C02 = "rgba(116,140,97,1)",
      C03 = "rgba(188,94,33,1)"),
    c(C01 = "rgba(39,79,102,0.15)", C02 = "rgba(116,140,97,0.15)",
      C03 = "rgba(188,94,33,0.15)")
  )
})

test_that("pathogen selection reconciliation never exposes a stale window", {
  covid <- regionalepi:::.shiny_reconcile_selection(
    "COVID-19","influenza_2025_26","reviewed")
  expect_identical(covid$window_id,"covid19_2026_27")
  expect_identical(covid$window$pathogen,"COVID-19")
  expect_identical(covid$range_mode,"window")
  expect_identical(nrow(covid$periods),0L)
  influenza <- regionalepi:::.shiny_reconcile_selection(
    "Influenza, saisonal",covid$window_id,"window")
  expect_identical(influenza$window$pathogen,"Influenza, saisonal")
  expect_true(influenza$window_id %in% unname(influenza$choices))
})

test_that("predefined period availability follows pathogen and window", {
  no_influenza_period <- regionalepi:::.shiny_reconcile_selection(
    "Influenza, saisonal", "influenza_2020_21", "reviewed")
  expect_identical(nrow(no_influenza_period$periods), 0L)
  expect_identical(no_influenza_period$range_mode, "window")
  expect_no_error(regionalepi:::.shiny_select_analysis_range(
    no_influenza_period$window, no_influenza_period$range_mode))

  influenza <- regionalepi:::.shiny_reconcile_selection(
    "Influenza, saisonal", "influenza_2022_23", "reviewed")
  expect_gt(nrow(influenza$periods), 0L)
  expect_identical(influenza$range_mode, "reviewed")
  expect_no_error(regionalepi:::.shiny_select_analysis_range(
    influenza$window, "reviewed", influenza$periods[1L, ]))

  covid <- regionalepi:::.shiny_reconcile_selection(
    "COVID-19", "covid19_2021_22", "reviewed")
  expect_gt(nrow(covid$periods), 0L)
  expect_identical(covid$range_mode, "reviewed")
  expect_no_error(regionalepi:::.shiny_select_analysis_range(
    covid$window, "reviewed", covid$periods[1L, ]))

  norovirus <- regionalepi:::.shiny_reconcile_selection(
    "Norovirus-Gastroenteritis", "norovirus_2025_26", "reviewed")
  expect_identical(nrow(norovirus$periods), 0L)
  expect_identical(norovirus$range_mode, "window")
})

test_that("selection reconciliation clears periods absent from a new window", {
  completed <- regionalepi:::.shiny_reconcile_selection(
    "Influenza, saisonal", "influenza_2025_26", "reviewed")
  expect_identical(completed$range_mode, "reviewed")
  expect_false(is.na(completed$reviewed_period_id))
  running <- regionalepi:::.shiny_reconcile_selection(
    "Influenza, saisonal", "influenza_2026_27", "reviewed",
    completed$reviewed_period_id)
  expect_identical(running$range_mode, "window")
  expect_true(is.na(running$reviewed_period_id))
  expect_identical(nrow(running$periods), 0L)
  expect_identical(regionalepi:::.shiny_select_analysis_range(
    running$window, running$range_mode)$mode, "window")
  reverse <- regionalepi:::.shiny_reconcile_selection(
    "Influenza, saisonal", "influenza_2025_26", running$range_mode,
    running$reviewed_period_id)
  expect_gt(nrow(reverse$periods), 0L)
  expect_false(is.na(reverse$reviewed_period_id))
  expect_identical(reverse$range_mode, "window")
})

test_that("navigation is display-only and absent from analytical cache keys", {
  keys <- c(
    regionalepi:::.shiny_demographic_cache_key("2022–2025"),
    regionalepi:::.shiny_surveillance_cache_key("COVID-19", 2024),
    regionalepi:::.shiny_summary_cache_key("source", "period", "fit")
  )
  expect_false(any(grepl("overview|typology|epidemiology|methods", keys)))
  ui_text <- paste(readLines(file.path(regionalepi:::.shiny_app_dir(), "ui.R"), warn = FALSE), collapse = "\n")
  expect_match(ui_text, 'id="analysis_section"', fixed = TRUE)
})

test_that("Pass-3c navigation has four main areas and preserves reference APIs", {
  ui_text<-paste(readLines(file.path(regionalepi:::.shiny_app_dir(),"ui.R"),
    warn=FALSE),collapse="\n")
  main_labels<-c("Demografische Typologie","Infektionsgeschehen",
    "Regionale Analyse","Methodik und Daten")
  expect_true(all(vapply(main_labels,function(label)
    grepl(paste0('tabPanel("',label,'"'),ui_text,fixed=TRUE),logical(1L))))
  expect_false(grepl('tabPanel("Übersicht"',ui_text,fixed=TRUE))
  expect_false(grepl('tabPanel("Referenzanalyse"',ui_text,fixed=TRUE))
  expect_match(ui_text,"Karte und Kreise",fixed=TRUE)
  expect_match(ui_text,"Veränderungen 2017–2025",fixed=TRUE)
  expect_match(ui_text,"Übergangsmatrix",fixed=TRUE)
  expect_match(ui_text,"transition_sankey",fixed=TRUE)
  expect_match(ui_text,"transition_map",fixed=TRUE)
  expect_match(ui_text,"transition_district_detail",fixed=TRUE)
  expect_match(ui_text,
    "Ein Wechsel der Clusterzuordnung bedeutet nicht automatisch, dass sich die demografische Struktur eines Kreises grundlegend verändert hat. Die Zuordnung hängt auch von der Verteilung aller Kreise und den neu berechneten Clusterzentren ab.",
    fixed=TRUE)
  expect_false("Norovirus-Gastroenteritis"%in%
    unname(regionalepi:::.shiny_reference_pathogen_choices()))
  influenza<-regionalepi:::.shiny_reference_selection(
    "Influenza, saisonal","influenza_2017_18")
  expect_identical(influenza$window$observation_window_id,"influenza_2017_18")
  expect_true(all(influenza$periods$period_id%in%"influenza_2017_18"))
  expect_error(regionalepi:::.shiny_reference_selection(
    "Norovirus-Gastroenteritis"),"Unsupported")
  expect_true(is.function(regionalepi::fit_dissertation_typology))
})

test_that("Pass-2b regional navigation and demographic foundation are explicit", {
  app<-regionalepi:::.shiny_app_dir()
  ui_text<-paste(readLines(file.path(app,"ui.R"),warn=FALSE),collapse="\n")
  server_text<-paste(readLines(file.path(app,"server.R"),warn=FALSE),
    collapse="\n")
  regional_start<-regexpr('tabsetPanel(id = "regional_view"',ui_text,
    fixed=TRUE)[[1L]]
  methods_start<-regexpr('tabPanel("Methodik und Daten"',ui_text,
    fixed=TRUE)[[1L]]
  regional_ui<-substr(ui_text,regional_start,methods_start-1L)
  positions<-vapply(c("Zeitverlauf","Demografische Zusammensetzung","Kreise"),
    function(label)regexpr(paste0('tabPanel("',label,'"'),regional_ui,
      fixed=TRUE)[[1L]],integer(1L))
  expect_true(all(positions>0L))
  expect_identical(order(positions),1:3)
  expect_match(regional_ui,'tabsetPanel(id = "regional_view",\n        tabPanel("Zeitverlauf"',
    fixed=TRUE)
  expect_false(grepl('tabPanel("Demografische Struktur"',regional_ui,
    fixed=TRUE))
  expect_match(ui_text,'h4("Demografische Grundlage")',fixed=TRUE)
  expect_match(ui_text,'actionLink("show_typology", "Typologie ändern")',
    fixed=TRUE)
  expect_match(server_text,
    'updateTabsetPanel(session,"analysis_section",selected="typology")',
    fixed=TRUE)
  expect_match(server_text,
    'updateTabsetPanel(session,"typology_view",selected="map_districts")',
    fixed=TRUE)
})

test_that("Pass-3c consolidates distributions and methodology", {
  app<-regionalepi:::.shiny_app_dir()
  ui_text<-paste(readLines(file.path(app,"ui.R"),warn=FALSE),collapse="\n")
  server_text<-paste(readLines(file.path(app,"server.R"),warn=FALSE),
    collapse="\n")
  expect_match(ui_text,".status-info",fixed=TRUE)
  expect_match(server_text,
    'p(class="status-info",paste(sprintf(',fixed=TRUE)
  expect_match(server_text,
    'p(class="status-info",paste("Fr\u00fcher Beobachtungsstand:",early))',
    fixed=TRUE)
  expect_match(server_text,
    '"Laufender Beobachtungszeitraum: bis zum aktuellen Analyse-Stichtag."',
    fixed=TRUE)
  expect_false(grepl(
    'running_notice<-if(isTRUE(r$is_truncated))if(is.null(early))',
    server_text,fixed=TRUE))
  expect_false(grepl('tabPanel("Referenzanalyse"',ui_text,fixed=TRUE))
  expect_false(grepl("reference_state",server_text,fixed=TRUE))
  expect_match(ui_text,'"Ausgewählte ISO-Kalenderwoche" = "week"',fixed=TRUE)
  expect_match(ui_text,'uiOutput("distribution_week_control")',fixed=TRUE)
  expect_match(ui_text,paste0('c("2017–2020", "2022–2025", ',
    '"Benutzerdefinierter Referenzzeitraum")'),
    fixed=TRUE)
  expect_match(ui_text,'selectInput("demographic_start_year", "Startjahr"',
    fixed=TRUE)
  expect_match(ui_text,'selectInput("demographic_end_year", "Endjahr"',
    fixed=TRUE)
  expect_match(server_text,"Benutzerdefinierte Konfiguration",fixed=TRUE)
  expect_match(server_text,"Einjähriger Referenzzeitraum",fixed=TRUE)
  expect_match(server_text,paste(
    "Die konkrete Clusterlösung wurde nicht gesondert wissenschaftlich",
    "validiert."),fixed=TRUE)
  expect_false(grepl("Explorative Typologie|Explorative jährliche Typologie",
    paste(ui_text,server_text)))
  expect_match(server_text,"Zensus 2011, ab 2022",fixed=TRUE)
  expect_match(server_text,"output$weekly_distribution_plot",fixed=TRUE)
  expect_match(server_text,"output$analysis_week_distribution_png",fixed=TRUE)
  expect_match(server_text,"output$methods_page",fixed=TRUE)
  expect_match(server_text,'h3("Vergleichbarkeit mit früheren Auswertungen")',
    fixed=TRUE)
  expect_match(server_text,
    "Die demografische Typologie für 2017–2020 reproduziert die Kreiszuordnungen der ursprünglichen Dissertationstypologie.",
    fixed=TRUE)
  expect_match(server_text,"output$comparability_note",fixed=TRUE)
  expect_match(server_text,'strong("R-Paket")',fixed=TRUE)
  expect_match(server_text,'strong("Wissenschaftliche Grundlage")',fixed=TRUE)
  expect_match(server_text,".regionalepi_package_citation()",fixed=TRUE)
  expect_match(server_text,".regionalepi_dissertation_citation()",fixed=TRUE)
  expect_match(server_text,'tags$summary("Technische Provenienz")',fixed=TRUE)
})

test_that("typology configuration identity includes scientific and provenance dimensions", {
  current<-regionalepi:::.shiny_typology_configuration("2022–2025",3L,"snapshot")
  expect_named(current,c("configuration_id","specification_id","indicator_ids",
    "reference_years","source_identity","k","fitting_specification_id",
    "fitting_specification_version","fit_parameters"))
  key<-regionalepi:::.shiny_typology_configuration_key(current,"snapshot-v4")
  expect_false(identical(key,regionalepi:::.shiny_typology_configuration_key(
    regionalepi:::.shiny_typology_configuration("2017–2020",3L,"snapshot"),
    "snapshot-v4")))
  expect_false(identical(key,regionalepi:::.shiny_typology_configuration_key(
    regionalepi:::.shiny_typology_configuration("2022–2025",4L,"snapshot"),
    "snapshot-v4")))
  expect_false(identical(key,regionalepi:::.shiny_typology_configuration_key(
    current,"other-provenance")))
  custom<-regionalepi:::.shiny_typology_configuration(
    "2021–2023",3L,"snapshot",reference_years=2021:2023)
  custom_key<-regionalepi:::.shiny_typology_configuration_key(
    custom,"regionalepi_demography_47bd90242e6c148e")
  expect_false(identical(key,custom_key))
  expect_match(custom_key,"2021-2022-2023",fixed=TRUE)
  expect_match(custom_key,"regionalepi_demography_47bd90242e6c148e",
    fixed=TRUE)
  expect_false(identical(
    regionalepi:::.shiny_demographic_cache_key(
      "2021–2023","snapshot",2021:2023),
    regionalepi:::.shiny_demographic_cache_key(
      "2022–2023","snapshot",2022:2023)))
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
  current <- regionalepi:::.shiny_current_display_assignments(historical)
  expect_identical(names(current), c("geo_id", "cluster_id"))
  expect_identical(nrow(current), 400L)
  expect_identical(current$geo_id, map$features$geo_id)
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
  expect_identical(tail(unname(ui_environment$initial_window_choices), 1L),
                   "influenza_2026_27")
  expect_match(paste(deparse(body(run_regionalepi_app)), collapse = "\n"),
               "optional package")
})

test_that("server error callback resolves its formatter from the package namespace", {
  skip_if_not_installed("shiny")
  app <- regionalepi:::.shiny_app_dir()
  server_environment <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(app, "server.R"), envir = server_environment)

  testthat::local_mocked_bindings(
    .shiny_regional_credentials_available = function(...) TRUE,
    .shiny_fetch_demography = function(years) {
      stop("synthetic Regionaldatenbank failure", call. = FALSE)
    },
    .shiny_app_log_failure = function(stage, error, logger = message) {
      logged <<- list(stage = stage, message = conditionMessage(error))
    },
    .package = "regionalepi"
  )
  logged <- NULL
  shiny::testServer(server_environment$server, {
    session$setInputs(
      pathogen = "Influenza, saisonal",
      window_id = "influenza_2023_24", range_mode = "window",
      demographic_period = "2022–2025", demographic_source = "live",
      typology_mode = "dynamic",
      k = "3", load_analysis = 1
    )
    session$flushReact()
    status <- paste(as.character(output$load_status), collapse = "")
    expect_match(status, "demografischen Daten für die Typologie konnten nicht geladen werden",
                 fixed = TRUE)
    expect_false(grepl("synthetic Regionaldatenbank failure", status, fixed = TRUE))
    expect_false(grepl("app_format_error", status, fixed = TRUE))
    expect_false(grepl("Die zuvor geladene Analyse bleibt angezeigt.", status,
      fixed = TRUE))
    expect_false(state$busy)
    expect_match(state$technical_error, "synthetic Regionaldatenbank failure",
                 fixed = TRUE)
    expect_identical(logged$stage, "demography")
    expect_identical(logged$message, "synthetic Regionaldatenbank failure")

  })
})

test_that("analysis-load diagnostics are stage-aware and do not expose source internals", {
  secret <- paste(
    "SurvStat@RKI retrieval failed: SurvStat API transport:",
    "Authorization: Basic credential-secret",
    "<SOAP><raw-response>private</raw-response></SOAP>"
  )
  output <- character()
  diagnostic <- regionalepi:::.shiny_app_log_failure(
    "surveillance_analysis", simpleError(secret),
    logger = function(message) output <<- c(output, message)
  )
  expect_identical(diagnostic$stage, "surveillance_analysis")
  expect_identical(diagnostic$category, "survstat_transport")
  expect_match(output, "stage=surveillance_analysis", fixed = TRUE)
  expect_match(output, "condition=simpleError/error/condition", fixed = TRUE)
  expect_match(output, "SurvStat transport or source availability failed.",
    fixed = TRUE)
  expect_false(grepl("credential-secret|Authorization|SOAP|raw-response|private",
    output, ignore.case = TRUE))
})

test_that("analysis-load failure handling preserves a previous result", {
  previous <- list(marker = "previous analysis")
  state <- new.env(parent = emptyenv())
  state$result <- previous
  output <- character()
  diagnostic <- regionalepi:::.shiny_app_handle_failure(
    state, simpleError("synthetic deployed failure"), "surveillance_analysis",
    logger = function(message) output <<- c(output, message)
  )
  expect_identical(state$result, previous)
  expect_identical(state$error,
    "Die Live-Analyse konnte nicht geladen werden. Bitte erneut versuchen.")
  expect_identical(state$technical_error, "synthetic deployed failure")
  expect_identical(diagnostic$stage, "surveillance_analysis")
  expect_match(output, "category=unexpected", fixed = TRUE)
  expect_identical(
    regionalepi:::.shiny_app_display_error(state$error, !is.null(state$result)),
    paste(state$error, "Die zuvor geladene Analyse bleibt angezeigt.")
  )
})

test_that("failed-refresh display identifies a still-visible previous result", {
  error <- "Die Live-Analyse konnte nicht geladen werden."
  expect_identical(
    regionalepi:::.shiny_app_display_error(error, FALSE), error
  )
  expect_identical(
    regionalepi:::.shiny_app_display_error(error, TRUE),
    paste(error, "Die zuvor geladene Analyse bleibt angezeigt.")
  )
})

test_that("Shiny user errors hide authentication and source internals", {
  missing <- simpleError(paste(
    "Regionaldatenbank retrieval failed: Invalid Regionaldatenbank",
    "authentication: required environment variables are missing or empty"
  ))
  text <- regionalepi:::.shiny_app_format_error(missing, "Die Live-Analyse")
  expect_match(text, "fehlen die Zugangsdaten", fixed = TRUE)
  expect_false(grepl("environment|authentication", text, ignore.case = TRUE))

  denominator <- regionalepi:::.shiny_app_format_error(
    simpleError(paste(
      "Dynamic annual-average-population incidence requires configured",
      "Regionaldatenbank credentials."
    )),
    "Die Live-Analyse"
  )
  expect_match(denominator,
    "dynamische Inzidenzanalyse benötigt die amtliche durchschnittliche Jahresbevölkerung",
    fixed = TRUE)
  expect_false(grepl("demografischen Daten konnten nicht geladen werden",
    denominator, fixed = TRUE))

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

test_that("runtime code does not look up ordinary package data in the namespace", {
  app <- regionalepi:::.shiny_app_dir()
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE),
    collapse = "\n")
  data_names <- c(
    "regionalepi_geography_resources_2024",
    "regionalepi_map_geometry_2024",
    "regionalepi_state_boundaries_2024",
    "regionalepi_demographic_snapshot_v1",
    "regionalepi_demographic_snapshot_v2",
    "regionalepi_demographic_snapshot_v3",
    "regionalepi_demographic_snapshot_v4",
    "regionalepi_demographic_snapshot_v5"
  )
  direct_lookup <- paste0(
    "(?:get|get0)\\s*\\([^)]*\"(?:",
    paste(data_names, collapse = "|"),
    ")\""
  )
  expect_false(grepl(direct_lookup, server_text, perl = TRUE))
  expect_false(grepl('asNamespace\\("regionalepi"\\)', server_text,
    perl = TRUE))
  expect_match(server_text, "regionalepi:::.regionalepi_geography_resources()",
    fixed = TRUE)
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
  expect_identical(sum(calls == "addGeoJSON"), 2L)
  expect_length(widget$x$fitBounds, 5L)
  expect_equal(unlist(widget$x$fitBounds[1:4]),
    unname(bounds[c("lat1", "lng1", "lat2", "lng2")]))
  hook <- widget$jsHooks$render[[1L]]$code
  expect_match(hook, "color:'#F4F4F0', weight:0.45", fixed = TRUE)
  expect_match(hook, "fillOpacity:0.88", fixed = TRUE)
  expect_match(hook, "weight: p.geo_id === id ? 3.25 : 0.45", fixed = TRUE)
  expect_match(hook, "fillOpacity: p.geo_id === id ? 0.94 : 0.88", fixed = TRUE)
  expect_match(hook, "layer.bindTooltip", fixed = TRUE)
  expect_match(hook, "p.state_name", fixed = TRUE)
  exterior <- jsonlite::fromJSON(
    regionalepi:::.shiny_germany_outline_geojson(), simplifyVector = FALSE)
  expect_identical(length(exterior$features), 1L)
  expect_identical(exterior$features[[1L]]$geometry$type, "MultiPolygon")
  expect_identical(exterior$features[[1L]]$properties$role,
    "Germany exterior outline")
})

test_that("district heatmap grouping derives boundaries for every k", {
  expect_identical(regionalepi:::.shiny_incidence_heatmap_colours(),
    c("#F7F7F9", "#DADAE4", "#9C9EB5", "#5F627B", "#DD7F02"))
  expect_identical(regionalepi:::.shiny_signed_heatmap_colours(),
    c("#5F627B", "#F7F7F7", "#C86600"))
  for(k in 2:5) {
    x <- data.frame(geo_id=sprintf("%05d",1:(k*2)),geo_name=LETTERS[1:(k*2)],
      cluster_id=rep(sprintf("C%02d",1:k),each=2),stringsAsFactors=FALSE)
    layout <- regionalepi:::.shiny_district_heatmap_layout(x,"00003")
    expect_identical(length(layout$boundaries),k-1L)
    expect_identical(layout$selected_position,3L)
    expect_identical(layout$rows$cluster_id,
      rep(sprintf("C%02d",1:k),each=2))
    source<-x[rep(seq_len(nrow(x)),each=2L),,drop=FALSE]
    source$date<-rep(as.Date("2025-01-06")+c(0,7),times=nrow(x))
    source$incidence<-seq_len(nrow(source));source$incidence[[2L]]<-NA_real_
    source$row_position<-layout$rows$row_position[
      match(source$geo_id,layout$rows$geo_id)]
    grid<-regionalepi:::.shiny_heatmap_grid(source,"row_position","date","incidence")
    expect_identical(as.integer(grid$rows),layout$rows$row_position)
    expect_true(anyNA(grid$z))
  }
  expect_identical(regionalepi:::.shiny_format_number(9453),"9.453")
  expect_identical(regionalepi:::.shiny_format_number(16.28,2L),"16,28")
})

test_that("final district-week traces preserve every synthetic cell coordinate", {
  skip_if_not_installed("plotly")
  ids <- sprintf("%05d", 1:5)
  assignments <- data.frame(geo_id = ids, raw_cluster = c(1L,1L,2L,2L,2L),
    display_cluster_id = c("C01","C01","C02","C02","C02"))
  fit <- list(mode = "dynamic", provenance = list(fit_id = "synthetic"),
    assignments = assignments,
    profiles = data.frame(display_cluster_id=rep(c("C01","C02"),each=3),
      indicator_id=rep(c("population_density","mean_age",
        "youth_dependency_ratio"),2),standardized_center=c(-1,1,0,1,-1,0)))
  metadata <- regionalepi:::.shiny_cluster_display_metadata(fit,"dynamic")
  dates <- as.Date("2025-01-06") + 7*0:2
  data <- expand.grid(geo_id=ids,date=dates,KEEP.OUT.ATTRS=FALSE,
    stringsAsFactors=FALSE)
  data$geo_name <- paste0("District ", data$geo_id)
  data$cluster_id <- assignments$display_cluster_id[
    match(data$geo_id, assignments$geo_id)]
  data$incidence <- seq_len(nrow(data)); data$incidence[[6L]] <- 0
  data$incidence[c(3L,12L)] <- NA_real_
  data$cases <- 100 + seq_len(nrow(data)); data$cases[[12L]] <- NA_real_
  data$reporting_year <- 2025L
  data$reporting_week <- rep(1:3, each=length(ids))
  audit <- regionalepi:::.shiny_district_heatmap_data(data,metadata,"00003")
  widget <- plotly::plotly_build(
    regionalepi:::.shiny_district_heatmap_widget(audit))
  traces <- widget$x$data
  expect_identical(vapply(traces, `[[`, character(1L), "type"),
    c("scatter","heatmap","heatmap"))
  expect_identical(dim(traces[[2L]]$z), c(5L,3L))
  expect_identical(traces[[2L]]$xgap, 1)
  expect_identical(traces[[2L]]$ygap, 0)
  expect_equal(traces[[2L]]$z, audit$incidence, ignore_attr = TRUE)
  expect_equal(traces[[2L]]$customdata, audit$customdata, ignore_attr = TRUE)
  expect_identical(is.na(traces[[3L]]$z), !audit$missing)
  expect_equal(unname(traces[[1L]]$marker$color),
    audit$districts$display_colour, ignore_attr = TRUE)
  for (i in seq_len(nrow(data))) {
    row <- match(data$geo_id[[i]], audit$districts$geo_id)
    column <- match(data$date[[i]], audit$weeks$date)
    expect_equal(traces[[2L]]$z[row,column],data$incidence[[i]])
    expect_equal(audit$cases[row,column],data$cases[[i]])
    expect_identical(audit$missing[row,column],is.na(data$incidence[[i]]))
    expect_identical(traces[[2L]]$customdata[row,column],
      paste(data$geo_id[[i]],data$date[[i]],sep="|"))
  }
  expect_identical(audit$selected_position,3L)
  expect_identical(audit$boundaries,2.5)
  expect_true(all(audit$blocks$last_row-audit$blocks$first_row+1L==audit$blocks$n))
})

test_that("actual server path switches pathogens without an invalid window", {
  skip_if_not_installed("shiny")
  app <- regionalepi:::.shiny_app_dir()
  env <- new.env(parent=asNamespace("shiny"))
  sys.source(file.path(app,"server.R"),envir=env)
  shiny::testServer(env$server, {
    session$setInputs(pathogen="Influenza, saisonal",
      window_id="influenza_2025_26",range_mode="reviewed")
    session$flushReact()
    expect_identical(selected_window()$pathogen,"Influenza, saisonal")
    session$setInputs(window_id="influenza_2022_23",range_mode="reviewed")
    session$flushReact()
    expect_gt(nrow(reviewed_periods()),0L)
    session$setInputs(reviewed_period_id=reviewed_periods()$period_id[[1L]])
    session$flushReact()
    expect_identical(analysis_range()$mode,"reviewed")
    session$setInputs(window_id="influenza_2020_21")
    session$flushReact()
    expect_identical(nrow(reviewed_periods()),0L)
    expect_identical(selection()$range_mode,"window")
    expect_no_error(analysis_range())
    expect_identical(analysis_range()$mode,"window")
    session$setInputs(window_id="influenza_2025_26",range_mode="reviewed")
    session$flushReact()
    session$setInputs(pathogen="COVID-19")
    session$flushReact()
    expect_identical(selected_window()$pathogen,"COVID-19")
    expect_identical(selected_window()$observation_window_id,"covid19_2026_27")
    expect_identical(nrow(reviewed_periods()),0L)
    expect_no_error(analysis_range())
    expect_identical(analysis_range()$mode,"window")
    session$setInputs(window_id="covid19_2024_25")
    session$flushReact()
    expect_identical(selected_window()$observation_window_id,"covid19_2024_25")
    expect_gt(nrow(reviewed_periods()),0L)
    session$setInputs(pathogen="Norovirus-Gastroenteritis",range_mode="reviewed")
    session$flushReact()
    expect_identical(selected_window()$pathogen,"Norovirus-Gastroenteritis")
    expect_identical(selected_window()$observation_window_id,"norovirus_2026_27")
    expect_identical(nrow(reviewed_periods()),0L)
    expect_identical(analysis_range()$mode,"window")
    session$setInputs(pathogen="Influenza, saisonal")
    session$flushReact()
    expect_identical(selected_window()$pathogen,"Influenza, saisonal")
    expect_true(selected_window()$observation_window_id %in%
      unname(regionalepi:::.shiny_window_choices("Influenza, saisonal")))
    expect_null(state$error, info=state$technical_error)
    expect_identical(state$retrieval_count,0L)
  })
})

test_that("running-window load cannot reuse a stale Influenza period", {
  skip_if_not_installed("shiny")
  app <- regionalepi:::.shiny_app_dir()
  env <- new.env(parent=asNamespace("shiny"))
  sys.source(file.path(app,"server.R"),envir=env)
  map <- regionalepi_map_geometry()
  calls <- 0L
  observed_ranges <- list()
  fake_analysis <- function(pathogen, years, resources, source_mode,
                            effective_range) {
    calls <<- calls + 1L
    observed_ranges[[calls]] <<- effective_range
    data <- data.frame(
      geo_id = map$features$geo_id,
      geo_name = map$features$geo_name,
      date = as.Date("2026-09-28"), reporting_year = 2026L,
      reporting_week = 40L, incidence = 1, cases = 1,
      incidence_source = 1, incidence_annual_average = 1,
      population_year = 2025L, incidence_status = "provisional",
      stringsAsFactors = FALSE)
    list(data=data,diagnostics=list(
      exact_key_equality=TRUE,status="provisional",
      population_source_mode="snapshot",
      status_by_reporting_year=data.frame(
        reporting_year=2026L,population_year=2025L,
        incidence_status="provisional",stringsAsFactors=FALSE)),
      provenance=list(incidence=list(),source_incidence=list(),counts=list(),
        population=list(population=list(data_status="synthetic"))),
      timings=numeric())
  }
  testthat::local_mocked_bindings(
    .shiny_fetch_analysis_bundle=fake_analysis,
    .shiny_analysis_as_of_date=function(...) as.Date("2026-10-06"),
    .package="regionalepi")
  shiny::testServer(env$server, {
    rendered <- function(x) {
      if(is.list(x) && "html" %in% names(x)) return(x$html)
      paste(as.character(x),collapse="")
    }
    session$setInputs(pathogen="Influenza, saisonal",
      window_id="influenza_2025_26",range_mode="reviewed",
      typology_mode="dynamic",demographic_period="2022–2025",k="3",
      demographic_source="snapshot")
    session$flushReact()
    old_period <- reviewed_periods()$period_id[[1L]]
    session$setInputs(reviewed_period_id=old_period)
    session$flushReact()
    expect_identical(analysis_range()$mode,"reviewed")

    session$setInputs(window_id="influenza_2026_27")
    session$flushReact()
    expect_identical(selected_window()$observation_window_id,
      "influenza_2026_27")
    expect_identical(selection()$range_mode,"window")
    expect_true(is.na(selection()$reviewed_period_id))
    expect_identical(nrow(reviewed_periods()),0L)
    expect_null(reviewed_period())
    expect_identical(analysis_range()$mode,"window")
    expect_identical(input$typology_mode,"dynamic")
    expect_identical(input$demographic_period,"2022–2025")
    expect_identical(input$k,"3")
    expect_identical(input$demographic_source,"snapshot")
    expect_identical(unname(custom_week_selection()$choices),
      c("2026-KW40","2026-KW41"))
    session$setInputs(range_mode="custom",custom_start_week="2026-KW40",
      custom_end_week="2026-KW41")
    session$flushReact()
    expect_identical(analysis_range()$start_date,as.Date("2026-09-28"))
    expect_identical(analysis_range()$end_date,as.Date("2026-10-11"))
    session$setInputs(range_mode="window")
    session$flushReact()

    session$setInputs(load_analysis=1)
    session$flushReact()
    expect_null(state$error, info=state$technical_error)
    expect_identical(state$result$window$observation_window_id,
      "influenza_2026_27")
    expect_identical(state$result$analysis_range$nominal_start_date,
      as.Date("2026-09-28"))
    expect_identical(state$result$analysis_range$nominal_end_date,
      as.Date("2027-05-23"))
    expect_match(rendered(output$range_status),
      "Laufender Beobachtungszeitraum: bis zum aktuellen Analyse-Stichtag.",
      fixed=TRUE)
    expect_match(rendered(output$range_status),"status-info",fixed=TRUE)
    expect_false(grepl("erst 2 Kalenderwochen",
      rendered(output$range_status),fixed=TRUE))
    plot_note <- rendered(output$incidence_early_window_note)
    expect_match(plot_note,paste(
      "Früher Beobachtungsstand: Der Beobachtungszeitraum 2026/27 hat in KW 40",
      "begonnen und umfasst bis zum Analyse-Stichtag erst 2 Kalenderwochen.",
      "Zeitliche Verläufe sind daher noch eingeschränkt interpretierbar."),
      fixed=TRUE)
    expect_false(grepl("SurvStat",plot_note,fixed=TRUE))
    expect_identical(calls,1L)

    session$setInputs(load_analysis=2)
    session$flushReact()
    expect_null(state$error)
    expect_identical(state$result$window$observation_window_id,
      "influenza_2026_27")
    expect_identical(state$result$analysis_range$mode,"window")
    expect_identical(calls,1L)

    session$setInputs(range_mode="custom",custom_start_week="2026-KW40",
      custom_end_week="2026-KW41",load_analysis=3)
    session$flushReact()
    expect_null(state$error)
    expect_identical(state$result$analysis_range$mode,"custom")
    expect_identical(state$result$analysis_range$nominal_end_date,
      as.Date("2026-10-11"))
    expect_identical(state$result$analysis_range$end_date,as.Date("2026-10-06"))
    range_text <- rendered(output$range_status)
    expect_match(range_text,"2026 \u2013 KW 40",fixed=TRUE)
    expect_match(range_text,"2026 \u2013 KW 41",fixed=TRUE)
    expect_match(range_text,"28.09.2026",fixed=TRUE)
    expect_match(range_text,"06.10.2026",fixed=TRUE)
    expect_false(grepl("11.10.2026",range_text,fixed=TRUE))
    expect_identical(calls,1L)

    session$setInputs(window_id="influenza_2025_26")
    session$flushReact()
    expect_gt(nrow(reviewed_periods()),0L)
    session$setInputs(range_mode="reviewed")
    session$flushReact()
    expect_identical(analysis_range()$mode,"reviewed")
    session$setInputs(window_id="influenza_2026_27")
    session$flushReact()
    expect_identical(selection()$range_mode,"window")
    expect_true(is.na(selection()$reviewed_period_id))
    expect_identical(analysis_range()$mode,"window")
  })
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
  expect_match(server_text, ".shiny_fetch_demography", fixed = TRUE)
  expect_match(server_text, "SurvStat-Fallzahlen und amtliche durchschnittliche Jahresbevölkerung", fixed = TRUE)
  expect_match(server_text, ".shiny_fetch_analysis_bundle", fixed = TRUE)
  expect_match(server_text, "Inzidenz auf Basis der durchschnittlichen Jahresbevölkerung", fixed = TRUE)
  expect_match(server_text, "Vorläufig: Für Berichtsjahr %d wird die zuletzt verfügbare amtliche durchschnittliche Jahresbevölkerung %d als Bezugsbevölkerung verwendet.", fixed = TRUE)
  expect_false(grepl("incidence_selector|Inzidenz auswählen", ui_text))
  expect_false(grepl(".shiny_fetch_surveillance_bundle",server_text,
    fixed=TRUE))
  expect_match(server_text, "mkey <- paste0(\"map:\"", fixed = TRUE)
  expect_false(grepl("fetch_surveillance_bundle",server_text,fixed=TRUE))
  expect_match(server_text, ".shiny_surveillance_cache_match", fixed = TRUE)
  expect_match(ui_text, "Erweiterte Einstellungen", fixed = TRUE)
  expect_match(ui_text,
    "Regionale Infektionssurveillance und demografische Typologien",
    fixed = TRUE)
  expect_match(server_text, "show.legend=FALSE", fixed = TRUE)
  expect_match(ui_text, "Darstellung, nicht den gewählten Analysezeitraum",
    fixed = TRUE)
  expect_match(ui_text, "Demografische Typologie", fixed = TRUE)
  expect_match(ui_text, "Standardisierte demografische Clusterprofile",
    fixed = TRUE)
  expect_identical(
    unname(regionalepi:::.shiny_pathogen_choices()[["Norovirus"]]),
    "Norovirus-Gastroenteritis"
  )
  expect_match(ui_text, "Regionale Analyse", fixed = TRUE)
  expect_match(ui_text, "Räumliche Vergleichsebene", fixed = TRUE)
  expect_match(ui_text, '"Großregionen" = "grossregion"', fixed = TRUE)
  expect_match(ui_text, '"grossregion"', fixed = TRUE)
  expect_false(grepl("Regionaltyp im regionalen Vergleich",ui_text,fixed=TRUE))
  expect_match(server_text,
    "Die Prozentwerte beziehen sich auf Kreise, nicht auf Bevölkerungsanteile.",
    fixed = TRUE)
  expect_match(ui_text, "sind nicht als kausale Effekte", fixed = TRUE)
  expect_match(server_text, "Infektionsgeschehen nach demografischem Regionaltyp", fixed = TRUE)
  expect_match(server_text, ".summarize_weekly_regional_incidence", fixed = TRUE)
  expect_match(ui_text, "bereits geladene reguläre Analyse", fixed = TRUE)
  expect_match(server_text,
    "Für Norovirus sind keine vordefinierten epidemiologischen Zeiträume hinterlegt.",
    fixed = TRUE)
  expect_match(server_text, "summarize_state_cluster_incidence", fixed = TRUE)
})

test_that("Shiny demographic snapshot is default and live mode is explicit", {
  app <- regionalepi:::.shiny_app_dir()
  ui_environment <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(app, "ui.R"), envir = ui_environment)
  html <- as.character(ui_environment$ui)
  ui_text <- paste(readLines(file.path(app, "ui.R"), warn = FALSE),
    collapse = "\n")
  expect_match(html, 'id="demographic_source_control"', fixed = TRUE)
  expect_match(html, 'id="custom_start_week"', fixed = TRUE)
  expect_match(html, 'id="custom_end_week"', fixed = TRUE)
  expect_match(html, "Start-KW", fixed = TRUE)
  expect_match(html, "End-KW", fixed = TRUE)
  expect_false(grepl('id="custom_range"',html,fixed=TRUE))
  expect_false(grepl("dateRangeInput",ui_text,fixed=TRUE))
  time_plot_position <- regexpr('plotlyOutput("incidence_plot"',ui_text,
    fixed=TRUE)[[1L]]
  early_note_position <- regexpr('uiOutput("incidence_early_window_note")',
    ui_text,fixed=TRUE)[[1L]]
  completeness_position <- regexpr('uiOutput("incidence_warning")',ui_text,
    fixed=TRUE)[[1L]]
  expect_true(time_plot_position < early_note_position)
  expect_true(early_note_position < completeness_position)
  expect_match(html,
    "regionalepi: Regionale Infektionssurveillance und demografische Typologien",
    fixed = TRUE)
  expect_match(html,"Demografische Typologie",fixed=TRUE)
  expect_match(html,"Infektionsgeschehen",fixed=TRUE)
  expect_false(grepl("Referenzanalyse",html,fixed=TRUE))
  expect_match(ui_text, "--re-primary:#5F627B", fixed = TRUE)
  expect_match(ui_text, "--re-action:#DD7F02", fixed = TRUE)
  expect_match(ui_text,"--re-primary-soft:#ECECF2",fixed=TRUE)

  testthat::local_mocked_bindings(
    .shiny_fetch_demography = function(...) stop("live retrieval used"),
    .package = "regionalepi"
  )
  current <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)
  historical <- regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)
  expect_identical(current$source_mode, "snapshot")
  expect_identical(historical$source_mode, "snapshot")
  expect_identical(regionalepi:::.shiny_demography_status(current),
    paste("Geprüfter Snapshot – Typologie-Datenstand 09.10.2026;",
          "Jahresdurchschnittsbevölkerung 08.10.2026"))
  expect_identical(nrow(current$summary$data), 1200L)
  expect_identical(nrow(historical$summary$data), 1203L)

  testthat::local_mocked_bindings(
    .shiny_regional_credentials_available=function(...) FALSE,
    .shiny_fetch_surveillance_bundle=function(...) list(
      timings=numeric(),resolution=list()),
    fetch_regional_average_population=function(...)
      stop("live annual-average population retrieval used"),
    prepare_analysis_incidence=function(
      surveillance,annual_average_population,provisional_denominator_years) {
      expect_identical(sort(unique(annual_average_population$data$year)),2024:2025)
      expect_identical(provisional_denominator_years,c("2026"=2025L))
      list(data=data.frame(),diagnostics=list(status="provisional"),
        provenance=list(),source_incidence=surveillance)
    },
    .package="regionalepi"
  )
  expect_identical(
    regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)$source_mode,
    "snapshot"
  )
  snapshot_bundle <- regionalepi:::.shiny_fetch_analysis_bundle(
    "Influenza, saisonal", 2025:2026, list(), "snapshot"
  )
  expect_identical(snapshot_bundle$diagnostics$population_source_mode,
    "snapshot")
  expect_identical(snapshot_bundle$diagnostics$status,"provisional")
  expect_error(regionalepi:::.shiny_fetch_analysis_bundle(
      "Influenza, saisonal", 2025L, list(), "live"),
    "annual-average-population incidence requires configured Regionaldatenbank credentials",
    fixed=TRUE
  )
})

test_that("Regionaldatenbank availability is non-secret and requires both credentials", {
  skip_if_not_installed("shiny")
  available <- function(user, password) {
    values <- c(REGIONALSTATISTIK_USER=user,
      REGIONALSTATISTIK_PASSWORD=password)
    regionalepi:::.shiny_regional_credentials_available(
      function(name, unset="") if(name %in% names(values)) values[[name]] else unset)
  }
  expect_true(available("user","password"))
  expect_false(available("","password"))
  expect_false(available("user",""))
  expect_false(available("",""))
  expect_false(available("   ","password"))
  expect_false(available("user","\t"))
  server_text<-paste(readLines(file.path(regionalepi:::.shiny_app_dir(),
    "server.R"),warn=FALSE),collapse="\n")
  expect_match(server_text,"Live-Abruf Regionaldatenbank",fixed=TRUE)
  expect_match(server_text,"Demografische Daten für die Typologie",fixed=TRUE)
  expect_match(server_text,"Live-Abruf Regionaldatenbank (Zugangsdaten erforderlich)",
    fixed=TRUE)
  expect_match(server_text,"aria-disabled','true'",fixed=TRUE)
  expect_match(server_text,"if(v==='live')s.setValue('snapshot')",fixed=TRUE)
  expect_match(server_text,"Geprüfter Snapshot (empfohlen)",fixed=TRUE)
  expect_match(server_text,"REGIONALSTATISTIK_USER und REGIONALSTATISTIK_PASSWORD",
    fixed=TRUE)
  expect_match(server_text,
    "Der geprüfte Snapshot unterstützt die vollständige dynamische Analyse",
    fixed=TRUE)
  expect_match(server_text,
    "Der geprüfte Snapshot enthält sowohl die demografischen Daten der Typologie als auch die amtliche durchschnittliche Jahresbevölkerung",
    fixed=TRUE)
  expect_false(grepl("synthetic-user|synthetic-password",server_text,fixed=TRUE))
  ui_text<-paste(readLines(file.path(regionalepi:::.shiny_app_dir(),
    "ui.R"),warn=FALSE),collapse="\n")
  expect_false(grepl("demographisch|Demographisch|Demographie",paste(
    ui_text,server_text),perl=TRUE))

  app <- regionalepi:::.shiny_app_dir()
  server_environment <- new.env(parent=asNamespace("shiny"))
  sys.source(file.path(app,"server.R"),envir=server_environment)
  testthat::local_mocked_bindings(
    .shiny_regional_credentials_available=function(...) FALSE,
    .package="regionalepi"
  )
  shiny::testServer(server_environment$server,{
    session$setInputs(pathogen="Influenza, saisonal",
      window_id="influenza_2025_26",range_mode="window")
    session$flushReact()
    rendered <- output$demographic_source_control
    rendered <- if(is.list(rendered)&&"html"%in%names(rendered))
      rendered$html else paste(as.character(rendered),collapse="")
    expect_match(rendered,"Demografische Daten für die Typologie",fixed=TRUE)
    expect_match(rendered,"Geprüfter Snapshot (empfohlen)",fixed=TRUE)
    expect_match(rendered,"value=\"snapshot\" selected",fixed=TRUE)
    expect_match(rendered,
      "Live-Abruf Regionaldatenbank (Zugangsdaten erforderlich)",fixed=TRUE)
    expect_match(rendered,"aria-disabled','true'",fixed=TRUE)
    expect_match(rendered,"if(v==='live')s.setValue('snapshot')",fixed=TRUE)
    expect_match(rendered,
      "Der geprüfte Snapshot unterstützt die vollständige dynamische Analyse",
      fixed=TRUE)
  })
})

test_that("reviewed small-n regional display geometry remains unchanged", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)$summary
  fit <- regionalepi:::.shiny_fit_typology(summary, "dynamic", 3L)
  assignments <- regionalepi:::.shiny_current_display_assignments(
    regionalepi:::.shiny_map_assignments(regionalepi_map_geometry(), fit))
  crosswalk <- regionalepi_district_state_crosswalk()
  assignments <- merge(assignments, crosswalk[c("geo_id", "state_id")],
    by = "geo_id", sort = FALSE)
  expected <- list(
    `01` = c(C01 = 5L, C02 = 6L, C03 = 4L),
    `16` = c(C01 = 17L, C02 = 3L, C03 = 2L)
  )
  expected_rules <- list(
    `01` = c(C01 = "points_and_boxplot", C02 = "points_and_boxplot",
      C03 = "points_and_median"),
    `16` = c(C01 = "points_boxplot_and_violin", C02 = "points_and_median",
      C03 = "points_and_median")
  )
  for (state_id in names(expected)) {
    counts <- table(factor(assignments$cluster_id[
      assignments$state_id == state_id], levels = names(expected[[state_id]])))
    expect_identical(stats::setNames(as.integer(counts), names(counts)),
      expected[[state_id]])
    rules <- vapply(as.integer(counts), function(n)
      regionalepi:::.shiny_finite_n_display(seq_len(n))$display_rule,
      character(1L))
    expect_identical(stats::setNames(rules, names(counts)),
      expected_rules[[state_id]])
  }
})

test_that("regional relative-activity and legend UI semantics are explicit", {
  app<-regionalepi:::.shiny_app_dir()
  ui_text<-paste(readLines(file.path(app,"ui.R"),warn=FALSE),collapse="\n")
  server_text<-paste(readLines(file.path(app,"server.R"),warn=FALSE),collapse="\n")
  expect_match(server_text,"regional_relative_activity",fixed=TRUE)
  expect_match(ui_text,"Vordefinierter epidemiologischer Zeitraum",fixed=TRUE)
  expect_false(grepl("Geprüfter epidemiologischer Zeitraum",ui_text,fixed=TRUE))
  expect_match(server_text,".regional_relative_activity",fixed=TRUE)
  expect_match(server_text,'type="heatmap"',fixed=TRUE)
  expect_match(server_text,"plot_bgcolor=\"#D1D5DB\"",fixed=TRUE)
  expect_false(grepl("normalized_activity",server_text,fixed=TRUE))
  expect_match(server_text,"show.legend=FALSE",fixed=TRUE)
  expect_false(grepl("peak-week|cross-correlation|centre of mass",server_text))
})

test_that("final user-facing terminology is localized and current", {
  app <- regionalepi:::.shiny_app_dir()
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE),
    collapse = "\n")
  visualization_text <- paste(
    deparse(body(regionalepi:::.shiny_state_composition_widget)),
    deparse(body(regionalepi:::.shiny_state_comparison_widget)),
    collapse = "\n"
  )

  expect_match(server_text,
    "Start- und End-KW können innerhalb des Beobachtungszeitraums gewählt werden.",
    fixed = TRUE)
  expect_match(server_text, "Vorläufig: Für Berichtsjahr %d", fixed = TRUE)
  expect_match(visualization_text, "Bundesweit bestimmter Regionaltyp: %s",
    fixed = TRUE)
  expect_match(visualization_text, "Demografischer Regionaltyp", fixed = TRUE)
  expect_false(grepl("Nationaler Cluster", visualization_text, fixed = TRUE))
})

test_that("Pass-B nested information architecture and theme are explicit", {
  app <- regionalepi:::.shiny_app_dir()
  ui_text <- paste(readLines(file.path(app,"ui.R"),warn=FALSE),collapse="\n")
  server_text <- paste(readLines(file.path(app,"server.R"),warn=FALSE),
    collapse="\n")
  expect_match(ui_text,'tabsetPanel(id = "typology_view"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Clusterprofile"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Stabilität"',fixed=TRUE)
  expect_match(ui_text,'h4("Verteilungen der Kreise")',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Veränderungen 2017–2025"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Methodik und Daten"',fixed=TRUE)
  expect_match(ui_text,'uiOutput("methods_page")',fixed=TRUE)
  expect_false(grepl('tabsetPanel(id = "methods_view"',ui_text,fixed=TRUE))
  expect_match(ui_text,"Paarweise Clusterunterschiede",fixed=TRUE)
  expect_match(ui_text,"Regionengruppen aus Ländern",fixed=TRUE)
  expect_match(ui_text,"--re-primary:#5F627B",fixed=TRUE)
  expect_match(ui_text,"--re-action:#DD7F02",fixed=TRUE)
  expect_match(ui_text,"--re-action-hover:#C46F00",fixed=TRUE)
  expect_match(ui_text,"color:#181D22",fixed=TRUE)
  expect_false(grepl("#4183C4",ui_text,fixed=TRUE))
  expect_match(ui_text,
    "-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Arial,sans-serif",
    fixed=TRUE)
  expect_match(server_text,"output$methods_page",fixed=TRUE)
  expect_match(server_text,'tags$summary("Technische Provenienz")',fixed=TRUE)
  expect_match(server_text,"Die methodische Konzeption basiert auf der demografischen Regionaltypologie von ",fixed=TRUE)
  expect_match(server_text,"Die demografische Typologie für 2017–2020 reproduziert die Kreiszuordnungen der ursprünglichen Dissertationstypologie.",fixed=TRUE)
  expect_gte(length(gregexpr("https://doi.org/10.17169/refubium-51449",
    server_text,fixed=TRUE)[[1L]]),2L)
  expect_false(grepl("Paper 1",server_text,fixed=TRUE))
  expect_match(server_text,"REGIONALSTATISTIK_USER",fixed=TRUE)
  expect_match(server_text,"REGIONALSTATISTIK_PASSWORD",fixed=TRUE)
  expect_match(server_text,"Methodik und Daten → Datenquellen → Regionaldatenbank",fixed=TRUE)
  expect_match(server_text,"Geografischer Referenzstand:",fixed=TRUE)
  expect_match(server_text,
    'format(cache$map$provenance$source_vintage)',fixed=TRUE)
  expect_false(grepl("Die kanonischen Kreisidentitäten",server_text,fixed=TRUE))
  expect_false(grepl("Methodik & Daten",paste(ui_text,server_text),fixed=TRUE))
  expect_match(server_text,"erlauben keine kausalen Aussagen",fixed=TRUE)
  expect_false(grepl('uiOutput("provenance")',ui_text,fixed=TRUE))
})

test_that("regional relative-activity heatmap grid is complete ordered and value preserving", {
  dates<-as.Date("2024-01-01")+7L*0:2
  x<-expand.grid(date=dates,cluster_id=c("C01","C02"),stringsAsFactors=FALSE)
  x$median_incidence<-c(0,2,4,NA,0,5)
  x$regional_reference_median<-c(1,1,1,2,2,2)
  x$regional_relative_activity<-c(-1,0,3,NA,-2,3)
  x$observed_districts<-c(3L,3L,3L,0L,3L,3L)
  x$expected_districts<-3L;x$missing_districts<-3L-x$observed_districts
  x$completeness<-x$observed_districts/3
  x$regional_observed_districts<-c(3L,3L,3L,2L,2L,2L)
  x$regional_expected_districts<-3L
  x$regional_missing_districts<-3L-x$regional_observed_districts
  x$regional_completeness<-x$regional_observed_districts/3
  metadata<-data.frame(display_cluster_id=c("C02","C01"),
    display_label=c("C02","C01"),profile_description=c("Profil 2","Profil 1"),
    stringsAsFactors=FALSE)
  grid<-regionalepi:::.shiny_regional_temporal_heatmap_data(x,metadata)
  expect_identical(grid$rows,c("C02","C01"))
  expect_identical(grid$columns,dates)
  expect_identical(nrow(grid$data),6L)
  expect_setequal(as.numeric(grid$z),x$regional_relative_activity)
  expect_true(any(is.na(grid$z)))
  expect_true(any(grid$z==0,na.rm=TRUE))
  expect_match(paste(grid$text,collapse=" "),"Cluster-Wochenmedian",fixed=TRUE)
  expect_match(paste(grid$text,collapse=" "),"Region gesamt",fixed=TRUE)
  expect_match(paste(grid$text,collapse=" "),"Keine beobachtbare relative Aktivität",fixed=TRUE)
  expect_error(regionalepi:::.shiny_regional_temporal_heatmap_data(
    rbind(x,x[1L,]),metadata),"complete unique")
  expect_error(regionalepi:::.shiny_regional_temporal_heatmap_data(
    x[-1L,],metadata),"complete unique")
})

test_that("regional relative-activity heatmap retains k2 to k5 and historical order", {
  for(ids in list(sprintf("C%02d",1:2),sprintf("C%02d",1:3),
                  sprintf("C%02d",1:4),sprintf("C%02d",1:5),
                  c("ClD","ClJ","ClA"))) {
    x<-expand.grid(date=as.Date("2024-01-01")+7L*0:1,
      cluster_id=ids,stringsAsFactors=FALSE)
    x$median_incidence<-rep(c(0,2),length(ids))
    x$regional_reference_median<-1
    x$regional_relative_activity<-rep(c(-1,1),length(ids))
    x$observed_districts<-2L;x$expected_districts<-2L
    x$missing_districts<-0L;x$completeness<-1
    x$regional_observed_districts<-2L;x$regional_expected_districts<-2L
    x$regional_missing_districts<-0L;x$regional_completeness<-1
    metadata<-data.frame(display_cluster_id=ids,display_label=ids,
      profile_description=paste("Profil",ids),stringsAsFactors=FALSE)
    grid<-regionalepi:::.shiny_regional_temporal_heatmap_data(x,metadata)
    expect_identical(grid$rows,ids)
    expect_identical(dim(grid$z),c(length(ids),2L))
  }
})

test_that("regional context styles are deterministic and role-distinct", {
  names<-c("Süden","Mitte (West)","Norden (West)","Osten","Deutschland")
  first<-regionalepi:::.shiny_regional_context_styles(names,"Süden")
  second<-regionalepi:::.shiny_regional_context_styles(names,"Süden")
  expect_identical(first,second)
  expect_identical(first$comparison_name,names)
  expect_identical(first$linetype[first$comparison_name=="Süden"],"solid")
  expect_identical(first$linetype[first$comparison_name=="Deutschland"],"longdash")
  comparisons<-first[!first$comparison_name%in%c("Süden","Deutschland"),]
  expect_identical(length(unique(comparisons$colour)),3L)
  expect_identical(length(unique(comparisons$linetype)),3L)
  expect_gt(first$linewidth[first$comparison_name=="Süden"],max(comparisons$linewidth))
  expect_error(regionalepi:::.shiny_regional_context_styles(
    c(names,"Zusatz"),"Süden"),"at most three")
})

test_that("named Plotly legend cleanup removes technical tuple suffixes", {
  skip_if_not_installed("plotly");skip_if_not_installed("ggplot2")
  x<-data.frame(date=rep(as.Date("2024-01-01")+0:1,2),value=1:4,
    region=rep(c("Süden","Deutschland"),each=2),
    role=rep(c("Fokal","Referenz"),each=2))
  graph<-ggplot2::ggplot(x,ggplot2::aes(date,value,colour=region,
    linetype=role,group=region))+ggplot2::geom_line()
  widget<-regionalepi:::.shiny_clean_named_legend(
    plotly::ggplotly(graph),c("Süden","Deutschland"))
  visible<-vapply(widget$x$data,function(trace)!identical(trace$showlegend,FALSE),logical(1))
  names<-vapply(widget$x$data[visible],function(trace)
    if(is.null(trace$name))"" else trace$name,character(1))
  expect_setequal(names,c("Süden","Deutschland"))
  expect_false(any(grepl("[(),]",names)))
})

test_that("absolute regional IQR legend has one clean entry per cluster", {
  skip_if_not_installed("plotly");skip_if_not_installed("ggplot2")
  x<-expand.grid(date=as.Date("2024-01-01")+0:2,
    cluster_id=c("C01","C02","C03"),stringsAsFactors=FALSE)
  x$median<-seq_len(nrow(x));x$q1<-x$median-1;x$q3<-x$median+1
  graph<-ggplot2::ggplot(x,ggplot2::aes(date,median,colour=cluster_id,
    fill=cluster_id,group=cluster_id))+ggplot2::geom_ribbon(
      ggplot2::aes(ymin=q1,ymax=q3),colour=NA,show.legend=FALSE)+
    ggplot2::geom_line()
  widget<-regionalepi:::.shiny_finalize_incidence_legend(plotly::ggplotly(graph))
  visible<-vapply(widget$x$data,function(trace)!identical(trace$showlegend,FALSE),logical(1))
  names<-vapply(widget$x$data[visible],function(trace)
    if(is.null(trace$name))"" else trace$name,character(1))
  expect_identical(sort(unique(names)),c("C01","C02","C03"))
  expect_identical(length(names),3L)
  expect_false(any(grepl("[(),]",names)))
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
    definition_version = "test_v1", variant_context = NA_character_,
    historical_context = "synthetic", evidence_class = "REVIEWED_TEST_PERIOD",
    source_reference = "synthetic",
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

test_that("linked display state reuses one source bundle and one typology fit", {
  skip_if_not_installed("shiny")
  app <- regionalepi:::.shiny_app_dir()
  server_environment <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(app, "server.R"), envir = server_environment)
  map <- regionalepi_map_geometry()
  dates <- as.Date(c("2025-10-06", "2025-10-13"))
  grid <- expand.grid(geo_id = map$features$geo_id, date = dates,
                      stringsAsFactors = FALSE)
  grid$geo_name <- map$features$geo_name[match(grid$geo_id, map$features$geo_id)]
  grid$reporting_year <- 2025L
  grid$reporting_week <- as.integer(format(grid$date, "%V"))
  grid$incidence <- seq_len(nrow(grid)) / 100
  grid$cases <- as.numeric(seq_len(nrow(grid)) %% 7L)
  calls <- 0L
  fake_bundle <- function(...) {
    calls <<- calls + 1L
    list(data = grid, diagnostics = list(exact_key_equality = TRUE),
         provenance = list(incidence = list(), counts = list()), timings = numeric())
  }
  fake_analysis_bundle <- function(...) {
    calls <<- calls + 1L
    data <- grid
    data$incidence_source <- data$incidence + 100
    data$incidence_annual_average <- data$incidence
    data$population_year <- 2025L
    data$incidence_status <- "final"
    list(data = data, diagnostics = list(
      exact_key_equality = TRUE, status = "final",
      population_source_mode = "snapshot",
      status_by_reporting_year = data.frame(
        reporting_year = 2025L, population_year = 2025L,
        incidence_status = "final", stringsAsFactors = FALSE
      )), provenance = list(
        incidence = list(), source_incidence = list(), counts = list(),
        population = list(population = list(data_status = "synthetic-population"))
      ), timings = numeric())
  }
  testthat::local_mocked_bindings(
    .shiny_fetch_surveillance_bundle = fake_bundle,
    .shiny_fetch_analysis_bundle = fake_analysis_bundle,
    .shiny_regional_credentials_available = function() TRUE,
    .package = "regionalepi"
  )
  shiny::testServer(server_environment$server, {
    rendered <- function(x) {
      if (is.list(x) && "html" %in% names(x)) return(x$html)
      paste(as.character(x), collapse = "")
    }
    rendered_text <- function(x) trimws(gsub("\\s+", " ",
      gsub("<[^>]+>", " ", rendered(x))))
    session$setInputs(pathogen="Influenza, saisonal",window_id="influenza_2025_26",
      range_mode="window",
      typology_mode="dynamic",demographic_period="2022–2025",
      k="3",density_scale="original",district_overlay=TRUE,
      regional_level="grossregion",load_analysis=1)
    session$flushReact()
    expect_null(state$error)
    expect_match(state$result$cache_keys$demographic,"snapshot",fixed=TRUE)
    expect_identical(calls, 1L)
    expect_identical(state$result$bundle$data$incidence,
      state$result$bundle$data$incidence_annual_average)
    expect_false(identical(state$result$bundle$data$incidence,
      state$result$bundle$data$incidence_source))
    expect_match(rendered(output$regional_analysis_context),
      "Demografische Typologie · 2022–2025 · k=3", fixed = TRUE)
    expect_match(rendered(output$regional_analysis_context),
      "Inzidenz auf Basis der durchschnittlichen Jahresbevölkerung",
      fixed = TRUE)
    expect_match(rendered(output$analysis_heading), "Demografische Typologie", fixed = TRUE)
    expect_match(rendered(output$analysis_heading), "k=3", fixed = TRUE)
    expect_match(rendered_text(output$active_demographic_status),
      "Demografische Typologie · 2022–2025 · k=3",fixed=TRUE)
    expect_match(rendered_text(output$active_demographic_status),
      "Für die angezeigte Analyse verwendet:",fixed=TRUE)
    expect_no_error(output$analysis_xlsx)
    expect_no_error(output$analysis_weekly_csv)
    expect_no_error(output$analysis_district_csv)
    expect_no_error(output$analysis_weekly_png)
    expect_no_error(output$analysis_district_png)
    selected_week<-as.character(exploration()$expected_dates[[1L]])
    session$setInputs(district_distribution_scope="week",
      distribution_week=selected_week)
    session$flushReact()
    expect_no_error(output$weekly_distribution_status)
    expect_no_error(output$weekly_distribution_plot)
    expect_no_error(output$analysis_week_distribution_png)
    expect_identical(calls,1L)
    expect_false(grepl("Vorläufig:", rendered(output$analysis_wide_status), fixed = TRUE))
    fit_id <- state$result$fit$provenance$fit_id
    fixed_incidence <- state$result$bundle$data$incidence
    fixed_assignments <- state$result$fit$assignments
    fixed_weekly <- exploration()$weekly
    fixed_relative <- exploration()$weekly$relative_activity
    fixed_membership <- regional_membership()
    session$setInputs(k="4")
    session$flushReact()
    expect_match(rendered(output$analysis_wide_status),
      "Auswahl geändert – Analyse aktualisieren", fixed = TRUE)
    expect_match(rendered(output$analysis_wide_status),
      "zuvor geladenen Analyse", fixed = TRUE)
    expect_match(rendered_text(output$active_demographic_status),
      "Demografische Typologie · 2022–2025 · k=4",fixed=TRUE)
    expect_match(rendered_text(output$active_demographic_status),
      "Für die angezeigte Analyse verwendet:",fixed=TRUE)
    expect_match(rendered_text(output$active_demographic_status),
      "Demografische Typologie · 2022–2025 · k=3",fixed=TRUE)
    expect_match(rendered(output$regional_analysis_context),
      "Demografische Typologie · 2022–2025 · k=3", fixed = TRUE)
    expect_identical(state$result$bundle$data$incidence, fixed_incidence)
    expect_identical(state$result$fit$assignments, fixed_assignments)
    expect_identical(exploration()$weekly, fixed_weekly)
    expect_identical(exploration()$weekly$relative_activity, fixed_relative)
    expect_identical(regional_membership(), fixed_membership)
    session$setInputs(k="3",load_analysis=2)
    session$flushReact()
    expect_false(grepl("Auswahl geändert", rendered(output$analysis_wide_status),
      fixed = TRUE))
    expect_identical(calls, 1L)
    provisional_result <- state$result
    provisional_result$bundle$diagnostics$status_by_reporting_year$incidence_status <-
      "provisional"
    provisional_result$bundle$diagnostics$status_by_reporting_year$reporting_year <-
      2026L
    provisional_result$bundle$diagnostics$status_by_reporting_year$population_year <-
      2025L
    state$result <- provisional_result
    session$flushReact()
    expect_match(rendered(output$analysis_wide_status),
      "Vorläufig: Für Berichtsjahr 2026 wird die zuletzt verfügbare amtliche durchschnittliche Jahresbevölkerung 2025 als Bezugsbevölkerung verwendet.",
      fixed = TRUE)
    expect_false(grepl("provisional", rendered(output$analysis_wide_status),
      fixed = TRUE))
    final_result <- state$result
    final_result$bundle$diagnostics$status_by_reporting_year$incidence_status <-
      "final"
    state$result <- final_result
    session$flushReact()
    expect_no_error(output$methods_page)
    expect_false(grepl("Vorläufig", rendered(output$analysis_wide_status),
      fixed = TRUE))
    original_distribution <- output$demographic_distribution_plot
    session$setInputs(density_scale="log10")
    session$flushReact()
    expect_false(identical(output$demographic_distribution_plot,
      original_distribution))
    expect_identical(calls,1L)
    expect_identical(state$result$fit$provenance$fit_id,fit_id)
    state$selected_geo_id <- "01001"
    session$setInputs(range_mode="custom",custom_start_week="2025-KW41",
      custom_end_week="2025-KW42",analysis_section="typology")
    session$flushReact()
    expect_identical(calls, 1L)
    expect_identical(state$result$fit$provenance$fit_id, fit_id)
    expect_identical(state$selected_geo_id, "01001")
    expect_identical(nrow(exploration()$data), 800L)
    session$setInputs(regional_focal="south",regional_comparisons=c("east"))
    session$flushReact()
    expect_no_error(output$regional_composition_plot)
    expect_no_error(output$regional_demographic_distribution_plot)
    expect_no_error(output$regional_cluster_time_plot)
    expect_no_error(output$regional_relative_activity_plot)
    expect_no_error(output$regional_context_time_plot)
    expect_no_error(output$regional_district_plot)
    expect_identical(calls, 1L)
    expect_identical(state$result$fit$provenance$fit_id, fit_id)
    relative<-regional_relative_activity()
    expect_equal(relative$regional_relative_activity,
      relative$median_incidence-relative$regional_reference_median)
    expect_identical(relative$observed_districts,
      regional_cluster_weekly()$observed_districts)
    expect_true(all(relative$regional_expected_districts>=
      relative$regional_observed_districts))
    session$setInputs(regional_view="Kreise",regional_density_scale="log10")
    session$flushReact()
    expect_no_error(output$regional_demographic_distribution_plot)
    expect_identical(calls,1L)
    expect_identical(state$result$fit$provenance$fit_id,fit_id)
    session$setInputs(regional_level="state",regional_focal="09")
    session$flushReact()
    expect_no_error(output$regional_composition_plot)
    expect_identical(calls, 1L)
    expect_identical(state$result$fit$provenance$fit_id, fit_id)
    session$setInputs(regional_level="aggregated_state")
    session$flushReact()
    expect_no_error(output$regional_composition_plot)
    expect_no_error(output$regional_context_time_plot)
    session$setInputs(regional_level="grossregion")
    session$flushReact()
    expect_no_error(output$regional_composition_plot)
    expect_no_error(output$regional_context_time_plot)
    expect_identical(calls, 1L)
    expect_identical(state$result$fit$provenance$fit_id, fit_id)

    regular_fit_id<-state$result$fit$provenance$fit_id
    session$setInputs(typology_mode="dissertation")
    session$flushReact()
    expect_identical(state$result$fit$provenance$fit_id,regular_fit_id)
    expect_true(all(grepl("^C0[1-3]$",
      state$result$map_join$data$display_cluster_id)))
    expect_true("incidence_annual_average"%in%names(state$result$bundle$data))
    expect_identical(calls,1L)

    session$setInputs(pathogen="COVID-19",window_id="covid19_2020_21",
      range_mode="custom",custom_start_week="2020-KW20",
      custom_end_week="2021-KW20")
    session$flushReact()
    expect_null(state$result)
    expect_identical(calls,1L)
  })
})

test_that("demographic startup is offline and independent of surveillance controls", {
  skip_if_not_installed("shiny")
  app<-regionalepi:::.shiny_app_dir()
  server_environment<-new.env(parent=asNamespace("shiny"))
  sys.source(file.path(app,"server.R"),envir=server_environment)
  retrievals<-0L
  testthat::local_mocked_bindings(
    .shiny_fetch_surveillance_bundle=function(...){retrievals<<-retrievals+1L;stop("unexpected retrieval")},
    .shiny_fetch_analysis_bundle=function(...){retrievals<<-retrievals+1L;stop("unexpected retrieval")},
    .package="regionalepi")
  shiny::testServer(server_environment$server,{
    session$setInputs(demographic_period="2022–2025",k="3",
      demographic_source="snapshot",pathogen="Influenza, saisonal",
      window_id="influenza_2025_26",range_mode="window",
      regional_level="grossregion")
    session$flushReact()
    expect_null(demographic_state$error)
    expect_false(is.null(demographic_state$result))
    expect_identical(demographic_state$result$demographic_years,2022:2025)
    expect_identical(demographic_state$result$configuration$k,3L)
    expect_false(is.null(demographic_state$transition))
    expect_identical(demographic_state$transition_compute_count,1L)
    transition<-demographic_state$transition$result
    expect_identical(transition$diagnostics$compared_districts,400L)
    expect_identical(transition$diagnostics$unchanged_districts,369L)
    expect_identical(transition$diagnostics$changed_districts,31L)
    transition_before<-serialize(transition,NULL)
    expect_no_error(output$typology_xlsx)
    expect_no_error(output$typology_csv)
    expect_no_error(output$typology_png)
    expect_no_error(output$transition_xlsx)
    expect_no_error(output$transition_csv)
    expect_no_error(output$transition_png)
    expect_no_error(output$transition_summary)
    expect_no_error(output$transition_matrix)
    expect_no_error(output$transition_sankey)
    expect_no_error(output$transition_map)
    expect_no_error(output$transition_district_detail)
    expect_identical(retrievals,0L)
    fit_count<-demographic_state$fit_count
    transition_count<-demographic_state$transition_compute_count
    session$setInputs(show_typology=1,analysis_section="typology",
      typology_view="map_districts")
    session$flushReact()
    expect_identical(demographic_state$fit_count,fit_count)
    expect_identical(demographic_state$transition_compute_count,
      transition_count)
    expect_identical(retrievals,0L)
    session$setInputs(pathogen="COVID-19",window_id="covid19_2025_26",
      analysis_section="regional",selected_geo_id="01001",
      transition_filter="changed",transition_matrix_mode="row_percentage")
    session$flushReact()
    expect_identical(demographic_state$fit_count,fit_count)
    expect_identical(demographic_state$transition_compute_count,1L)
    expect_identical(serialize(demographic_state$transition$result,NULL),
      transition_before)
    expect_identical(retrievals,0L)
    expect_null(state$result)

    session$setInputs(
      demographic_period="Benutzerdefinierter Referenzzeitraum",
      demographic_start_year="2021",demographic_end_year="2022")
    session$flushReact()
    expect_null(demographic_state$error)
    expect_identical(demographic_state$result$demographic_years,2021:2022)
    expect_identical(
      demographic_state$result$reference_selection$selection_type,"custom")
    expect_identical(
      demographic_state$result$reference_selection$solution_review_status,
      "not_individually_reviewed")
    expect_true(
      demographic_state$result$reference_selection$census_comparability_notice)
    expect_identical(retrievals,0L)
    custom_fit_count<-demographic_state$fit_count
    session$setInputs(pathogen="Norovirus-Gastroenteritis",
      window_id="norovirus_2025_26")
    session$flushReact()
    expect_identical(demographic_state$fit_count,custom_fit_count)
    expect_identical(retrievals,0L)
  })
})
