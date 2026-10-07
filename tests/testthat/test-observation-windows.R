test_that("observation windows have exact reviewed ISO boundaries", {
  x <- regionalepi_observation_windows()
  expect_invisible(validate_observation_windows(x))
  flu <- x[x$observation_window_id == "influenza_2025_26", ]
  expect_identical(flu$start_date, as.Date("2025-09-29"))
  expect_identical(flu$end_date, as.Date("2026-05-17"))
  covid <- x[x$observation_window_id == "covid19_2024_25", ]
  expect_identical(covid$start_date, as.Date("2024-05-13"))
  expect_identical(covid$end_date, as.Date("2025-05-18"))
  expect_match(covid$note, "not an official RKI", fixed = TRUE)
  norovirus <- x[x$observation_window_id == "norovirus_2025_26", ]
  expect_identical(norovirus$pathogen, "Norovirus-Gastroenteritis")
  expect_identical(norovirus$label,
    "Norovirus-Beobachtungszeitraum 2025/26")
  expect_identical(norovirus$start_date, as.Date("2025-06-30"))
  expect_identical(norovirus$end_date, as.Date("2026-06-28"))
  expect_identical(norovirus$start_iso_week, 27L)
  expect_identical(norovirus$end_iso_week, 26L)
  expect_match(norovirus$note, "occurs year-round", fixed = TRUE)
  expect_match(norovirus$note, "no reviewed national Norovirus wave", fixed = TRUE)
  running <- x[x$observation_window_id %in% c(
    "influenza_2026_27", "covid19_2026_27", "norovirus_2026_27"
  ), ]
  expect_identical(running$start_date, as.Date(c(
    "2026-09-28", "2026-05-11", "2026-06-29"
  )))
  expect_identical(running$end_date, as.Date(c(
    "2027-05-23", "2027-05-23", "2027-07-04"
  )))
  expect_true(all(grepl("laufend", running$label, fixed = TRUE)))
})

test_that("observation-window validation rejects undocumented assumptions", {
  x <- regionalepi_observation_windows()[1, ]
  bad <- x; bad$start_date <- bad$start_date + 1L
  expect_error(validate_observation_windows(bad), "ISO-week boundaries")
  bad <- x; bad$window_type <- "wave"
  expect_error(validate_observation_windows(bad), "window type")
  bad <- rbind(x, x)
  expect_error(validate_observation_windows(bad), "IDs unique")
})

test_that("three analysis ranges remain explicit and bounded", {
  window <- regionalepi:::.shiny_selected_window("Influenza, saisonal", "influenza_2025_26")
  whole <- regionalepi:::.shiny_select_analysis_range(window, "window")
  expect_identical(whole$start_date, window$start_date)
  period <- regionalepi:::.shiny_periods_in_window(window)[1, ]
  reviewed <- regionalepi:::.shiny_select_analysis_range(window, "reviewed", period)
  expect_identical(reviewed$start_date, as.Date("2025-11-24"))
  custom <- regionalepi:::.shiny_select_analysis_range(window, "custom",
    custom_dates = as.Date(c("2026-01-05", "2026-02-01")))
  expect_identical(custom$label, "Benutzerdefinierter Analysezeitraum")
  expect_error(regionalepi:::.shiny_select_analysis_range(window, "custom",
    custom_dates = as.Date(c("2024-12-30", "2026-02-01"))), "au\u00dferhalb")
  expect_error(regionalepi:::.shiny_select_analysis_range(window, "reviewed"),
    "No reviewed")
})

test_that("running ranges use an injectable analysis cutoff", {
  ids <- c("influenza_2026_27", "covid19_2026_27", "norovirus_2026_27")
  for (id in ids) {
    pathogen <- regionalepi_observation_windows()$pathogen[
      match(id, regionalepi_observation_windows()$observation_window_id)
    ]
    window <- regionalepi:::.shiny_selected_window(pathogen, id)
    nominal <- regionalepi:::.shiny_select_analysis_range(window, "window")
    effective <- regionalepi:::.shiny_effective_analysis_range(
      nominal, as.Date("2026-10-06")
    )
    expect_identical(effective$end_date, as.Date("2026-10-06"))
    expect_identical(effective$analysis_as_of_date, as.Date("2026-10-06"))
    expect_identical(effective$nominal_end_date, window$end_date)
    expect_identical(seq.int(
      as.integer(format(effective$start_date, "%Y")),
      as.integer(format(effective$end_date, "%Y"))
    ), 2026L)
  }
  expect_identical(regionalepi:::.shiny_analysis_as_of_date(
    function() as.Date("2026-10-06")
  ), as.Date("2026-10-06"))
})

test_that("early running-window note reports calendar weeks reached", {
  window <- regionalepi:::.shiny_selected_window(
    "Influenza, saisonal", "influenza_2026_27")
  range <- regionalepi:::.shiny_select_analysis_range(window, "window")
  first <- regionalepi:::.shiny_effective_analysis_range(
    range, as.Date("2026-09-28"))
  expect_identical(regionalepi:::.shiny_early_window_note(window, first),
    paste0("Der Beobachtungszeitraum 2026/27 hat in KW 40 begonnen und umfasst ",
      "bis zum Analyse-Stichtag erst 1 Kalenderwoche. Zeitliche Verl\u00e4ufe sind ",
      "daher noch eingeschr\u00e4nkt interpretierbar."))
  second <- regionalepi:::.shiny_effective_analysis_range(
    range, as.Date("2026-10-06"))
  expect_match(regionalepi:::.shiny_early_window_note(window, second),
    "umfasst bis zum Analyse-Stichtag erst 2 Kalenderwochen", fixed = TRUE)
  fifth <- regionalepi:::.shiny_effective_analysis_range(
    range, as.Date("2026-10-26"))
  expect_match(regionalepi:::.shiny_early_window_note(window, fifth),
    "erst 5 Kalenderwochen", fixed = TRUE)
  sixth <- regionalepi:::.shiny_effective_analysis_range(
    range, as.Date("2026-11-02"))
  expect_null(regionalepi:::.shiny_early_window_note(window, sixth))

  completed <- regionalepi:::.shiny_selected_window(
    "Influenza, saisonal", "influenza_2025_26")
  completed_range <- regionalepi:::.shiny_effective_analysis_range(
    regionalepi:::.shiny_select_analysis_range(completed, "window"),
    as.Date("2026-10-06"))
  expect_null(regionalepi:::.shiny_early_window_note(
    completed, completed_range))
})

test_that("custom ranges require explicit complete ISO calendar weeks", {
  window <- regionalepi:::.shiny_selected_window(
    "Influenza, saisonal", "influenza_2026_27")
  expect_error(regionalepi:::.shiny_select_analysis_range(window, "custom",
    custom_dates = as.Date(c("2026-09-30", "2026-10-05"))),
    "complete ISO calendar weeks", fixed = TRUE)
  one <- regionalepi:::.shiny_select_custom_week_range(
    window, "2026-KW40", "2026-KW40", as.Date("2026-10-06"))
  expect_identical(one$start_date, as.Date("2026-09-28"))
  expect_identical(one$end_date, as.Date("2026-10-04"))
  multiple <- regionalepi:::.shiny_select_custom_week_range(
    window, "2026-KW40", "2026-KW41", as.Date("2026-10-06"))
  expect_identical(multiple$start_date, as.Date("2026-09-28"))
  expect_identical(multiple$end_date, as.Date("2026-10-11"))
  effective <- regionalepi:::.shiny_effective_analysis_range(
    multiple, as.Date("2026-10-06"))
  expect_identical(effective$end_date, as.Date("2026-10-06"))
  expect_identical(effective$nominal_end_date, as.Date("2026-10-11"))
})

test_that("custom ISO-week choices respect windows and running cutoffs", {
  running <- regionalepi:::.shiny_selected_window(
    "Influenza, saisonal", "influenza_2026_27")
  choices <- regionalepi:::.shiny_custom_week_choices(
    running, as.Date("2026-10-06"))
  expect_identical(names(choices), c("2026 \u2013 KW 40", "2026 \u2013 KW 41"))
  expect_identical(unname(choices), c("2026-KW40", "2026-KW41"))
  expect_false("2026-KW42" %in% choices)

  completed <- regionalepi:::.shiny_selected_window(
    "Influenza, saisonal", "influenza_2025_26")
  historical <- regionalepi:::.shiny_custom_week_choices(
    completed, as.Date("2026-10-06"))
  expect_identical(unname(historical)[[1L]], "2025-KW40")
  expect_identical(utils::tail(unname(historical), 1L), "2026-KW20")
  cross_year <- regionalepi:::.shiny_select_custom_week_range(
    completed, "2025-KW52", "2026-KW01", as.Date("2026-10-06"))
  expect_identical(cross_year$start_date, as.Date("2025-12-22"))
  expect_identical(cross_year$end_date, as.Date("2026-01-04"))
})

test_that("custom ISO-week state is retained or reconciled deterministically", {
  window <- regionalepi:::.shiny_selected_window(
    "Influenza, saisonal", "influenza_2026_27")
  retained <- regionalepi:::.shiny_reconcile_custom_weeks(
    window, "2026-KW40", "2026-KW41", as.Date("2026-10-06"))
  expect_identical(retained$start_week, "2026-KW40")
  expect_identical(retained$end_week, "2026-KW41")
  start_changed <- regionalepi:::.shiny_reconcile_custom_weeks(
    window, "2026-KW41", "2026-KW40", as.Date("2026-10-06"), "start")
  expect_identical(start_changed$end_week, "2026-KW41")
  end_changed <- regionalepi:::.shiny_reconcile_custom_weeks(
    window, "2026-KW41", "2026-KW40", as.Date("2026-10-06"), "end")
  expect_identical(end_changed$start_week, "2026-KW40")
  reset <- regionalepi:::.shiny_reconcile_custom_weeks(
    window, "2025-KW40", "2026-KW41", as.Date("2026-10-06"))
  expect_identical(reset$start_week, "2026-KW40")
  expect_identical(reset$end_week, "2026-KW41")
})

test_that("COVID 2025/26 remains available without an invented reviewed wave", {
  window <- regionalepi:::.shiny_selected_window("COVID-19", "covid19_2025_26")
  expect_identical(nrow(regionalepi:::.shiny_periods_in_window(window)), 0L)
  expect_no_error(regionalepi:::.shiny_select_analysis_range(window, "window"))
})

test_that("Norovirus windows have no invented reviewed period", {
  choices <- regionalepi:::.shiny_window_choices("Norovirus-Gastroenteritis")
  expect_identical(length(choices), 10L)
  expect_true("norovirus_2025_26" %in% unname(choices))
  window <- regionalepi:::.shiny_selected_window(
    "Norovirus-Gastroenteritis", "norovirus_2025_26"
  )
  expect_identical(nrow(regionalepi:::.shiny_periods_in_window(window)), 0L)
  expect_length(regionalepi:::.shiny_period_choices("Norovirus-Gastroenteritis"), 0L)
  reconciled <- regionalepi:::.shiny_reconcile_selection(
    "Norovirus-Gastroenteritis", "influenza_2025_26", "reviewed"
  )
  expect_identical(reconciled$window_id, "norovirus_2026_27")
  expect_identical(reconciled$range_mode, "window")
})

test_that("exploration summaries preserve missing counts and all-district estimand", {
  dates <- as.Date("2025-01-06") + rep(c(0, 7), each = 4)
  data <- data.frame(geo_id = rep(sprintf("%05d", 1:4), 2), geo_name = rep(LETTERS[1:4], 2),
    date = dates, reporting_year = 2025L, reporting_week = rep(1:2, each = 4),
    incidence = c(1, 3, 5, 7, 2, NA, 6, 8), cases = c(1, 2, NA, 4, 2, NA, 3, 4),
    stringsAsFactors = FALSE)
  fit <- list(assignments = data.frame(geo_id=sprintf("%05d",1:4),
      display_cluster_id=rep(c("C01","C02"),each=2)),
    provenance=list(fit_id="fit"), indicator_set=list(definition_version="v1"))
  range <- list(start_date=min(dates),end_date=max(dates))
  result <- regionalepi:::.shiny_exploration_summaries(list(data=data),fit,range)
  expect_equal(result$weekly$all_district_median[result$weekly$date==min(dates)][1],4)
  district <- result$district_period[result$district_period$geo_id=="00003",]
  expect_identical(district$cumulative_observed_cases, 3)
  expect_identical(district$missing_case_week_count, 1L)
  grid <- regionalepi:::.shiny_heatmap_grid(result$data,"geo_id","date","incidence")
  expect_true(is.na(grid$z["00002", "2025-01-13"]))
  expect_false(isTRUE(grid$z["00002", "2025-01-13"] == 0))
})

test_that("dynamic descriptions are versioned deterministic display metadata", {
  profiles <- data.frame(display_cluster_id=rep("C01",3),
    indicator_id=c("population_density","mean_age","youth_dependency_ratio"),
    standardized_center=c(1.2,-.8,.1))
  x <- regionalepi:::.shiny_profile_descriptions(profiles)
  expect_identical(x$description_specification_id,"dynamic_profile_descriptions_v2")
  expect_match(x$profile_description,"deutlich höhere Bevölkerungsdichte",fixed=TRUE)
  expect_match(x$profile_description,"niedrigeres Durchschnittsalter",fixed=TRUE)
  expect_null(regionalepi:::.shiny_profile_descriptions(profiles,"dissertation"))
})

test_that("reviewed Bundesland boundary resource is exact and display-only", {
  x <- regionalepi_state_boundaries()
  expect_invisible(validate_state_boundaries_resource(x))
  expect_identical(nrow(x$features),16L)
  expect_identical(sort(x$features$geo_id),sprintf("%02d",1:16))
  expect_identical(x$provenance$source_layer,"vg2500_lan")
  expect_identical(x$provenance$source_crs,"EPSG:25832")
  expect_identical(x$provenance$output_crs,"EPSG:4326")
  expect_match(x$provenance$selection,"no simplification",fixed=TRUE)
})
