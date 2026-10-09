loadNamespace("regionalepi")

map <- regionalepi::regionalepi_map_geometry()
regionalepi::validate_map_geometry_resource(map)
stopifnot(
  identical(nrow(map$features), 400L),
  identical(map$provenance$resource_id, "regionalepi_map_geometry_2024")
)

states <- regionalepi::regionalepi_state_boundaries()
regionalepi::validate_state_boundaries_resource(states)
stopifnot(
  identical(nrow(states$features), 16L),
  identical(states$provenance$resource_id,
            "regionalepi_state_boundaries_2024")
)

crosswalk <- regionalepi::regionalepi_district_state_crosswalk()
regionalepi::validate_district_state_crosswalk(crosswalk)
stopifnot(
  identical(nrow(crosswalk), 400L),
  identical(unique(crosswalk$definition_version), "district_state_2024_v1")
)

groups <- regionalepi::regionalepi_state_comparison_groups()
regionalepi::validate_state_comparison_groups(groups)
stopifnot(
  identical(nrow(groups), 16L),
  identical(length(unique(groups$comparison_group_id)), 12L),
  identical(unique(groups$definition_version), "aggregated_states_12_v1")
)

snapshot <- regionalepi::regionalepi_demographic_snapshot()
regionalepi::validate_demographic_snapshot(snapshot)
stopifnot(
  identical(snapshot$provenance$snapshot_id,
            "regionalepi_demography_47bd90242e6c148e"),
  identical(snapshot$diagnostics$checksum_verified, TRUE),
  identical(snapshot$diagnostics$component_row_counts,
            c(mean_age = 3604L, youth_dependency = 3604L,
              population = 3604L, area = 3604L,
              annual_average_population = 3600L))
)

transition_from <- regionalepi::fit_dynamic_typology(
  regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary, k = 3L
)
transition_to <- regionalepi::fit_dynamic_typology(
  regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)$summary, k = 3L
)
transition <- regionalepi::compare_dynamic_typology_transitions(
  transition_from, transition_to, map$features$geo_id
)
expected_transition <- matrix(
  c(113L, 8L, 3L, 14L, 193L, 6L, 0L, 0L, 63L),
  nrow = 3L, byrow = TRUE,
  dimnames = list(
    from = c("older", "family", "dense"),
    to = c("older", "family", "dense")
  )
)
stopifnot(
  identical(as.integer(transition$count_matrix),
            as.integer(expected_transition)),
  identical(dim(transition$count_matrix), dim(expected_transition)),
  identical(transition$diagnostics$compared_districts, 400L),
  identical(transition$diagnostics$unchanged_districts, 369L),
  identical(transition$diagnostics$changed_districts, 31L),
  identical(transition$diagnostics$from_fit_only_geo_ids, "16056")
)

if (requireNamespace("shiny", quietly = TRUE)) {
  app_directory <- system.file(
    "shiny", "regionalepi", package = "regionalepi", mustWork = TRUE
  )
  server_environment <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(app_directory, "server.R"), envir = server_environment)
  shiny::testServer(server_environment$server, {
    session$setInputs(
      demographic_period = "2022–2025", k = "3",
      demographic_source = "snapshot",
      pathogen = "Influenza, saisonal",
      window_id = "influenza_2025_26", range_mode = "window",
      regional_level = "grossregion"
    )
    session$flushReact()
    stopifnot(
      identical(cache$map$provenance$resource_id,
                "regionalepi_map_geometry_2024"),
      identical(cache$states$provenance$resource_id,
                "regionalepi_state_boundaries_2024"),
      identical(nrow(cache$crosswalk), 400L),
      identical(resources$provenance$resource_id,
                "regionalepi_geography_resources_2024"),
      identical(nrow(regionalepi::regionalepi_state_comparison_groups()), 16L),
      identical(regionalepi::regionalepi_demographic_snapshot()$provenance$snapshot_id,
                "regionalepi_demography_47bd90242e6c148e"),
      !is.null(demographic_state$result),
      identical(demographic_state$result$demographic_years, 2022:2025),
      identical(demographic_state$result$configuration$k, 3L),
      !is.null(demographic_state$transition),
      identical(demographic_state$transition_compute_count, 1L),
      identical(demographic_state$transition$result$diagnostics$compared_districts,
                400L),
      identical(demographic_state$transition$result$diagnostics$unchanged_districts,
                369L),
      identical(demographic_state$transition$result$diagnostics$changed_districts,
                31L),
      !is.null(output$typology_xlsx),
      !is.null(output$transition_xlsx),
      identical(state$retrieval_count, 0L),
      is.null(state$result)
    )
    session$setInputs(
      demographic_period = "Benutzerdefinierter Referenzzeitraum",
      demographic_start_year = "2021", demographic_end_year = "2022"
    )
    session$flushReact()
    stopifnot(
      is.null(demographic_state$error),
      identical(demographic_state$result$demographic_years, 2021:2022),
      identical(demographic_state$result$reference_selection$selection_type,
                "custom"),
      identical(demographic_state$result$reference_selection$solution_review_status,
                "not_individually_reviewed"),
      identical(state$retrieval_count, 0L)
    )
  })
}

demographic <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)
fit <- regionalepi:::.shiny_fit_typology(demographic$summary, "dynamic", 3L)
historical_window_ids <- paste0("influenza_", 2017:2021, "_", 18:22)
for (window_id in historical_window_ids) {
  window <- regionalepi:::.shiny_selected_window(
    "Influenza, saisonal", window_id
  )
  reporting_years <- seq.int(
    as.integer(format(window$start_date, "%Y")),
    as.integer(format(window$end_date, "%Y"))
  )
  population <- regionalepi:::.shiny_population_for_reporting_years(
    reporting_years, "snapshot"
  )
  stopifnot(
    identical(population$supported_years, reporting_years),
    !length(population$unsupported_years),
    !length(population$provisional)
  )
}
reference <- regionalepi:::.shiny_fit_typology(
  regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary,
  "dissertation", 3L
)
reference_map <- regionalepi:::.shiny_map_assignments(map, reference)
reference_metadata <- regionalepi:::.shiny_cluster_display_metadata(
  reference, "dissertation", "neutral", NULL, reference_map$data$geo_id
)
known_ids <- c("11000", "12073", "14521", "03453", "03460", "16063")
known_clusters <- c("ClD", "ClA", "ClA", "ClJ", "ClJ", "ClA")
stopifnot(
  identical(as.integer(table(factor(
    reference$assignments$display_cluster_id,
    levels = c("ClD", "ClJ", "ClA")
  ))), c(63L, 213L, 125L)),
  identical(as.integer(table(factor(
    reference_map$data$display_cluster_id,
    levels = c("ClD", "ClJ", "ClA")
  ))), c(63L, 213L, 124L)),
  identical(reference_metadata$n_districts, c(63L, 213L, 124L)),
  identical(
    reference_map$data$display_cluster_id[
      match(known_ids, reference_map$data$geo_id)
    ],
    known_clusters
  )
)
stability_fits <- lapply(2:5, function(k) {
  regionalepi:::.shiny_fit_typology(demographic$summary, "dynamic", k)
})
stability <- regionalepi:::.shiny_typology_stability(stability_fits)
stability_policy <- regionalepi:::.shiny_dynamic_colour_policy(
  stability_fits[[2L]], reference, stability_fits[[2L]]
)
stability_metadata <- regionalepi:::.shiny_cluster_display_metadata(
  stability_fits[[2L]], "dynamic", "profile_aligned", stability_policy,
  regionalepi::regionalepi_map_geometry()$features$geo_id
)
palette_alignment <- regionalepi:::.shiny_dynamic_colour_policy(
  fit, reference, fit
)
stopifnot(
  identical(sort(unique(fit$assignments$display_cluster_id)),
            c("C01", "C02", "C03")),
  identical(length(stability_fits), 4L),
  identical(sort(unique(stability_metadata$display_cluster_id)),
            c("C01", "C02", "C03")),
  is.list(stability),
  is.list(palette_alignment),
  identical(names(palette_alignment$colours), c("C01", "C02", "C03"))
)
