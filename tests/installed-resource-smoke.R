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
            "regionalepi_demography_996c38455f53c59c"),
  identical(snapshot$diagnostics$checksum_verified, TRUE),
  identical(snapshot$diagnostics$component_row_counts,
            c(mean_age = 3204L, youth_dependency = 3204L,
              population = 3204L, area = 3204L,
              annual_average_population = 1600L))
)

if (requireNamespace("shiny", quietly = TRUE)) {
  app_directory <- system.file(
    "shiny", "regionalepi", package = "regionalepi", mustWork = TRUE
  )
  server_environment <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(app_directory, "server.R"), envir = server_environment)
  shiny::testServer(server_environment$server, {
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
                "regionalepi_demography_996c38455f53c59c")
    )
  })
}

demographic <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)
fit <- regionalepi:::.shiny_fit_typology(demographic$summary, "dynamic", 3L)
reference <- regionalepi:::.shiny_fit_typology(
  regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary,
  "dissertation", 3L
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
