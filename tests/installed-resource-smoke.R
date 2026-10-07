library(regionalepi)

map <- regionalepi_map_geometry()
validate_map_geometry_resource(map)
stopifnot(
  identical(nrow(map$features), 400L),
  identical(map$provenance$resource_id, "regionalepi_map_geometry_2024")
)

states <- regionalepi_state_boundaries()
validate_state_boundaries_resource(states)
stopifnot(
  identical(nrow(states$features), 16L),
  identical(states$provenance$resource_id,
            "regionalepi_state_boundaries_2024")
)

crosswalk <- regionalepi_district_state_crosswalk()
validate_district_state_crosswalk(crosswalk)
stopifnot(
  identical(nrow(crosswalk), 400L),
  identical(unique(crosswalk$definition_version), "district_state_2024_v1")
)

groups <- regionalepi_state_comparison_groups()
validate_state_comparison_groups(groups)
stopifnot(
  identical(nrow(groups), 16L),
  identical(length(unique(groups$comparison_group_id)), 12L),
  identical(unique(groups$definition_version), "aggregated_states_12_v1")
)

snapshot <- regionalepi_demographic_snapshot()
validate_demographic_snapshot(snapshot)
stopifnot(
  identical(snapshot$provenance$snapshot_id,
            "regionalepi_demography_996c38455f53c59c"),
  identical(snapshot$diagnostics$checksum_verified, TRUE),
  identical(snapshot$diagnostics$component_row_counts,
            c(mean_age = 3204L, youth_dependency = 3204L,
              population = 3204L, area = 3204L,
              annual_average_population = 1600L))
)
