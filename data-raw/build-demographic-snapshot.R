if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The demographic snapshot builder requires pkgload.", call. = FALSE)
}

pkgload::load_all(".", quiet = TRUE)

prior_snapshot <- regionalepi_demographic_snapshot()
prior_components <- regionalepi:::.snapshot_reconstruct_components(prior_snapshot)
relations <- utils::read.csv(
  "data-raw/reviewed/annual-average-population-relations-2017-2020.csv",
  colClasses = "character", stringsAsFactors = FALSE
)
relations$year <- as.integer(relations$year)
map_ids <- regionalepi_map_geometry()$features$geo_id
latest_population <- prior_snapshot$data$population[
  prior_snapshot$data$population$reference_date == as.Date("2025-12-31"),
  c("geo_id", "geo_name"), drop = FALSE
]
if (!setequal(latest_population$geo_id, map_ids)) {
  stop(
    "The reviewed 2025 population and current map district ID sets differ.",
    call. = FALSE
  )
}
target_geography <- latest_population
annual_average_population_source <- fetch_regional_average_population(2017:2025)
annual_average_population <- regionalepi:::.harmonize_reviewed_annual_average_population(
  annual_average_population_source, relations, target_geography
)
components <- list(
  mean_age = prior_components$mean_age,
  youth_dependency = prior_components$youth,
  population = prior_components$population,
  area = prior_components$area,
  annual_average_population = annual_average_population
)

component_statuses <- vapply(names(components), function(name) {
  regionalepi:::.snapshot_component_status(components[[name]], name)
}, character(1L))
cat("Component source statuses:\n")
print(component_statuses)

regionalepi_demographic_snapshot_v4 <- regionalepi:::.build_demographic_snapshot(
  components,
  snapshot_version = paste0(
    "regionaldatenbank_2017-2020_2022-2025_",
    "annual-average-population_2017-2025_v4"
  ),
  prior_snapshot_id = prior_snapshot$provenance$snapshot_id
)

dir.create("data", showWarnings = FALSE)
save(
  regionalepi_demographic_snapshot_v4,
  file = "data/regionalepi_demographic_snapshot_v4.rda",
  compress = "xz",
  version = 3L
)

regionalepi::validate_demographic_snapshot(
  regionalepi_demographic_snapshot_v4
)

cat("Built", regionalepi_demographic_snapshot_v4$provenance$snapshot_id, "\n")
cat("Source status:",
    regionalepi_demographic_snapshot_v4$provenance$source_data_status, "\n")
cat("Rows:", paste(
  names(regionalepi_demographic_snapshot_v4$data),
  vapply(regionalepi_demographic_snapshot_v4$data, nrow, integer(1L)),
  collapse = "; "), "\n")
cat("Object bytes:", as.numeric(object.size(
  regionalepi_demographic_snapshot_v4)), "\n")
