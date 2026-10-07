if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The demographic snapshot builder requires pkgload.", call. = FALSE)
}

pkgload::load_all(".", quiet = TRUE)

required_years <- c(2017:2020, 2022:2025)
reference_dates <- as.Date(sprintf("%d-12-31", required_years))
retrieval_dates <- as.Date(sprintf("%d-12-31", 2017:2025))

components <- list(
  mean_age = fetch_regional_mean_age(retrieval_dates),
  youth_dependency = fetch_regional_youth_dependency(retrieval_dates),
  population = fetch_regional_population(retrieval_dates),
  area = fetch_regional_area(retrieval_dates),
  annual_average_population = fetch_regional_average_population(2022:2025)
)

for (name in c("mean_age", "youth_dependency", "population", "area")) {
  keep <- components[[name]]$data$reference_date %in% reference_dates
  components[[name]]$data <- components[[name]]$data[keep, , drop = FALSE]
}

component_statuses <- vapply(names(components), function(name) {
  regionalepi:::.snapshot_component_status(components[[name]], name)
}, character(1L))
cat("Component source statuses:\n")
print(component_statuses)

prior_snapshot <- get(
  "regionalepi_demographic_snapshot_v2", envir = asNamespace("regionalepi")
)
regionalepi_demographic_snapshot_v3 <- regionalepi:::.build_demographic_snapshot(
  components,
  snapshot_version = paste0(
    "regionaldatenbank_2017-2020_2022-2025_",
    "annual-average-population_2022-2025_v3"
  ),
  prior_snapshot_id = prior_snapshot$provenance$snapshot_id
)

dir.create("data", showWarnings = FALSE)
save(
  regionalepi_demographic_snapshot_v3,
  file = "data/regionalepi_demographic_snapshot_v3.rda",
  compress = "xz",
  version = 3L
)

regionalepi::validate_demographic_snapshot(
  regionalepi_demographic_snapshot_v3
)

cat("Built", regionalepi_demographic_snapshot_v3$provenance$snapshot_id, "\n")
cat("Source status:",
    regionalepi_demographic_snapshot_v3$provenance$source_data_status, "\n")
cat("Rows:", paste(
  names(regionalepi_demographic_snapshot_v3$data),
  vapply(regionalepi_demographic_snapshot_v3$data, nrow, integer(1L)),
  collapse = "; "), "\n")
cat("Object bytes:", as.numeric(object.size(
  regionalepi_demographic_snapshot_v3)), "\n")
