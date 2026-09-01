if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The demographic snapshot builder requires pkgload.", call. = FALSE)
}

pkgload::load_all(".", quiet = TRUE)

required_years <- c(2017:2020, 2022:2024)
reference_dates <- as.Date(sprintf("%d-12-31", required_years))
retrieval_dates <- as.Date(sprintf("%d-12-31", 2017:2024))

components <- list(
  mean_age = fetch_regional_mean_age(retrieval_dates),
  youth_dependency = fetch_regional_youth_dependency(retrieval_dates),
  population = fetch_regional_population(retrieval_dates),
  area = fetch_regional_area(retrieval_dates)
)

for (name in names(components)) {
  keep <- components[[name]]$data$reference_date %in% reference_dates
  components[[name]]$data <- components[[name]]$data[keep, , drop = FALSE]
}

component_statuses <- vapply(names(components), function(name) {
  regionalepi:::.snapshot_component_status(components[[name]], name)
}, character(1L))
cat("Component source statuses:\n")
print(component_statuses)

regionalepi_demographic_snapshot_v1 <- regionalepi:::.build_demographic_snapshot(
  components,
  snapshot_version = "regionaldatenbank_2017-2020_2022-2024_v1"
)

dir.create("data", showWarnings = FALSE)
save(
  regionalepi_demographic_snapshot_v1,
  file = "data/regionalepi_demographic_snapshot_v1.rda",
  compress = "xz",
  version = 3L
)

regionalepi::validate_demographic_snapshot(
  regionalepi_demographic_snapshot_v1
)

cat("Built", regionalepi_demographic_snapshot_v1$provenance$snapshot_id, "\n")
cat("Source status:",
    regionalepi_demographic_snapshot_v1$provenance$source_data_status, "\n")
cat("Rows:", paste(
  names(regionalepi_demographic_snapshot_v1$data),
  vapply(regionalepi_demographic_snapshot_v1$data, nrow, integer(1L)),
  collapse = "; "), "\n")
cat("Object bytes:", as.numeric(object.size(
  regionalepi_demographic_snapshot_v1)), "\n")
