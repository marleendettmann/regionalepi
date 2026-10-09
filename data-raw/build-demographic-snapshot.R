if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The demographic snapshot builder requires pkgload.", call. = FALSE)
}

pkgload::load_all(".", quiet = TRUE)

prior_snapshot <- regionalepi:::.regionalepi_package_data(
  "regionalepi_demographic_snapshot_v4")
regionalepi::validate_demographic_snapshot(prior_snapshot)

# Parsed results are retained in the authenticated R session. If a later gate
# fails, rerunning this script reuses successful 2021 retrievals without
# persisting raw responses or credentials.
cache_name <- ".regionalepi_v5_2021_components"
if (!exists(cache_name, envir = .GlobalEnv, inherits = FALSE)) {
  assign(cache_name, list(), envir = .GlobalEnv)
}
retrieved_2021 <- get(cache_name, envir = .GlobalEnv, inherits = FALSE)
reference_date <- as.Date("2021-12-31")

retrieve_once <- function(name, call) {
  if (is.null(retrieved_2021[[name]])) {
    retrieved_2021[[name]] <<- force(call)
    assign(cache_name, retrieved_2021, envir = .GlobalEnv)
  }
  retrieved_2021[[name]]
}

observations_2021 <- list(
  mean_age = retrieve_once("mean_age",
    regionalepi::fetch_regional_mean_age(reference_date)),
  youth_dependency = retrieve_once("youth_dependency",
    regionalepi::fetch_regional_youth_dependency(reference_date)),
  population = retrieve_once("population",
    regionalepi::fetch_regional_population(reference_date)),
  area = retrieve_once("area",
    regionalepi::fetch_regional_area(reference_date))
)

regionalepi_demographic_snapshot_v5 <-
  regionalepi:::.extend_demographic_snapshot_v5(
    prior_snapshot, observations_2021)

dir.create("data", showWarnings = FALSE)
save(
  regionalepi_demographic_snapshot_v5,
  file = "data/regionalepi_demographic_snapshot_v5.rda",
  compress = "xz",
  version = 3L
)

component_statuses <- vapply(names(observations_2021), function(name) {
  regionalepi:::.snapshot_component_status(observations_2021[[name]], name)
}, character(1L))
yearly_counts <- lapply(
  regionalepi_demographic_snapshot_v5$data[c(
    "mean_age", "youth_dependency", "population", "area")],
  function(data) table(format(data$reference_date, "%Y")))

cat("Component source statuses:\n")
print(component_statuses)
cat("Yearly district counts:\n")
print(yearly_counts)
cat("2021 geography diagnostics:\n")
print(regionalepi_demographic_snapshot_v5$diagnostics$authenticated_2021_gate)
cat("Snapshot ID:",
    regionalepi_demographic_snapshot_v5$provenance$snapshot_id, "\n")
cat("Checksum:",
    regionalepi_demographic_snapshot_v5$provenance$content_checksum, "\n")
cat("Component row counts:\n")
print(regionalepi_demographic_snapshot_v5$diagnostics$component_row_counts)
cat("Object bytes:",
    as.numeric(object.size(regionalepi_demographic_snapshot_v5)), "\n")
