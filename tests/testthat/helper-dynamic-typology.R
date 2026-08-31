dynamic_summary_example <- function(n = 30L, period = 2017:2020) {
  ids <- sprintf("%05d", seq_len(n))
  index <- seq_len(n)
  profiles <- list(
    population_density = 50 + index * 4 + (index %% 3) * 30,
    mean_age = 35 + (index %% 10) + floor(index / 10),
    youth_dependency_ratio = 18 + (index %% 7) * 2 + floor(index / 15)
  )
  units <- c(
    population_density = "persons_per_km2",
    mean_age = "years",
    youth_dependency_ratio = "persons_under_20_per_100_persons_20_64"
  )
  rows <- do.call(rbind, lapply(names(profiles), function(indicator) {
    data.frame(
      geo_id = ids, geo_name = paste("District", ids), geo_level = "district",
      indicator_id = indicator, indicator_value = profiles[[indicator]],
      indicator_unit = units[[indicator]], definition_version = "dissertation_v1",
      period_start = as.Date(sprintf("%d-12-31", min(period))),
      period_end = as.Date(sprintf("%d-12-31", max(period))),
      aggregation_method = "arithmetic_mean",
      annual_observation_count = as.integer(length(period)),
      provenance_id = paste0("summary-", indicator), stringsAsFactors = FALSE
    )
  }))
  list(data = rows, diagnostics = list(), provenance = list())
}

map_geometry_example <- function(id = "01001") {
  list(
    type = "MultiPolygon",
    coordinates = list(list(list(
      c(9, 54), c(10, 54), c(10, 55), c(9, 54)
    )))
  )
}

map_resource_example <- function(ids = c("01001", "02000")) {
  features <- data.frame(
    geo_id = ids, geo_name = paste("District", ids), geo_level = "district",
    geo_vintage = as.Date("2024-12-31"),
    source_feature_id = paste0("source-", ids), stringsAsFactors = FALSE
  )
  features$geometry <- I(lapply(ids, map_geometry_example))
  provenance <- list(
    resource_id = "synthetic-map", resource_version = "v1",
    source_organization = "synthetic", source_product = "synthetic",
    source_vintage = as.Date("2024-12-31"), source_file = "synthetic.gpkg",
    source_layer = "district", source_crs = "EPSG:25832",
    output_crs = "EPSG:4326", simplification = "none",
    transformation = "synthetic transform",
    source_feature_count = as.integer(length(ids)),
    output_feature_count = as.integer(length(ids)),
    source_vertex_count = as.integer(4L * length(ids)),
    output_vertex_count = as.integer(4L * length(ids)),
    acquisition_provenance = "synthetic", source_sha256 = "synthetic",
    builder = "synthetic", license = "synthetic", license_url = "https://example.test",
    attribution = "synthetic", source_reference = "https://example.test",
    change_notice = "synthetic", package_version = "0.0.0.9000"
  )
  list(
    features = features, provenance = provenance,
    discrepancies = data.frame(
      geo_id = ids[[1L]], source_geo_name = "Source",
      canonical_geo_name = features$geo_name[[1L]], reason = "reviewed",
      stringsAsFactors = FALSE
    )
  )
}

