#' Validate a display-only Bundesland boundary resource
#' @param x An ordinary resource list.
#' @return `x`, invisibly.
#' @export
validate_state_boundaries_resource <- function(x) {
  contract <- "state boundaries resource"
  .require_named_list(x, c("features", "browser_geojson", "provenance"), contract)
  .require_data_frame(x$features, contract)
  .require_columns(x$features, c("geo_id", "geo_name", "source_feature_id", "geometry"), contract)
  if (nrow(x$features) != 16L || anyDuplicated(x$features$geo_id) ||
      anyNA(x$features[c("geo_id", "geo_name", "source_feature_id")]) ||
      any(!grepl("^[0-9]{2}$", x$features$geo_id)))
    .stop_contract(contract, "requires exactly 16 complete unique L\u00e4nder")
  invisible(lapply(x$features$geometry, .validate_map_multipolygon, contract = contract))
  .check_scalar_nonempty_character(x$browser_geojson, "browser_geojson", contract)
  .require_named_list(x$provenance, c("resource_id", "resource_version", "source_organization",
    "source_product", "source_vintage", "source_file", "source_layer", "source_crs",
    "output_crs", "selection", "source_sha256", "feature_count", "license", "license_url",
    "attribution", "builder"), contract)
  if (!identical(x$provenance$source_layer, "vg2500_lan") ||
      !identical(x$provenance$source_crs, "EPSG:25832") ||
      !identical(x$provenance$output_crs, "EPSG:4326") || x$provenance$feature_count != 16L)
    .stop_contract(contract, "provenance contradicts the reviewed geometry")
  invisible(x)
}

#' Access reviewed display-only Bundesland boundaries
#' @param vintage The exact reviewed resource vintage.
#' @return A validated ordinary geometry resource.
#' @export
regionalepi_state_boundaries <- function(vintage = as.Date("2024-12-31")) {
  if (!inherits(vintage, "Date") || length(vintage) != 1L || is.na(vintage) ||
      vintage != as.Date("2024-12-31")) stop("Unsupported state-boundary vintage.", call. = FALSE)
  resource <- get("regionalepi_state_boundaries_2024", envir = environment(), inherits = TRUE)
  validate_state_boundaries_resource(resource)
  resource
}
