.map_feature_fields <- c(
  "geo_id", "geo_name", "geo_level", "geo_vintage",
  "source_feature_id", "geometry"
)

.map_provenance_fields <- c(
  "resource_id", "resource_version", "source_organization",
  "source_product", "source_vintage", "source_file", "source_layer",
  "source_crs", "output_crs", "simplification", "transformation",
  "source_feature_count", "output_feature_count", "source_vertex_count",
  "output_vertex_count", "browser_geojson_bytes", "browser_geojson_md5",
  "acquisition_provenance", "source_sha256",
  "builder", "license", "license_url", "attribution", "source_reference",
  "change_notice", "package_version"
)

.map_browser_geojson_md5 <- function(value) {
  path <- tempfile("regionalepi-map-", fileext = ".geojson")
  on.exit(unlink(path), add = TRUE)
  writeBin(charToRaw(value), path)
  unname(tools::md5sum(path))
}

#' Validate a map-geometry resource
#'
#' Validates visualization-only district geometry stored as ordinary
#' GeoJSON-compatible R lists. The contract is separate from canonical
#' non-spatial geography and does not require `sf`. It deliberately imposes no
#' universal feature count or particular vintage.
#'
#' @param x A list containing `features`, dataset-level `provenance`, and a
#'   reviewed `discrepancies` table.
#' @return `x`, invisibly.
#' @export
validate_map_geometry_resource <- function(x) {
  contract <- "map_geometry_resource"
  .require_named_list(x, c(
    "features", "browser_geojson", "provenance", "discrepancies"
  ), contract)
  features <- x$features
  .require_data_frame(features, paste(contract, "features"))
  .require_columns(features, .map_feature_fields, paste(contract, "features"))
  if (!identical(names(features), .map_feature_fields)) {
    .stop_contract(contract, "features contain missing, unexpected, or reordered fields")
  }
  for (field in c("geo_id", "geo_name", "geo_level", "source_feature_id")) {
    .check_character(features[[field]], field, contract)
  }
  .check_date(features$geo_vintage, "geo_vintage", contract)
  if (anyNA(features$geo_id) || any(!grepl("^[0-9]{5}$", features$geo_id))) {
    .stop_contract(contract, "`geo_id` must be complete five-character AGS text")
  }
  if (anyDuplicated(features$geo_id)) {
    .stop_contract(contract, "each geo_id must have exactly one map feature")
  }
  if (anyNA(features$geo_name) || any(!nzchar(features$geo_name)) ||
      anyNA(features$geo_level) || any(!nzchar(features$geo_level)) ||
      anyNA(features$geo_vintage) || anyNA(features$source_feature_id) ||
      any(!nzchar(features$source_feature_id))) {
    .stop_contract(contract, "feature identity and vintage fields must be complete")
  }
  if (!is.list(features$geometry) || length(features$geometry) != nrow(features)) {
    .stop_contract(contract, "`geometry` must be one GeoJSON-compatible list per feature")
  }
  invisible(lapply(features$geometry, .validate_map_multipolygon, contract = contract))
  .check_scalar_nonempty_character(x$browser_geojson,
                                   "browser_geojson", contract)
  if (!startsWith(x$browser_geojson, "{\"type\":\"FeatureCollection\"") ||
      nchar(x$browser_geojson, type = "bytes") !=
        x$provenance$browser_geojson_bytes) {
    .stop_contract(contract, "browser GeoJSON is malformed or has wrong size")
  }
  .validate_map_discrepancies(x$discrepancies, features$geo_id, contract)
  .validate_map_provenance(x$provenance, nrow(features), contract)
  if (!identical(.map_browser_geojson_md5(x$browser_geojson),
                 x$provenance$browser_geojson_md5)) {
    .stop_contract(contract, "browser GeoJSON checksum does not match")
  }
  invisible(x)
}

.validate_map_multipolygon <- function(geometry, contract) {
  if (!is.list(geometry) || !identical(names(geometry), c("type", "coordinates")) ||
      !identical(geometry$type, "MultiPolygon") ||
      !is.list(geometry$coordinates) || !length(geometry$coordinates)) {
    .stop_contract(contract, "every geometry must be a non-empty MultiPolygon")
  }
  for (polygon in geometry$coordinates) {
    if (!is.list(polygon) || !length(polygon)) {
      .stop_contract(contract, "every MultiPolygon part must contain rings")
    }
    for (ring in polygon) {
      if (!is.list(ring) || length(ring) < 4L) {
        .stop_contract(contract, "every geometry ring must contain at least four positions")
      }
      valid <- vapply(ring, function(position) {
        is.numeric(position) && length(position) == 2L &&
          !anyNA(position) && all(is.finite(position))
      }, logical(1L))
      if (!all(valid) || !identical(as.numeric(ring[[1L]]),
                                    as.numeric(ring[[length(ring)]]))) {
        .stop_contract(contract, "geometry rings must contain finite closed XY positions")
      }
      if (any(vapply(ring, function(position) {
        abs(position[[1L]]) > 180 || abs(position[[2L]]) > 90
      }, logical(1L)))) {
        .stop_contract(contract, "geometry positions must be valid EPSG:4326 longitude/latitude")
      }
    }
  }
  invisible(geometry)
}

.validate_map_discrepancies <- function(x, ids, contract) {
  required <- c("geo_id", "source_geo_name", "canonical_geo_name", "reason")
  .require_data_frame(x, paste(contract, "discrepancies"))
  .require_columns(x, required, paste(contract, "discrepancies"))
  if (!identical(names(x), required)) {
    .stop_contract(contract, "discrepancies contain unexpected or reordered fields")
  }
  for (field in required) .check_character(x[[field]], field, contract)
  if (anyNA(x) || any(!nzchar(as.matrix(x))) || anyDuplicated(x$geo_id) ||
      any(!x$geo_id %in% ids) || any(x$source_geo_name == x$canonical_geo_name)) {
    .stop_contract(contract, "reviewed source/canonical discrepancies are malformed")
  }
  invisible(x)
}

.validate_map_provenance <- function(x, feature_count, contract) {
  .require_named_list(x, .map_provenance_fields, paste(contract, "provenance"))
  if (!identical(names(x), .map_provenance_fields)) {
    .stop_contract(contract, "provenance contains unexpected or reordered fields")
  }
  numeric_fields <- c(
    "source_feature_count", "output_feature_count", "source_vertex_count",
    "output_vertex_count", "browser_geojson_bytes"
  )
  character_fields <- setdiff(.map_provenance_fields, c("source_vintage", numeric_fields))
  for (field in character_fields) .check_scalar_nonempty_character(x[[field]], field, contract)
  .check_date(x$source_vintage, "source_vintage", contract)
  if (length(x$source_vintage) != 1L || is.na(x$source_vintage)) {
    .stop_contract(contract, "source_vintage must be one complete Date")
  }
  for (field in numeric_fields) {
    .check_whole_number(x[[field]], field, contract, non_negative = TRUE)
    if (length(x[[field]]) != 1L) {
      .stop_contract(contract, paste0("`", field, "` must have length one"))
    }
  }
  if (x$output_feature_count != feature_count || x$source_feature_count != feature_count ||
      x$source_vertex_count <= 0 || x$output_vertex_count <= 0) {
    .stop_contract(contract, "provenance feature or vertex counts contradict the resource")
  }
  if (!identical(x$output_crs, "EPSG:4326")) {
    .stop_contract(contract, "map output CRS must be EPSG:4326")
  }
  if (!grepl("^[0-9a-f]{32}$", x$browser_geojson_md5)) {
    .stop_contract(contract, "browser GeoJSON checksum is malformed")
  }
  invisible(x)
}
