#' Access reviewed map-ready district geometry
#'
#' Returns visualization-only, GeoJSON-compatible district MultiPolygons and
#' the attribution needed by clients. No `sf` installation or local BKG source
#' path is required. Unsupported vintages fail rather than falling back to a
#' latest resource.
#'
#' @param vintage One exact reviewed map vintage. V1 supports only
#'   `as.Date("2024-12-31")`.
#' @return A validated ordinary list with `features`, `provenance`, and
#'   `discrepancies`.
#' @export
regionalepi_map_geometry <- function(vintage = as.Date("2024-12-31")) {
  contract <- "regionalepi map geometry"
  .check_date(vintage, "vintage", contract)
  if (length(vintage) != 1L || is.na(vintage) ||
      vintage != as.Date("2024-12-31")) {
    .stop_contract(contract, "unsupported map vintage; v1 provides only 2024-12-31")
  }
  resource <- .regionalepi_package_data("regionalepi_map_geometry_2024")
  validate_map_geometry_resource(resource)
  resource
}
