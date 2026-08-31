#' Reviewed map-ready VG2500 district geometry for 2024
#'
#' Visualization-only German district MultiPolygons derived from BKG VG2500,
#' vintage 2024-12-31. Geometry was transformed once from EPSG:25832 to
#' EPSG:4326 without simplification. Canonical names come from the reviewed
#' regionalepi geography register after exact AGS-set validation.
#'
#' The object is an ordinary list with `features`, dataset-level `provenance`,
#' and the three reviewed VG2500/canonical name `discrepancies`. Geometry is
#' stored as GeoJSON-compatible R lists and does not require `sf` at runtime.
#' It must not be used to calculate population density or other areas.
#'
#' The source is licensed under Datenlizenz Deutschland - Namensnennung -
#' Version 2.0. See `inst/NOTICE` and the resource provenance for attribution.
#'
#' @format A validated map-geometry resource containing 400 district features.
#' @source Bundesamt für Kartographie und Geodäsie (BKG), VG2500.
"regionalepi_map_geometry_2024"

