#' Reviewed Regionaldatenbank demographic source snapshots
#'
#' Immutable, normalized source observations. Version 4 contains the typology
#' inputs for 2017--2020 and 2022--2025 plus official annual-average population
#' for reporting years 2017--2025. Versions 1--3 are retained as immutable
#' prior snapshots. The 2017--2020 annual-average population is additively
#' harmonized from 401 source districts to the reviewed current 400-district
#' analysis geography before incidence is calculated. Population density,
#' period summaries, typologies, and incidence
#' are not stored and are reproduced through the ordinary package functions.
#' Source status, component provenance, retrieval timestamps, versioning policy,
#' and a deterministic content checksum are stored at dataset level.
#'
#' @format A validated list with `data`, `provenance`, and `diagnostics`.
#' @source Regionaldatenbank Deutschland, tables `12411-07-01-4`,
#'   `12411-08-01-4`, `12411-01-01-4`, `11111-01-01-4`, and
#'   `12411-05-01-4`.
"regionalepi_demographic_snapshot_v4"

#' @rdname regionalepi_demographic_snapshot_v4
"regionalepi_demographic_snapshot_v3"

#' @rdname regionalepi_demographic_snapshot_v4
"regionalepi_demographic_snapshot_v2"

#' @rdname regionalepi_demographic_snapshot_v4
"regionalepi_demographic_snapshot_v1"
