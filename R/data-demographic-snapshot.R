#' Reviewed Regionaldatenbank demographic source snapshot
#'
#' Immutable, normalized source observations for 2017--2020 and 2022--2024.
#' The resource contains source-provided mean age and youth dependency ratio,
#' total population, and district area. Population density, period summaries,
#' and typologies are deliberately not stored and are reproduced through the
#' ordinary package functions. Source status, component provenance, retrieval
#' timestamps, versioning policy, and a deterministic content checksum are
#' stored at dataset level.
#'
#' @format A validated list with `data`, `provenance`, and `diagnostics`.
#' @source Regionaldatenbank Deutschland, tables `12411-07-01-4`,
#'   `12411-08-01-4`, `12411-01-01-4`, and `11111-01-01-4`.
"regionalepi_demographic_snapshot_v1"
