#' Reviewed geography resources for the 2024 reference register
#'
#' A versioned bundle of non-geometric runtime resources used to resolve the
#' reviewed SurvStat 2024 source geography labels against BKG VG-Hist districts
#' applicable on 2024-12-31.
#'
#' @format A named list with four elements:
#' \describe{
#'   \item{bkg_districts}{A 400-row canonical geography register with
#'   `geo_id`, `geo_name`, `geo_level`, `valid_from`, `valid_to`, `source`,
#'   `vghid`, `sns`, `stg`, `vgcode`, and `canonical_type`. `geo_id` is the
#'   applicable character SKZ; `vghid` is retained historical provenance.}
#'   \item{survstat_aliases}{Nineteen exact reviewed aliases satisfying
#'   `validate_geography_aliases()`.}
#'   \item{survstat_spatial_units}{Twelve reviewed Berlin Bezirk source
#'   identities with opaque namespaced identifiers. They do not identify or
#'   imply an aggregation target.}
#'   \item{provenance}{Dataset-level BKG product, reference-date, checksum,
#'   license, transformation, builder, and reviewed SurvStat metadata.}
#' }
#'
#' Alias and spatial-unit validity from 2024-12-31 through 2024-12-31 means
#' "reviewed and established for this reference date"; it does not assert that
#' a mapping existed for only one calendar day or establish historical
#' applicability. VG-Hist's `9999-12-31` `END` values are preserved because the
#' supplied official documentation defines `END` as an inclusive end date but
#' does not explicitly identify that value as an open-ended sentinel.
#'
#' The resource is derived and modified from BKG VG-Hist 2026-01 under CC BY
#' 4.0. Geometry is omitted. See the installed `NOTICE` file for attribution.
#' These source-data terms do not determine the regionalepi code license.
#'
#' @source Bundesamt für Kartographie und Geodäsie (BKG), Verwaltungsgebiete
#'   Historisch (VG-Hist), product version 2026-01.
"regionalepi_geography_resources_2024"
