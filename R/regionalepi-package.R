#' regionalepi: Regional epidemiology with explicit scientific contracts
#'
#' `regionalepi` provides validated data contracts and reproducible operations
#' for combining regional infectious-disease surveillance with demographic and
#' geographic context. Geographic identity is resolved explicitly before
#' analysis, and provenance remains attached to source and derived results.
#'
#' The historical path reproduces the reviewed 2017--2020 dissertation
#' typology and uses source-provided SurvStat incidence. The separate dynamic
#' path uses fit-local cluster IDs and, for current Paper-2 analysis, derives
#' incidence from SurvStat cases and official reporting-year annual-average
#' population. Additive counts and non-additive source rates therefore remain
#' distinct contracts.
#'
#' Start with [read_survstat()] or [fetch_survstat_cases()] for counts,
#' [read_survstat_incidence()] or [fetch_survstat_incidence()] for source rates,
#' and [fetch_regional_average_population()] plus
#' [derive_incidence_annual_average()] for the current derived-incidence path.
#' Reviewed offline resources are available through
#' [regionalepi_demographic_snapshot()] and [regionalepi_map_geometry()]. The
#' optional application starts with [run_regionalepi_app()].
#'
#' @examples
#' cases <- list(
#'   data = data.frame(
#'     geo_id = "01001", geo_name = "Flensburg", geo_level = "district",
#'     geo_vintage = as.Date(NA), date = as.Date("2022-01-03"),
#'     time_unit = "week", pathogen = "Example", cases = 5,
#'     source = "SurvStat@RKI", reporting_year = 2022L
#'   ),
#'   diagnostics = list(), provenance = list()
#' )
#' population <- list(
#'   data = data.frame(
#'     geo_id = "01001", geo_name = "Flensburg", geo_level = "district",
#'     geo_vintage = as.Date(NA), year = 2022L, population = 95000,
#'     population_measure = "annual_average_population",
#'     population_reference = "reporting_year_annual_average",
#'     population_basis = "census_2022",
#'     source = "Regionaldatenbank Deutschland",
#'     source_table = "12411-05-01-4",
#'     retrieved_at = as.POSIXct("2022-12-31", tz = "UTC"),
#'     data_status = "example", provenance_id = "example-population"
#'   ),
#'   diagnostics = list(), provenance = list()
#' )
#' result <- derive_incidence_annual_average(cases, population)
#' result$data[c("cases", "annual_average_population",
#'               "incidence_annual_average")]
#'
#' @keywords internal
"_PACKAGE"
