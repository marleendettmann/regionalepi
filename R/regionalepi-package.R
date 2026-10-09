#' regionalepi: Regional Infectious Disease Surveillance and Demographic Typologies
#'
#' `regionalepi` is an R package for reproducible analysis of regional
#' infectious disease surveillance in Germany using demographic indicators and
#' typologies. It combines demographic typology construction,
#' surveillance-data processing, incidence derivation, regional comparisons,
#' and interactive exploration.
#'
#' @section Scientific scope:
#'
#' The typology uses population density, mean age, and the youth dependency
#' ratio.
#'
#' The package was developed from a demographic-typology approach for studying
#' regional patterns of infectious disease surveillance in Germany. Demographic
#' characteristics are used to reproduce a historical reference typology or to
#' construct updated demographic typologies with k-means. These typologies can
#' then be linked to surveillance observations to examine differences in
#' infectious disease activity between demographic types and regions.
#' A fitted typology contains one reusable district assignment per character
#' `geo_id`, so assignments can also be joined to other compatible
#' district-level data outside the package workflow.
#'
#' The current implementation focuses on German districts. Geographic identity
#' is resolved explicitly before analysis, and provenance is retained for source
#' data, derived measures, typology fits, and analytical outputs.
#'
#' @section Analysis paths:
#' The historical reference path reproduces the reviewed 2017--2020 demographic
#' typology and uses source-provided SurvStat incidence. This path is retained
#' for exact reproduction of the historical reference analysis.
#'
#' The updated path constructs demographic typologies with fit-local cluster
#' identifiers and derives incidence from SurvStat case counts and official
#' reporting-year annual-average population.
#' For updated typologies, `k = 3` is retained as an interpretative reference
#' for comparison with the historical three-cluster structure; it is not
#' asserted to be statistically optimal.
#'
#' @section Typical workflow:
#' A typical analysis consists of four steps:
#'
#' 1. obtain or validate demographic indicators and surveillance observations;
#' 2. reproduce the historical reference typology or fit an updated demographic
#'    typology;
#' 3. attach the typology to district-level surveillance observations and derive
#'    the intended incidence measure;
#' 4. summarize and compare infectious disease activity across demographic
#'    types, time periods, and regions.
#'
#' Demographic data can be prepared from the bundled snapshot with
#' [regionalepi_demographic_snapshot()] and [prepare_demographic_snapshot()],
#' then fitted with [fit_dynamic_typology()] or
#' [fit_dissertation_typology()]. Counts can be read with [read_survstat()] or retrieved with
#' [fetch_survstat_cases()]. Source-provided rates can be read with
#' [read_survstat_incidence()] or retrieved with [fetch_survstat_incidence()].
#' The current derived-incidence path uses [fetch_regional_average_population()]
#' together with [derive_incidence_annual_average()] or
#' [prepare_analysis_incidence()]. Typologies are joined with
#' [attach_typology()], periods with [assign_epidemiological_periods()], and
#' results can be summarized with [summarize_incidence_by_typology()] or
#' [summarize_period_incidence_by_district()].
#'
#' @section Incidence definition:
#'
#' For completed reporting years, district incidence is calculated as
#'
#' `cases / annual-average population * 100,000`.
#'
#' The denominator is the official annual-average population of the reporting
#' year. For a current reporting year for which the final annual-average
#' population is not yet available, derived incidence may be displayed only as
#' explicitly provisional, using the immediately preceding year's available
#' official annual-average population as the denominator. Older denominators
#' are rejected. The population reference year and provisional status are
#' retained in the result provenance.
#' Annual-average population for 2017--2021 uses the Census 2011 progression
#' basis; values from 2022 use the Census 2022 progression basis. This is a
#' change in the population-estimation basis, not in the incidence formula.
#' Comparisons across the 2021/2022 transition should acknowledge this
#' methodological discontinuity.
#' Dynamic demographic reference periods may contain one or more contiguous
#' years wholly within 2017--2020 or wholly within 2021--2025. Periods crossing
#' 2020/2021 are rejected because the historical 401-district geography and the
#' current 400-district geography are not silently harmonized for nonadditive
#' indicators. User-defined configurations use the reviewed indicators and
#' fitting method, but the resulting cluster solution is not reviewed
#' individually. A one-year reference period uses that reporting year's
#' demographic indicators.
#'
#' Source-provided SurvStat incidence is retained separately for provenance,
#' methodological comparison, and historical reproduction. The derived measure
#' is neither age-standardized nor sex-standardized.
#'
#' @section Interactive application:
#'
#' All principal analytical objects can be used directly from R. The package
#' also includes an optional Shiny application for interactive exploration
#' of demographic typologies and infectious disease surveillance. Launch it with
#' [run_regionalepi_app()]. The application uses dynamically fitted demographic
#' typologies for regular surveillance analysis and displays the active
#' incidence definition and relevant provenance information. Historical
#' dissertation reproduction remains available through the package API rather
#' than as a separate application analysis area.
#' Scientific downloads use these prepared or loaded in-session results without
#' retrieving or refitting data. Excel workbooks combine result tables with
#' methodology, source, licensing, and provenance information; individual
#' tables are available as UTF-8 CSV and selected static scientific figures as
#' PNG. PDF reports, SVG, Leaflet-map exports, and direct Plotly exports are
#' deferred.
#'
#' Cite the package as: Dettmann, M. (2026). *regionalepi: Regionale
#' Infektionssurveillance und demografische Typologien* (Version 0.1.0).
#' R-Paket.
#'
#' The scientific foundation is: Dettmann, M. (2026). *Einfluss demografischer
#' Faktoren auf die Ausbreitung von Infektionskrankheiten am Beispiel von
#' Influenza und COVID-19*. Freie Universität Berlin.
#' DOI: 10.17169/refubium-51449.
#'
#' @section Data sources and provenance:
#'
#' The bundled Shiny application is configured for seasonal influenza
#' (`"Influenza, saisonal"`), COVID-19 (`"COVID-19"`), and norovirus
#' gastroenteritis (`"Norovirus-Gastroenteritis"`). These are exact,
#' case-sensitive SurvStat disease-member captions. Influenza and COVID-19 have
#' additional pathogen-specific methodological review and period definitions.
#' Norovirus is included in the application and covered by regression tests but
#' has not undergone the same depth of pathogen-specific methodological review.
#' These configurations are not a general restriction of the package API.
#' Other exact SurvStat pathogen members can be processed programmatically when
#' they satisfy the surveillance and geography contracts, although they have
#' not necessarily undergone pathogen-specific validation in `regionalepi`.
#'
#' Surveillance observations are provided by the Robert Koch-Institut (RKI)
#' through SurvStat@RKI 2.0. The RKI permits use of data from SurvStat@RKI 2.0
#' subject to source attribution and currently proposes the citation:
#' `"Robert Koch-Institut: SurvStat@RKI 2.0, https://survstat.rki.de,
#' Abfragedatum: <Datum der Abfrage>"`. Live query provenance retains the query
#' time and the RKI source data status where available; these are distinct
#' provenance fields.
#' SurvStat source geography labels are retained as provided.
#' [resolve_geography()] assigns the five-character district identifier `geo_id`
#' using the package's reviewed geography resources; source-specific spatial
#' aggregation remains a separate step. Resolved district data use `geo_id` for
#' typology attachment.
#'
#' Official demographic data are obtained from the Regionaldatenbank Deutschland,
#' provided by the Statistische Ämter des Bundes und der Länder. Regionaldatenbank
#' source data used by `regionalepi` are subject to the Datenlizenz Deutschland –
#' Namensnennung – Version 2.0 (`dl-de/by-2-0`). The bundled demographic resources
#' are derived from selected Regionaldatenbank source data and retain their source
#' provenance.
#' Public functions retrieve population stock with [fetch_regional_population()],
#' annual-average population with [fetch_regional_average_population()], area
#' with [fetch_regional_area()], mean age with [fetch_regional_mean_age()], and
#' youth dependency with [fetch_regional_youth_dependency()]. Live retrieval of
#' typology data and annual-average population is optional.
#'
#' The GPL-3 license of `regionalepi` applies to package code; it does not
#' relicense SurvStat observations, Regionaldatenbank data, or other third-party
#' resources. See the installed `NOTICE` file and source-specific provenance.
#' Current RKI guidance on SurvStat data use is available at
#' \url{https://survstat.rki.de/Content/Instruction/DataUsage.aspx}.
#'
#' @examples
#' # Example 1: derive district incidence from reported cases and the official
#' # annual-average population. The example is synthetic and runs offline.
#' cases <- list(
#'   data = data.frame(
#'     geo_id = "01001", geo_name = "Flensburg", geo_level = "district",
#'     geo_vintage = as.Date(NA), date = as.Date("2022-01-03"),
#'     time_unit = "week", pathogen = "Example", cases = 5,
#'     source = "SurvStat@RKI", reporting_year = 2022L
#'   ),
#'   diagnostics = list(), provenance = list()
#' )
#'
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
#'
#' result <- derive_incidence_annual_average(cases, population)
#' result$data[c(
#'   "cases", "annual_average_population", "incidence_annual_average"
#' )]
#'
#' # Example 2: inspect the reviewed demographic snapshot included with the
#' # package. No network request is made.
#' snapshot <- regionalepi_demographic_snapshot()
#'
#' names(snapshot$data)
#' head(snapshot$data$mean_age)
#' head(snapshot$data$youth_dependency)
#'
#' # Source and reproducibility metadata remain available separately.
#' snapshot$provenance$source
#' snapshot$provenance$covered_reference_dates
#' snapshot$diagnostics$checksum_verified
#'
#' # Example 3: prepare the reviewed snapshot and fit an updated typology.
#' # The complete workflow uses only public functions and runs offline.
#' demography <- prepare_demographic_snapshot(
#'   snapshot = snapshot,
#'   reference_years = 2022:2025
#' )
#'
#' head(demography$annual)
#' head(demography$summary$data)
#' demography$snapshot_provenance$snapshot_id
#'
#' fit <- fit_dynamic_typology(
#'   demography$summary,
#'   k = 3L
#' )
#'
#' head(fit$assignments)
#' fit$profiles
#'
#' clusters <- fit$assignments[c("geo_id", "display_cluster_id")]
#' head(clusters)
#'
#' # Example 4: retrieve live SurvStat case counts for the three pathogens
#' # configured in the bundled Shiny application. These examples require
#' # network access and are not run
#' # automatically during package checks.
#' \dontrun{
#' influenza_cases <- fetch_survstat_cases(
#'   pathogen = "Influenza, saisonal",
#'   reporting_years = 2025L,
#'   geography = "kreis",
#'   query_id = "example-influenza-2025"
#' )
#'
#' covid_cases <- fetch_survstat_cases(
#'   pathogen = "COVID-19",
#'   reporting_years = 2025L,
#'   geography = "kreis",
#'   query_id = "example-covid19-2025"
#' )
#'
#' norovirus_cases <- fetch_survstat_cases(
#'   pathogen = "Norovirus-Gastroenteritis",
#'   reporting_years = 2025L,
#'   geography = "kreis",
#'   query_id = "example-norovirus-2025"
#' )
#'
#' head(influenza_cases$data)
#' influenza_cases$provenance
#' }
#'
#' # Example 5: retrieve source-provided SurvStat incidence. This remains
#' # separate from incidence derived from annual-average population.
#' \dontrun{
#' influenza_source_incidence <- fetch_survstat_incidence(
#'   pathogen = "Influenza, saisonal",
#'   reporting_years = 2025L,
#'   geography = "kreis",
#'   query_id = "example-influenza-incidence-2025"
#' )
#'
#' covid_source_incidence <- fetch_survstat_incidence(
#'   pathogen = "COVID-19",
#'   reporting_years = 2025L,
#'   geography = "kreis",
#'   query_id = "example-covid19-incidence-2025"
#' )
#'
#' norovirus_source_incidence <- fetch_survstat_incidence(
#'   pathogen = "Norovirus-Gastroenteritis",
#'   reporting_years = 2025L,
#'   geography = "kreis",
#'   query_id = "example-norovirus-incidence-2025"
#' )
#' }
#'
#' # Example 6: launch the interactive application.
#' # This is not run automatically during package checks.
#' \dontrun{
#' run_regionalepi_app()
#' }
#'
"_PACKAGE"
