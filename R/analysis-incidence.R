#' Prepare annual-average-population incidence for current analysis
#'
#' Creates an analysis-ready data set from canonical SurvStat cases and the
#' official annual-average population. The source-provided SurvStat incidence
#' is retained verbatim as `incidence_source`; the existing downstream
#' `incidence` field in the returned analysis data contains the distinct
#' `incidence_annual_average` measure. The input object is not modified.
#'
#' Empty cells already interpreted as structural zero counts by the approved
#' count adapter therefore produce zero derived incidence. Explicitly missing
#' cases remain missing. Final observations require population from the same
#' reporting year. A reporting year may use the preceding year's official
#' annual-average population only through an explicit provisional mapping.
#'
#' @param surveillance A combined canonical SurvStat incidence-and-count result.
#' @param annual_average_population A validated annual-average population result.
#' @param provisional_denominator_years Optional named whole-valued vector whose
#'   names are provisional reporting years and values are explicit earlier
#'   denominator years, for example `c("2026" = 2025)`.
#' @return A list with analysis-ready `data`, diagnostics, provenance, and the
#'   unchanged source-incidence bundle.
#' @export
prepare_analysis_incidence <- function(
    surveillance, annual_average_population,
    provisional_denominator_years = integer()) {
  contract <- "annual-average population analysis incidence"
  .require_result_parts(surveillance, contract)
  .require_result_parts(annual_average_population, contract)
  validate_surveillance_incidence(surveillance$data)
  .require_columns(surveillance$data, c("cases", "reporting_year"), contract)
  validate_annual_average_population(annual_average_population$data)
  mapping <- .validate_provisional_denominator_years(
    provisional_denominator_years, contract
  )

  years <- sort(unique(surveillance$data$reporting_year))
  available <- sort(unique(annual_average_population$data$year))
  missing_years <- union(
    setdiff(years, available), as.integer(names(mapping))
  )
  if (length(mapping) &&
      !setequal(names(mapping), as.character(missing_years))) {
    .stop_contract(
      contract,
      "provisional denominator mappings must identify exactly the unavailable reporting years"
    )
  }
  if (length(missing_years) &&
      !setequal(as.character(missing_years), names(mapping))) {
    .stop_contract(
      contract,
      paste0(
        "official annual-average population is unavailable for reporting year(s): ",
        paste(missing_years, collapse = ", "),
        "; final analysis cannot use a fallback"
      )
    )
  }
  if (length(mapping) && any(!as.integer(mapping) %in% available)) {
    .stop_contract(
      contract,
      "every provisional denominator year must be present in the population input"
    )
  }

  population <- annual_average_population
  if (length(mapping)) {
    population$data <- population$data[
      !population$data$year %in% as.integer(names(mapping)), , drop = FALSE
    ]
  }
  mapped_parts <- list(population$data)
  if (length(mapping)) {
    for (reporting_year in names(mapping)) {
      denominator_year <- as.integer(mapping[[reporting_year]])
      part <- population$data[population$data$year == denominator_year, , drop = FALSE]
      part$year <- as.integer(reporting_year)
      mapped_parts[[length(mapped_parts) + 1L]] <- part
    }
  }
  population$data <- do.call(rbind, mapped_parts)
  key <- paste(population$data$geo_id, population$data$year, sep = "\r")
  population$data <- population$data[!duplicated(key), , drop = FALSE]
  rownames(population$data) <- NULL

  cases <- surveillance
  cases$data <- surveillance$data
  cases$data$incidence <- NULL
  derived <- derive_incidence_annual_average(cases, population)
  output <- derived$data
  output$incidence_source <- surveillance$data$incidence
  output$incidence <- output$incidence_annual_average
  output$population_year <- output$reporting_year
  if (length(mapping)) {
    for (reporting_year in names(mapping)) {
      use <- output$reporting_year == as.integer(reporting_year)
      output$population_year[use] <- as.integer(mapping[[reporting_year]])
      output$incidence_status[use] <- "provisional"
    }
  }
  validate_annual_average_incidence(output[setdiff(
    names(output), "incidence"
  )])
  if (!identical(output$incidence_source, surveillance$data$incidence)) {
    .stop_contract(contract, "source-provided incidence was not preserved")
  }

  statuses <- unique(output[c(
    "reporting_year", "population_year", "incidence_status"
  )])
  statuses <- statuses[order(statuses$reporting_year), , drop = FALSE]
  rownames(statuses) <- NULL
  derived$provenance[[1L]]$status_by_reporting_year <- statuses
  overall_status <- if (all(statuses$incidence_status == "final")) {
    "final"
  } else {
    "provisional"
  }
  derived$provenance[[1L]]$status <- overall_status
  derived$provenance[[1L]]$denominator$years <-
    sort(unique(statuses$population_year))
  derived$provenance[[1L]]$structural_zero_policy <- paste(
    "Blank result cells in the reviewed SurvStat case-count exports are",
    "interpreted as structural zero counts; source-incidence blanks remain NA"
  )
  list(
    data = output,
    diagnostics = c(derived$diagnostics[setdiff(
      names(derived$diagnostics), "status"
    )], list(
      status = overall_status,
      primary_incidence = "incidence_annual_average",
      source_incidence_preserved = TRUE,
      status_by_reporting_year = statuses,
      structural_zero_policy = "reviewed_structural_zero",
      source_incidence_blank_policy = "NA"
    )),
    provenance = list(
      incidence = derived$provenance,
      source_incidence = surveillance$provenance$incidence,
      counts = surveillance$provenance$counts,
      population = annual_average_population$provenance
    ),
    source_incidence = surveillance
  )
}

.validate_provisional_denominator_years <- function(x, contract) {
  if (is.null(x)) x <- integer()
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)) ||
      any(x != floor(x)) || any(x < 0) ||
      (length(x) && (is.null(names(x)) || any(!nzchar(names(x))) ||
        anyDuplicated(names(x)) || any(!grepl("^[0-9]+$", names(x)))))) {
    .stop_contract(
      contract,
      "`provisional_denominator_years` must be a named whole-valued year mapping"
    )
  }
  reporting_years <- if (length(x)) as.integer(names(x)) else integer()
  if (length(x) && any(as.integer(x) != reporting_years - 1L)) {
    .stop_contract(
      contract,
      "a provisional denominator must be exactly one year before its reporting year"
    )
  }
  as.integer(x) |> stats::setNames(names(x))
}
