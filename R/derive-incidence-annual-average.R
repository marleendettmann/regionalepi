.annual_average_incidence_definition <- "annual_average_population_v1"
.annual_average_incidence_id <- "incidence_annual_average_population"
.annual_average_incidence_multiplier <- 100000

#' Validate incidence derived from annual-average population
#'
#' Validates a distinct derived measure that retains source-provided cases and
#' records official annual-average population alongside
#' `incidence_annual_average`. It is not the source-provided `incidence`
#' contract and cannot be consumed silently by historical incidence analyses.
#'
#' @param x A data frame or tibble.
#' @return `x`, invisibly.
#' @export
validate_annual_average_incidence <- function(x) {
  contract <- "annual-average population incidence"
  validate_surveillance(x)
  required <- c(
    "reporting_year", "annual_average_population", "population_year",
    "population_provenance_id", "incidence_annual_average",
    "incidence_definition_id", "incidence_definition_version",
    "incidence_status"
  )
  .require_columns(x, required, contract)
  .check_whole_number(
    x$reporting_year, "reporting_year", contract, non_negative = TRUE
  )
  .check_whole_number(
    x$population_year, "population_year", contract, non_negative = TRUE
  )
  .check_numeric(
    x$annual_average_population, "annual_average_population", contract
  )
  .check_numeric(
    x$incidence_annual_average, "incidence_annual_average", contract,
    non_negative = TRUE, allow_na = TRUE
  )
  for (field in c(
    "population_provenance_id", "incidence_definition_id",
    "incidence_definition_version", "incidence_status"
  )) {
    .check_character(x[[field]], field, contract)
  }
  if (any(x$annual_average_population <= 0)) {
    .stop_contract(contract, "`annual_average_population` must be strictly positive")
  }
  if (any(x$source != "SurvStat@RKI")) {
    .stop_contract(
      contract,
      "numerator `source` must be source-provided SurvStat@RKI cases"
    )
  }
  if (any(x$incidence_definition_id != .annual_average_incidence_id)) {
    .stop_contract(
      contract,
      "`incidence_definition_id` must be incidence_annual_average_population"
    )
  }
  if (any(x$incidence_definition_version !=
      .annual_average_incidence_definition)) {
    .stop_contract(
      contract,
      "`incidence_definition_version` must be annual_average_population_v1"
    )
  }
  if (any(!x$incidence_status %in% c("final", "provisional"))) {
    .stop_contract(contract, "`incidence_status` must be final or provisional")
  }
  final <- x$incidence_status == "final"
  if (any(x$population_year[final] != x$reporting_year[final])) {
    .stop_contract(
      contract,
      "final incidence must use the reporting-year annual-average population"
    )
  }
  provisional <- !final
  if (any(x$population_year[provisional] >= x$reporting_year[provisional])) {
    .stop_contract(
      contract,
      "provisional incidence must record an earlier denominator year"
    )
  }
  if ("incidence" %in% names(x)) {
    .stop_contract(
      contract,
      "source-provided `incidence` must remain in its separate contract"
    )
  }
  expected <- x$cases / x$annual_average_population *
    .annual_average_incidence_multiplier
  equal <- (is.na(expected) & is.na(x$incidence_annual_average)) |
    (!is.na(expected) & !is.na(x$incidence_annual_average) &
       expected == x$incidence_annual_average)
  if (any(!equal)) {
    .stop_contract(
      contract,
      "`incidence_annual_average` does not equal cases / population * 100000"
    )
  }
  invisible(x)
}

#' Derive incidence from official annual-average population
#'
#' Calculates weekly district incidence as source-provided SurvStat cases
#' divided by official annual-average population for the same canonical
#' `geo_id` and reporting year, multiplied by 100,000. The join is identifier-
#' and year-based only; names are never matching keys. No rounding, weighting,
#' age standardization, sex standardization, or geographic transformation is
#' performed.
#'
#' This function creates the distinct field `incidence_annual_average`. It does
#' not create or modify the source-provided `incidence` field used by historical
#' dissertation reproduction.
#'
#' @param cases A result with `data`, `diagnostics`, and `provenance` containing
#'   validated canonical source-provided SurvStat cases.
#' @param annual_average_population A result returned by
#'   [fetch_regional_average_population()].
#' @param definition_version The fixed reviewed definition version
#'   `"annual_average_population_v1"`.
#' @return A list with derived `data`, `diagnostics`, and `provenance`.
#' @export
derive_incidence_annual_average <- function(
    cases, annual_average_population,
    definition_version = "annual_average_population_v1") {
  contract <- "annual-average population incidence derivation"
  .require_result_parts(cases, contract)
  .require_result_parts(annual_average_population, contract)
  validate_surveillance(cases$data)
  validate_annual_average_population(annual_average_population$data)
  if (any(cases$data$source != "SurvStat@RKI")) {
    .stop_contract(
      contract,
      "numerator `source` must be source-provided SurvStat@RKI cases"
    )
  }
  if (!is.character(definition_version) || length(definition_version) != 1L ||
      is.na(definition_version) ||
      !identical(definition_version, .annual_average_incidence_definition)) {
    .stop_contract(
      contract,
      "`definition_version` must be annual_average_population_v1"
    )
  }
  if ("incidence" %in% names(cases$data)) {
    .stop_contract(contract, "numerator input must contain cases, not incidence")
  }
  collisions <- intersect(c(
    "annual_average_population", "population_year", "population_provenance_id",
    "incidence_annual_average", "incidence_definition_id",
    "incidence_definition_version", "incidence_status"
  ), names(cases$data))
  if (length(collisions)) {
    .stop_contract(
      contract,
      sprintf("numerator input already contains derived field(s): %s",
              paste(collisions, collapse = ", "))
    )
  }
  .require_columns(cases$data, "reporting_year", contract)
  .check_whole_number(
    cases$data$reporting_year, "reporting_year", contract,
    non_negative = TRUE
  )

  numerator_keys <- unique(paste(
    cases$data$geo_id, cases$data$reporting_year, sep = "\r"
  ))
  population <- annual_average_population$data
  denominator_keys <- paste(population$geo_id, population$year, sep = "\r")
  if (anyDuplicated(denominator_keys)) {
    .stop_contract(
      contract,
      "annual-average population geo_id/year keys must be unique"
    )
  }
  observation_keys <- paste(
    cases$data$geo_id, cases$data$reporting_year, sep = "\r"
  )
  position <- match(observation_keys, denominator_keys)
  if (anyNA(position)) {
    .stop_contract(
      contract,
      "every case geo_id/year key must have exactly one matching denominator"
    )
  }
  matched_population <- population[unique(position), , drop = FALSE]
  denominator <- population$population[position]
  if (anyNA(denominator) || any(!is.finite(denominator)) ||
      any(denominator <= 0)) {
    .stop_contract(contract, "every annual-average population must be finite and positive")
  }

  output <- cases$data
  output$annual_average_population <- denominator
  output$population_year <- population$year[position]
  output$population_provenance_id <- population$provenance_id[position]
  output$incidence_annual_average <-
    output$cases / denominator * .annual_average_incidence_multiplier
  output$incidence_definition_id <- .annual_average_incidence_id
  output$incidence_definition_version <- definition_version
  output$incidence_status <- "final"
  validate_annual_average_incidence(output)

  provenance_id <- paste0(
    .annual_average_incidence_id, "_", definition_version
  )
  list(
    data = output,
    diagnostics = list(
      operation = "derive_incidence_annual_average",
      observation_count = nrow(output),
      district_year_count = length(numerator_keys),
      unmatched_case_keys = 0L,
      unused_population_keys = length(setdiff(denominator_keys, numerator_keys)),
      zero_case_count = sum(output$cases == 0, na.rm = TRUE),
      missing_case_count = sum(is.na(output$cases)),
      missing_incidence_count = sum(is.na(output$incidence_annual_average)),
      multiplier = .annual_average_incidence_multiplier,
      rounding_performed = FALSE,
      weighting = "none",
      age_or_sex_standardization = FALSE,
      status = "final"
    ),
    provenance = stats::setNames(list(list(
      operation = "cases_divided_by_reporting_year_annual_average_population",
      incidence_definition_id = .annual_average_incidence_id,
      incidence_definition_version = definition_version,
      formula = "cases / annual_average_population * 100000",
      multiplier = .annual_average_incidence_multiplier,
      status = "final",
      numerator = list(
        measure = "source_provided_survstat_cases",
        source = sort(unique(output$source)),
        query_ids = if ("query_id" %in% names(output))
          sort(unique(output$query_id)) else character(),
        provenance = cases$provenance
      ),
      denominator = list(
        measure = "annual_average_population",
        source = sort(unique(matched_population$source)),
        source_table = sort(unique(matched_population$source_table)),
        years = sort(unique(matched_population$year)),
        population_basis = sort(unique(matched_population$population_basis)),
        provenance_ids = sort(unique(matched_population$provenance_id)),
        provenance = annual_average_population$provenance
      ),
      join_keys = c("geo_id", "reporting_year"),
      rounding = "none",
      weighting = "none",
      age_or_sex_standardization = FALSE
    )), provenance_id)
  )
}
