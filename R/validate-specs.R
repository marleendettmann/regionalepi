.indicator_fields <- c("indicator_id", "indicator_type", "definition_version",
                       "parameters")

#' Validate an indicator specification
#'
#' All indicator specifications contain an ID, type, definition version, and a
#' parameter list. Ratio indicators additionally require numerator and
#' denominator parameter lists, a multiplier, and a unit. Ratio age boundaries
#' are explicit and configurable; an upper boundary may be `NA` for an
#' open-ended interval.
#'
#' @param x A named list.
#' @return `x`, invisibly.
#' @export
validate_indicator_spec <- function(x) {
  contract <- "indicator_spec"
  .require_named_list(x, .indicator_fields, contract)
  for (field in c("indicator_id", "indicator_type", "definition_version")) {
    .check_scalar_character(x[[field]], field, contract)
  }
  if (!is.list(x$parameters) || is.data.frame(x$parameters)) {
    .stop_contract(contract, "`parameters` must be a list")
  }
  types <- c("source_provided", "ratio", "density")
  if (!x$indicator_type %in% types) {
    .stop_contract(
      contract,
      "`indicator_type` must be source_provided, ratio, or density"
    )
  }
  if (identical(x$indicator_type, "source_provided")) {
    .validate_source_provided_parameters(x$parameters, contract)
  } else if (identical(x$indicator_type, "ratio")) {
    .validate_ratio_parameters(x$parameters, contract)
  } else {
    .validate_density_parameters(x$parameters, contract)
  }
  invisible(x)
}

.validate_ratio_parameters <- function(parameters, contract) {
  .require_named_list(
    parameters,
    c("numerator", "denominator", "multiplier", "unit"),
    paste(contract, "ratio parameters")
  )
  for (component in c("numerator", "denominator")) {
    part <- parameters[[component]]
    component_contract <- paste(contract, component)
    .require_named_list(part, c("variable", "age_from", "age_to"),
                        component_contract)
    .check_scalar_character(part$variable, "variable", component_contract)
    .check_whole_number(part$age_from, "age_from", component_contract,
                        non_negative = TRUE)
    if (length(part$age_from) != 1L) {
      .stop_contract(component_contract, "`age_from` must have length one")
    }
    .check_whole_number(part$age_to, "age_to", component_contract,
                        allow_na = TRUE, non_negative = TRUE)
    if (length(part$age_to) != 1L ||
        (!is.na(part$age_to) && part$age_to < part$age_from)) {
      .stop_contract(component_contract, "invalid age interval")
    }
  }
  .check_scalar_number(parameters$multiplier, "multiplier", contract,
                       non_negative = TRUE)
  .check_scalar_character(parameters$unit, "unit", contract)
  if ("interpretation" %in% names(parameters)) {
    .check_scalar_character(parameters$interpretation, "interpretation", contract)
  }
}

.validate_source_provided_parameters <- function(parameters, contract) {
  required <- c(
    "statistic", "table", "measure", "selection", "unit",
    "reference_date_semantics"
  )
  .require_named_list(parameters, required, paste(contract, "source parameters"))
  if (!setequal(names(parameters), required)) {
    .stop_contract(contract, "source-provided parameters contain unexpected fields")
  }
  for (field in setdiff(required, "selection")) {
    .check_scalar_character(parameters[[field]], field, contract)
  }
  selection <- parameters$selection
  if (!is.list(selection) || is.data.frame(selection) || !length(selection) ||
      is.null(names(selection)) || any(!nzchar(names(selection))) ||
      anyDuplicated(names(selection))) {
    .stop_contract(contract, "`selection` must be a uniquely named list")
  }
  for (field in names(selection)) {
    .check_scalar_character(selection[[field]], field, contract)
  }
}

.validate_density_parameters <- function(parameters, contract) {
  required <- c(
    "numerator", "denominator", "multiplier", "unit",
    "reference_date_alignment"
  )
  .require_named_list(parameters, required, paste(contract, "density parameters"))
  if (!setequal(names(parameters), required)) {
    .stop_contract(contract, "density parameters contain unexpected fields")
  }
  components <- list(
    numerator = c(variable = "population", unit = "persons"),
    denominator = c(variable = "area", unit = "km2")
  )
  for (name in names(components)) {
    part <- parameters[[name]]
    .require_named_list(part, c("variable", "unit"), paste(contract, name))
    if (!setequal(names(part), c("variable", "unit"))) {
      .stop_contract(contract, "density components contain unexpected fields")
    }
    for (field in c("variable", "unit")) {
      .check_scalar_character(part[[field]], field, contract)
    }
    if (!identical(unlist(part, use.names = TRUE), components[[name]])) {
      .stop_contract(contract, sprintf("unsupported density %s", name))
    }
  }
  .check_scalar_number(parameters$multiplier, "multiplier", contract)
  if (parameters$multiplier <= 0) {
    .stop_contract(contract, "density multiplier must be positive")
  }
  .check_scalar_character(parameters$unit, "unit", contract)
  .check_scalar_character(
    parameters$reference_date_alignment, "reference_date_alignment", contract
  )
  if (!identical(parameters$reference_date_alignment, "exact")) {
    .stop_contract(contract, "density reference-date alignment must be exact")
  }
}

.typology_fields <- c("typology_id", "indicators", "reference_years",
                      "temporal_aggregation", "standardization", "method",
                      "method_parameters", "definition_version")

#' Validate a typology specification
#'
#' Each entry in `indicators` must contain an `indicator_id`. A
#' `definition_version` is optional, allowing a typology to name an indicator
#' whose authoritative definition has not yet been added to the package.
#'
#' @param x A named list.
#' @return `x`, invisibly.
#' @export
validate_typology_spec <- function(x) {
  contract <- "typology_spec"
  .require_named_list(x, .typology_fields, contract)
  for (field in c("typology_id", "temporal_aggregation", "standardization",
                  "method", "definition_version")) {
    .check_scalar_character(x[[field]], field, contract)
  }
  if (!is.list(x$indicators) || !length(x$indicators)) {
    .stop_contract(contract, "`indicators` must be a non-empty list")
  }
  for (i in seq_along(x$indicators)) {
    ref <- x$indicators[[i]]
    .require_named_list(ref, "indicator_id",
                        sprintf("typology_spec indicator %d", i))
    .check_scalar_character(ref$indicator_id, "indicator_id", contract)
    if ("definition_version" %in% names(ref)) {
      .check_scalar_character(ref$definition_version, "definition_version", contract)
    }
  }
  years <- x$reference_years
  if (!is.integer(years) || !length(years) || anyNA(years) ||
      any(years < 0L) || anyDuplicated(years)) {
    .stop_contract(contract, "`reference_years` must be unique non-negative integers")
  }
  if (!is.list(x$method_parameters)) {
    .stop_contract(contract, "`method_parameters` must be a list")
  }
  invisible(x)
}

.analysis_fields <- c(
  "target_geo_level", "target_geo_vintage", "surveillance_source",
  "surveillance_version", "context_source", "context_version",
  "indicator_spec_version", "typology_spec_version", "package_version"
)

#' Validate reproducible analysis provenance
#'
#' Surveillance and contextual source versions may be `NA_character_` when a
#' source genuinely does not provide a version. All other provenance fields are
#' complete.
#'
#' @param x A named list containing target geography and versioned source,
#'   indicator, typology, and package provenance.
#' @return `x`, invisibly.
#' @export
validate_analysis_spec <- function(x) {
  contract <- "analysis_spec"
  .require_named_list(x, .analysis_fields, contract)
  versions_allowing_na <- c("surveillance_version", "context_version")
  character_fields <- setdiff(
    .analysis_fields,
    c("target_geo_vintage", versions_allowing_na)
  )
  for (field in character_fields) {
    .check_scalar_character(x[[field]], field, contract)
  }
  for (field in versions_allowing_na) {
    value <- x[[field]]
    if (!is.character(value) || length(value) != 1L ||
        (!is.na(value) && !nzchar(value))) {
      .stop_contract(
        contract,
        sprintf("`%s` must be one non-empty character value or NA", field)
      )
    }
  }
  .check_date(x$target_geo_vintage, "target_geo_vintage", contract)
  if (length(x$target_geo_vintage) != 1L) {
    .stop_contract(contract, "`target_geo_vintage` must have length one")
  }
  invisible(x)
}
