#' Validate annual demographic indicators
#'
#' Validates long-form annual district indicators. `geo_vintage` may be
#' `NA_Date_` while a source year-end identifier set has not been independently
#' matched to a canonical geography snapshot. `reference_date` does not itself
#' establish territorial vintage.
#'
#' @param x A data frame or tibble.
#' @return `x`, invisibly.
#' @export
validate_demographic_indicator <- function(x) {
  contract <- "demographic_indicator"
  required <- c(
    "geo_id", "geo_name", "geo_level", "geo_vintage", "reference_date",
    "indicator_id", "indicator_value", "indicator_unit",
    "definition_version", "value_origin", "source", "population_basis",
    "provenance_id"
  )
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  character_fields <- setdiff(required, c(
    "geo_vintage", "reference_date", "indicator_value"
  ))
  for (field in character_fields) .check_character(x[[field]], field, contract)
  .check_date(x$geo_vintage, "geo_vintage", contract, allow_na = TRUE)
  .check_date(x$reference_date, "reference_date", contract)
  .check_numeric(x$indicator_value, "indicator_value", contract,
                 non_negative = TRUE)
  if (any(x$geo_level != "district")) {
    .stop_contract(contract, "`geo_level` must be exactly \"district\"")
  }
  if (any(format(x$reference_date, "%m-%d") != "12-31")) {
    .stop_contract(contract, "`reference_date` must be 31 December")
  }
  origins <- c("source_provided", "derived")
  if (any(!x$value_origin %in% origins)) {
    .stop_contract(
      contract, "`value_origin` must be source_provided or derived"
    )
  }
  key <- paste(
    x$geo_id, format(x$reference_date), x$indicator_id,
    x$definition_version, sep = "\r"
  )
  if (anyDuplicated(key)) {
    .stop_contract(contract, "indicator observations must have unique keys")
  }
  geography_key <- paste(x$geo_id, format(x$reference_date), sep = "\r")
  mapping <- paste(x$geo_name, x$geo_level, sep = "\r")
  if (any(vapply(split(mapping, geography_key), function(value) {
    length(unique(value)) != 1L
  }, logical(1L)))) {
    .stop_contract(contract, "a geographic key must have one name and level")
  }
  invisible(x)
}

#' Validate annual Regionaldatenbank district area
#'
#' Area is represented in square kilometres. Values must be finite and
#' strictly positive. `geo_vintage` may remain unresolved at the source-adapter
#' stage and is never inferred from `reference_date`.
#'
#' @param x A data frame or tibble.
#' @return `x`, invisibly.
#' @export
validate_regional_area <- function(x) {
  contract <- "regional_area"
  required <- c(
    "geo_id", "geo_name", "geo_level", "geo_vintage", "reference_date",
    "area_km2", "source", "source_table", "source_measure",
    "retrieved_at", "data_status", "provenance_id"
  )
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  for (field in setdiff(required, c(
    "geo_vintage", "reference_date", "area_km2", "retrieved_at"
  ))) .check_character(x[[field]], field, contract)
  .check_date(x$geo_vintage, "geo_vintage", contract, allow_na = TRUE)
  .check_date(x$reference_date, "reference_date", contract)
  .check_posixct(x$retrieved_at, "retrieved_at", contract)
  .check_numeric(x$area_km2, "area_km2", contract)
  if (any(x$area_km2 <= 0)) {
    .stop_contract(contract, "`area_km2` must be strictly positive")
  }
  if (any(x$geo_level != "district") ||
      any(format(x$reference_date, "%m-%d") != "12-31")) {
    .stop_contract(contract, "area must describe districts at 31 December")
  }
  if (any(x$source_table != "11111-01-01-4") ||
      any(x$source_measure != "FLC006")) {
    .stop_contract(contract, "area source table or measure is not approved")
  }
  key <- paste(x$geo_id, format(x$reference_date), sep = "\r")
  if (anyDuplicated(key)) {
    .stop_contract(contract, "area observations must have unique keys")
  }
  invisible(x)
}
