#' Validate annual-average population data
#'
#' Validates official district annual-average population keyed by reporting
#' year. This contract is distinct from the 31 December population contract
#' validated by [validate_population_denominator()]. `geo_vintage` may remain
#' `NA_Date_` because a reporting year does not establish a territorial
#' vintage.
#'
#' @param x A data frame or tibble.
#' @return `x`, invisibly.
#' @export
validate_annual_average_population <- function(x) {
  contract <- "annual_average_population"
  required <- c(
    "geo_id", "geo_name", "geo_level", "geo_vintage", "year",
    "population", "population_measure", "population_reference",
    "population_basis", "source", "source_table", "retrieved_at",
    "data_status", "provenance_id"
  )
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  for (field in c(
    "geo_id", "geo_name", "geo_level", "population_measure",
    "population_reference", "population_basis", "source", "source_table",
    "data_status", "provenance_id"
  )) {
    .check_character(x[[field]], field, contract)
  }
  .check_date(x$geo_vintage, "geo_vintage", contract, allow_na = TRUE)
  .check_whole_number(x$year, "year", contract)
  .check_numeric(x$population, "population", contract, non_negative = TRUE)
  .check_posixct(x$retrieved_at, "retrieved_at", contract)

  if (any(!grepl("^[0-9]{5}$", x$geo_id))) {
    .stop_contract(contract, "`geo_id` must contain five-character district identifiers")
  }
  if (any(x$year < 0)) {
    .stop_contract(contract, "`year` must be non-negative")
  }
  if (any(x$population <= 0)) {
    .stop_contract(contract, "`population` must be strictly positive")
  }
  if (any(x$geo_level != "district")) {
    .stop_contract(contract, "`geo_level` must be district")
  }
  if (any(x$population_measure != "annual_average_population")) {
    .stop_contract(contract, "`population_measure` must be annual_average_population")
  }
  if (any(x$population_reference != "reporting_year_annual_average")) {
    .stop_contract(
      contract,
      "`population_reference` must be reporting_year_annual_average"
    )
  }
  if (any(x$source_table != "12411-05-01-4")) {
    .stop_contract(contract, "`source_table` must be 12411-05-01-4")
  }
  if (nrow(x) && length(unique(x$retrieved_at)) != 1L) {
    .stop_contract(contract, "`retrieved_at` must describe one retrieval event")
  }
  key <- paste(x$geo_id, x$year, sep = "\r")
  if (anyDuplicated(key)) {
    .stop_contract(contract, "each `geo_id` and `year` combination must be unique")
  }
  invisible(x)
}
