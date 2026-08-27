#' Validate population denominator data
#'
#' Validates total-population observations intended for use as denominators.
#' `reference_date` is the population observation date and remains distinct
#' from `geo_vintage`, which may be `NA_Date_` when the territorial vintage has
#' not been independently established.
#'
#' Required columns are `geo_id`, `geo_name`, `geo_level`, `geo_vintage`,
#' `reference_date`, `population`, `population_basis`, `source`, `source_table`,
#' `retrieved_at`, and `data_status`. Required fields are complete except for
#' the explicit `geo_vintage` exception. Each `geo_id` and `reference_date`
#' combination must occur at most once.
#'
#' @param x A data frame or tibble.
#' @return `x`, invisibly.
#' @export
validate_population_denominator <- function(x) {
  contract <- "population_denominator"
  required <- c(
    "geo_id", "geo_name", "geo_level", "geo_vintage", "reference_date",
    "population", "population_basis", "source", "source_table",
    "retrieved_at", "data_status"
  )
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  for (field in c(
    "geo_id", "geo_name", "geo_level", "population_basis", "source",
    "source_table", "data_status"
  )) {
    .check_character(x[[field]], field, contract)
  }
  .check_date(x$geo_vintage, "geo_vintage", contract, allow_na = TRUE)
  .check_date(x$reference_date, "reference_date", contract)
  .check_numeric(x$population, "population", contract, non_negative = TRUE)
  .check_posixct(x$retrieved_at, "retrieved_at", contract)

  if (nrow(x) && length(unique(x$retrieved_at)) != 1L) {
    .stop_contract(
      contract,
      "`retrieved_at` must represent one retrieval event within a data set"
    )
  }
  key <- paste(x$geo_id, format(x$reference_date), sep = "\r")
  if (anyDuplicated(key)) {
    .stop_contract(
      contract,
      "each `geo_id` and `reference_date` combination must be unique"
    )
  }
  invisible(x)
}
