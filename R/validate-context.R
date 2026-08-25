#' Validate canonical population base data
#'
#' Requires `geo_id`, `geo_name`, `geo_level`, `geo_vintage`,
#' `reference_date`, `age_from`, `age_to`, `population`, `population_basis`,
#' and `source`. Optional fields are `sex` and `retrieved_at`.
#' Required fields are complete except that `age_to` may be `NA` for an
#' open-ended group. Age boundaries are non-negative whole-valued numerics and
#' may use either integer or double storage.
#'
#' @param x A data frame or tibble.
#' @return `x`, invisibly.
#' @export
validate_context_population <- function(x) {
  contract <- "context_population"
  required <- c("geo_id", "geo_name", "geo_level", "geo_vintage",
                "reference_date", "age_from", "age_to", "population",
                "population_basis", "source")
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  for (field in c("geo_id", "geo_name", "geo_level", "population_basis", "source")) {
    .check_character(x[[field]], field, contract)
  }
  .check_date(x$geo_vintage, "geo_vintage", contract)
  .check_date(x$reference_date, "reference_date", contract)
  .check_whole_number(x$age_from, "age_from", contract, non_negative = TRUE)
  .check_whole_number(x$age_to, "age_to", contract, allow_na = TRUE,
                      non_negative = TRUE)
  bad_interval <- !is.na(x$age_to) & x$age_to < x$age_from
  if (any(bad_interval)) {
    .stop_contract(contract, "`age_to` must be at least `age_from`, or NA")
  }
  .check_numeric(x$population, "population", contract, non_negative = TRUE)
  .check_optional(x, "sex", .check_character, contract, allow_na = TRUE)
  .check_optional(x, "retrieved_at", .check_posixct, contract, allow_na = TRUE)
  invisible(x)
}

#' Validate generic contextual base quantities
#'
#' @param x A data frame or tibble with `geo_id`, `geo_name`, `geo_level`,
#'   `geo_vintage`, `reference_date`, `variable`, `value`, `unit`, and `source`.
#'   Required fields must be complete.
#' @return `x`, invisibly.
#' @export
validate_context_base <- function(x) {
  contract <- "context_base"
  required <- c("geo_id", "geo_name", "geo_level", "geo_vintage",
                "reference_date", "variable", "value", "unit", "source")
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  for (field in c("geo_id", "geo_name", "geo_level", "variable", "unit", "source")) {
    .check_character(x[[field]], field, contract)
  }
  .check_date(x$geo_vintage, "geo_vintage", contract)
  .check_date(x$reference_date, "reference_date", contract)
  .check_numeric(x$value, "value", contract)
  invisible(x)
}
