#' Validate canonical source-provided surveillance incidence
#'
#' Requires the common surveillance geography, time, pathogen, and source
#' fields plus `incidence` and `query_id`. Incidence is a non-additive
#' source-provided rate. Numeric zero is retained as zero; `NA_real_` is
#' permitted only to preserve a blank or unavailable source incidence and must
#' never be fabricated or interpreted as zero.
#'
#' This contract is intentionally distinct from the additive, whole-valued
#' `cases` contract validated by [validate_surveillance()]. Incidence data must
#' not be passed to additive geography transformations.
#'
#' @param x A data frame or tibble.
#' @return `x`, invisibly.
#' @export
validate_surveillance_incidence <- function(x) {
  contract <- "surveillance_incidence"
  .validate_surveillance_geography(x, contract)
  .require_columns(x, c("incidence", "query_id"), contract)
  .check_numeric(
    x$incidence, "incidence", contract,
    non_negative = TRUE, allow_na = TRUE
  )
  .check_character(x$query_id, "query_id", contract)
  .check_optional(
    x, "reporting_year", .check_whole_number, contract,
    non_negative = TRUE
  )
  .check_optional(
    x, "reporting_week", .check_whole_number, contract,
    non_negative = TRUE
  )
  .check_optional(x, "data_status", .check_posixct, contract, allow_na = TRUE)
  .check_optional(
    x, "reference_definition", .check_incidence_logical, contract,
    allow_na = TRUE
  )
  .check_optional(
    x, "reporting_path", .check_character, contract, allow_na = TRUE
  )
  invisible(x)
}

.check_incidence_logical <- function(x, field, contract, allow_na = FALSE) {
  if (!is.logical(x)) {
    .stop_contract(contract, sprintf("`%s` must be logical", field))
  }
  if (!allow_na && anyNA(x)) {
    .stop_contract(contract, sprintf("`%s` must not contain NA", field))
  }
}
