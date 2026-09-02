#' Validate canonical surveillance data
#'
#' Requires `geo_id`, `geo_name`, `geo_level`, `geo_vintage`, `date`,
#' `time_unit`, `pathogen`, `cases`, and `source`. Optional fields are
#' `age_group`, `sex`, `retrieved_at`, and `source_version`.
#'
#' `geo_id` is always character and `geo_vintage` is always `Date`, but either
#' may contain `NA` only at the initial canonical/source-adapter stage. Future
#' geographic or analytical processing must first resolve or explicitly
#' diagnose those values. Other required fields are complete. Optional fields
#' may contain `NA` when their values are unavailable.
#'
#' @param x A data frame or tibble.
#' @return `x`, invisibly.
#' @export
validate_surveillance <- function(x) {
  contract <- "surveillance"
  .validate_surveillance_geography(x, contract)
  .require_columns(x, "cases", contract)
  .check_numeric(
    x$cases, "cases", contract, non_negative = TRUE, whole = TRUE,
    allow_na = TRUE
  )
  invisible(x)
}

.validate_surveillance_geography <- function(x, contract) {
  required <- c(
    "geo_id", "geo_name", "geo_level", "geo_vintage", "date",
    "time_unit", "pathogen", "source"
  )
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  .check_character(x$geo_id, "geo_id", contract, allow_na = TRUE)
  for (field in c("geo_name", "geo_level", "time_unit", "pathogen", "source")) {
    .check_character(x[[field]], field, contract)
  }
  .check_date(x$geo_vintage, "geo_vintage", contract, allow_na = TRUE)
  .check_date(x$date, "date", contract)
  for (field in c("age_group", "sex", "source_version")) {
    .check_optional(x, field, .check_character, contract, allow_na = TRUE)
  }
  .check_optional(x, "retrieved_at", .check_posixct, contract, allow_na = TRUE)
  invisible(x)
}

.validate_surveillance_observations <- function(x) {
  has_cases <- "cases" %in% names(x)
  has_incidence <- "incidence" %in% names(x)
  if (identical(has_cases, has_incidence)) {
    .stop_contract(
      "surveillance observations",
      "must contain exactly one of `cases` or `incidence`"
    )
  }
  if (has_cases) validate_surveillance(x) else validate_surveillance_incidence(x)
}
