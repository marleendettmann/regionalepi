#' Validate canonical surveillance data
#'
#' Requires `geo_id`, `geo_name`, `geo_level`, `geo_vintage`, `date`,
#' `time_unit`, `pathogen`, `cases`, and `source`. Optional fields are
#' `age_group`, `sex`, `retrieved_at`, and `source_version`.
#'
#' `geo_id` is always character, but may contain `NA` only at the initial
#' canonical/source-adapter stage. Future analytical processing must first
#' resolve or explicitly diagnose those values. Other required fields are
#' complete. Optional fields may contain `NA` when their values are unavailable.
#'
#' @param x A data frame or tibble.
#' @return `x`, invisibly.
#' @export
validate_surveillance <- function(x) {
  contract <- "surveillance"
  required <- c("geo_id", "geo_name", "geo_level", "geo_vintage", "date",
                "time_unit", "pathogen", "cases", "source")
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  .check_character(x$geo_id, "geo_id", contract, allow_na = TRUE)
  for (field in c("geo_name", "geo_level", "time_unit", "pathogen", "source")) {
    .check_character(x[[field]], field, contract)
  }
  .check_date(x$geo_vintage, "geo_vintage", contract)
  .check_date(x$date, "date", contract)
  .check_numeric(x$cases, "cases", contract, non_negative = TRUE, whole = TRUE)
  for (field in c("age_group", "sex", "source_version")) {
    .check_optional(x, field, .check_character, contract, allow_na = TRUE)
  }
  .check_optional(x, "retrieved_at", .check_posixct, contract, allow_na = TRUE)
  invisible(x)
}
