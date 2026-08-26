#' Validate reviewed geography aliases
#'
#' Geography aliases are exact, source-specific, reviewed mappings from one
#' complete source label to a canonical target identifier. They are not text
#' normalization or fuzzy-matching rules. `source_version` may be
#' `NA_character_`; `NA` has exact missing-value semantics and is not a
#' wildcard.
#'
#' @param x A data frame or tibble with `source`, `source_version`,
#'   `source_label`, `source_type`, `target_geo_id`, `valid_from`, `valid_to`,
#'   `reason`, and `review_status`. Optional `target_vghid` is character.
#' @return `x`, invisibly.
#' @export
validate_geography_aliases <- function(x) {
  contract <- "geography aliases"
  required <- c(
    "source", "source_version", "source_label", "source_type",
    "target_geo_id", "valid_from", "valid_to", "reason", "review_status"
  )
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  if (anyDuplicated(names(x))) {
    .stop_contract(contract, "column names must be unique")
  }
  for (field in c(
    "source", "source_label", "source_type", "target_geo_id", "reason",
    "review_status"
  )) {
    .check_character(x[[field]], field, contract)
  }
  .check_character(x$source_version, "source_version", contract, allow_na = TRUE)
  .check_date(x$valid_from, "valid_from", contract)
  .check_date(x$valid_to, "valid_to", contract, allow_na = TRUE)
  if (any(!is.na(x$valid_to) & x$valid_to < x$valid_from)) {
    .stop_contract(contract, "`valid_to` must not precede `valid_from`")
  }
  if (any(x$review_status != "reviewed")) {
    .stop_contract(contract, "`review_status` must be exactly \"reviewed\" in v0.1")
  }
  .check_optional(x, "target_vghid", .check_character, contract, allow_na = TRUE)
  invisible(x)
}
