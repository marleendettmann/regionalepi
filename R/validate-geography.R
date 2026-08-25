#' Validate a canonical geographic register
#'
#' @param x A data frame or tibble with `geo_id`, `geo_name`, `geo_level`,
#'   `valid_from`, `valid_to`, and `source`. Optional fields are
#'   `parent_geo_id` and `geometry`.
#'   Required fields are complete except that `valid_to` may be `NA` for a
#'   currently valid geography.
#' @return `x`, invisibly.
#' @export
validate_geography <- function(x) {
  contract <- "geography"
  required <- c("geo_id", "geo_name", "geo_level", "valid_from", "valid_to", "source")
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  for (field in c("geo_id", "geo_name", "geo_level", "source")) {
    .check_character(x[[field]], field, contract)
  }
  .check_date(x$valid_from, "valid_from", contract)
  .check_date(x$valid_to, "valid_to", contract, allow_na = TRUE)
  if (any(!is.na(x$valid_to) & x$valid_to < x$valid_from)) {
    .stop_contract(contract, "`valid_to` must not precede `valid_from`")
  }
  .check_optional(x, "parent_geo_id", .check_character, contract, allow_na = TRUE)
  invisible(x)
}

#' Validate relations between geographic vintages
#'
#' Allowed relation types are `identity`, `aggregate`, `historical_merge`,
#' `historical_split`, and `boundary_change`. Optional weights are allocation
#' proportions and must be finite values from zero to one.
#' Required fields are complete except that `weight` and `note` may be `NA`.
#'
#' @param x A data frame or tibble.
#' @return `x`, invisibly.
#' @export
validate_geography_relations <- function(x) {
  contract <- "geography_relations"
  required <- c("from_geo_id", "from_vintage", "to_geo_id", "to_vintage",
                "relation_type", "weight", "source", "note")
  allowed <- c("identity", "aggregate", "historical_merge", "historical_split",
               "boundary_change")
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  for (field in c("from_geo_id", "to_geo_id", "relation_type", "source")) {
    .check_character(x[[field]], field, contract)
  }
  .check_date(x$from_vintage, "from_vintage", contract)
  .check_date(x$to_vintage, "to_vintage", contract)
  .check_numeric(x$weight, "weight", contract, allow_na = TRUE)
  if (any(x$weight[!is.na(x$weight)] < 0 | x$weight[!is.na(x$weight)] > 1)) {
    .stop_contract(contract, "`weight` must be an allocation proportion in [0, 1]")
  }
  .check_character(x$note, "note", contract, allow_na = TRUE)
  invalid <- setdiff(unique(x$relation_type), allowed)
  if (length(invalid)) {
    .stop_contract(contract, sprintf("invalid `relation_type`: %s", paste(invalid, collapse = ", ")))
  }
  invisible(x)
}
