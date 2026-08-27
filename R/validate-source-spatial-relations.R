#' Validate source-specific spatial aggregation relations
#'
#' Source spatial relations are reviewed, source-version-specific mappings used
#' to reconcile source spatial resolution. Their applicability dates are review
#' and register-selection provenance, not territorial vintages. Missing
#' `source_version` values use exact missing-value semantics and are never
#' wildcards.
#'
#' @param x A data frame or tibble with `source`, `source_version`,
#'   `from_geo_id`, `from_geo_level`, `to_geo_id`, `relation_type`,
#'   `valid_from`, `valid_to`, `review_status`, and `reason`.
#' @return `x`, invisibly.
#' @export
validate_source_spatial_relations <- function(x) {
  contract <- "source_spatial_relations"
  required <- c(
    "source", "source_version", "from_geo_id", "from_geo_level",
    "to_geo_id", "relation_type", "valid_from", "valid_to",
    "review_status", "reason"
  )
  .require_data_frame(x, contract)
  if (anyDuplicated(names(x))) {
    .stop_contract(contract, "column names must be unique")
  }
  .require_columns(x, required, contract)
  for (field in setdiff(required, c("source_version", "valid_from", "valid_to"))) {
    .check_character(x[[field]], field, contract)
  }
  .check_character(x$source_version, "source_version", contract, allow_na = TRUE)
  .check_date(x$valid_from, "valid_from", contract)
  .check_date(x$valid_to, "valid_to", contract, allow_na = TRUE)
  if (any(!is.na(x$valid_to) & x$valid_to < x$valid_from)) {
    .stop_contract(contract, "`valid_to` must not precede `valid_from`")
  }
  invalid <- setdiff(unique(x$relation_type), c("aggregate", "identity"))
  if (length(invalid)) {
    .stop_contract(
      contract,
      sprintf("invalid `relation_type`: %s", paste(invalid, collapse = ", "))
    )
  }
  if (any(x$review_status != "reviewed")) {
    .stop_contract(contract, "`review_status` must be exactly \"reviewed\" in v0.1")
  }
  bad_identity <- x$relation_type == "identity" & x$from_geo_id != x$to_geo_id
  if (any(bad_identity)) {
    .stop_contract(contract, "`identity` relations must map an identifier to itself")
  }
  bad_aggregate <- x$relation_type == "aggregate" & x$from_geo_id == x$to_geo_id
  if (any(bad_aggregate)) {
    .stop_contract(contract, "`aggregate` relations must map to a different identifier")
  }
  .check_source_relation_interval_overlap(x, contract)
  invisible(x)
}

.check_source_relation_interval_overlap <- function(x, contract) {
  if (nrow(x) < 2L) return(invisible(TRUE))
  versions <- ifelse(
    is.na(x$source_version), "<NA>",
    paste0(nchar(x$source_version), ":", x$source_version)
  )
  keys <- paste(x$source, versions, x$from_geo_id, x$from_geo_level, sep = "|")
  for (key in unique(keys)) {
    rows <- which(keys == key)
    if (length(rows) < 2L) next
    for (left in seq_len(length(rows) - 1L)) {
      for (right in seq.int(left + 1L, length(rows))) {
        i <- rows[[left]]
        j <- rows[[right]]
        i_end <- if (is.na(x$valid_to[[i]])) as.Date("9999-12-31") else x$valid_to[[i]]
        j_end <- if (is.na(x$valid_to[[j]])) as.Date("9999-12-31") else x$valid_to[[j]]
        if (x$valid_from[[i]] <= j_end && x$valid_from[[j]] <= i_end) {
          .stop_contract(
            contract,
            "applicability intervals must not overlap for the same source/version/from ID/level"
          )
        }
      }
    }
  }
  invisible(TRUE)
}
