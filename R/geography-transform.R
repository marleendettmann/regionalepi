#' Aggregate additive values to a coarser geography
#'
#' `aggregate_geography()` handles differences in spatial resolution using
#' `identity` and `aggregate` relations. In Geography Layer v0.1, every
#' non-empty input must contain exactly one common `geo_vintage`; multi-vintage
#' input is not supported. The output preserves that vintage.
#'
#' Target names and levels always come from the single row of
#' `target_geography` that is valid at the input vintage. Values are aggregated
#' only after the caller explicitly declares their additive semantics and all
#' non-geographic grouping dimensions.
#'
#' @param data A data frame or tibble containing canonical `geo_id`, `geo_name`,
#'   `geo_level`, and `geo_vintage` columns, grouping columns, and additive
#'   value columns.
#' @param relations A data frame or tibble satisfying the
#'   `geography_relations` contract.
#' @param target_geography A canonical geography register satisfying the
#'   `geography` contract. Exactly one row per target ID must be valid at the
#'   relevant vintage; the ID may occur in other historical rows.
#' @param value_cols Character vector naming numeric additive value columns.
#' @param group_cols Character vector naming every non-geographic dimension to
#'   preserve. No dimensions are inferred.
#' @param value_semantics The declared value semantics. Only `"additive"` is
#'   supported in v0.1.
#' @param tolerance A finite positive numeric tolerance for mass-balance checks.
#' @return A list with elements `data` and `diagnostics`.
#' @export
aggregate_geography <- function(data, relations, target_geography, value_cols,
                                group_cols, value_semantics = "additive",
                                tolerance = sqrt(.Machine$double.eps)) {
  .validate_transform_arguments(
    data, relations, target_geography, value_cols, group_cols,
    value_semantics, tolerance
  )
  if (!nrow(data)) {
    return(.empty_transform_result(
      data, operation = "aggregate_geography", source_vintage = as.Date(NA),
      target_vintage = as.Date(NA), value_cols = value_cols,
      value_semantics = value_semantics
    ))
  }
  vintages <- unique(data$geo_vintage)
  if (length(vintages) != 1L) {
    .stop_contract(
      "aggregate_geography",
      "non-empty input must have exactly one common `geo_vintage` in v0.1"
    )
  }
  .transform_geography(
    data = data,
    relations = relations,
    target_geography = target_geography,
    source_vintage = vintages,
    target_vintage = vintages,
    value_cols = value_cols,
    group_cols = group_cols,
    value_semantics = value_semantics,
    tolerance = tolerance,
    operation = "aggregate_geography",
    allowed_types = c("identity", "aggregate")
  )
}

#' Harmonize additive values to a target geographic vintage
#'
#' `harmonize_vintage()` handles historical territorial changes using
#' `identity` and `historical_merge` relations. Every non-empty input row must
#' have `geo_vintage` equal to `source_vintage`; output `geo_vintage` is set to
#' `target_vintage`.
#'
#' Target names and levels always come from the single row of
#' `target_geography` that is valid at `target_vintage`. No split or boundary
#' allocation is performed.
#'
#' @inheritParams aggregate_geography
#' @param source_vintage A complete scalar `Date` identifying the input vintage.
#' @param target_vintage A complete scalar `Date` identifying the output vintage.
#' @return A list with elements `data` and `diagnostics`.
#' @export
harmonize_vintage <- function(data, relations, target_geography,
                              source_vintage, target_vintage, value_cols,
                              group_cols, value_semantics = "additive",
                              tolerance = sqrt(.Machine$double.eps)) {
  .validate_transform_arguments(
    data, relations, target_geography, value_cols, group_cols,
    value_semantics, tolerance
  )
  .check_scalar_date(source_vintage, "source_vintage", "harmonize_vintage")
  .check_scalar_date(target_vintage, "target_vintage", "harmonize_vintage")
  if (!nrow(data)) {
    output <- data
    output$geo_vintage <- as.Date(character())
    return(.empty_transform_result(
      output, operation = "harmonize_vintage",
      source_vintage = source_vintage, target_vintage = target_vintage,
      value_cols = value_cols, value_semantics = value_semantics
    ))
  }
  if (any(data$geo_vintage != source_vintage)) {
    .stop_contract(
      "harmonize_vintage",
      "all input `geo_vintage` values must equal `source_vintage`"
    )
  }
  .transform_geography(
    data = data,
    relations = relations,
    target_geography = target_geography,
    source_vintage = source_vintage,
    target_vintage = target_vintage,
    value_cols = value_cols,
    group_cols = group_cols,
    value_semantics = value_semantics,
    tolerance = tolerance,
    operation = "harmonize_vintage",
    allowed_types = c("identity", "historical_merge")
  )
}

.validate_transform_arguments <- function(data, relations, target_geography,
                                          value_cols, group_cols,
                                          value_semantics, tolerance) {
  contract <- "geography transformation"
  .require_data_frame(data, contract)
  .require_columns(
    data, c("geo_id", "geo_name", "geo_level", "geo_vintage"), contract
  )
  if (anyDuplicated(names(data))) {
    .stop_contract(contract, "input column names must be unique")
  }
  .check_character(data$geo_id, "geo_id", contract)
  .check_character(data$geo_name, "geo_name", contract)
  .check_character(data$geo_level, "geo_level", contract)
  .check_date(data$geo_vintage, "geo_vintage", contract)
  validate_geography_relations(relations)
  validate_geography(target_geography)

  .check_column_argument(value_cols, "value_cols", allow_empty = FALSE)
  .check_column_argument(group_cols, "group_cols", allow_empty = TRUE)
  overlap <- intersect(value_cols, group_cols)
  if (length(overlap)) {
    .stop_contract(
      contract,
      sprintf("columns cannot be both values and groups: %s", paste(overlap, collapse = ", "))
    )
  }
  reserved <- c("geo_id", "geo_name", "geo_level", "geo_vintage")
  reserved_arguments <- intersect(c(value_cols, group_cols), reserved)
  if (length(reserved_arguments)) {
    .stop_contract(
      contract,
      sprintf("geographic columns cannot be value/group columns: %s",
              paste(unique(reserved_arguments), collapse = ", "))
    )
  }
  requested <- c(value_cols, group_cols)
  missing <- setdiff(requested, names(data))
  if (length(missing)) {
    .stop_contract(
      contract,
      sprintf("missing requested column(s): %s", paste(missing, collapse = ", "))
    )
  }
  unclassified <- setdiff(names(data), c(reserved, requested))
  if (length(unclassified)) {
    .stop_contract(
      contract,
      sprintf("unclassified input column(s): %s", paste(unclassified, collapse = ", "))
    )
  }
  for (field in value_cols) {
    .check_numeric(data[[field]], field, contract)
  }
  for (field in group_cols) {
    if (is.list(data[[field]]) || is.data.frame(data[[field]])) {
      .stop_contract(contract, sprintf("group column `%s` must be an atomic vector", field))
    }
  }
  if (!is.character(value_semantics) || length(value_semantics) != 1L ||
      is.na(value_semantics) || !identical(value_semantics, "additive")) {
    .stop_contract(contract, "`value_semantics` must be exactly \"additive\" in v0.1")
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) ||
      !is.finite(tolerance) || tolerance <= 0) {
    .stop_contract(contract, "`tolerance` must be one finite positive numeric value")
  }
  invisible(TRUE)
}

.check_column_argument <- function(x, field, allow_empty) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(x)) || anyDuplicated(x) ||
      (!allow_empty && !length(x))) {
    qualification <- if (allow_empty) "a unique character vector" else "a non-empty unique character vector"
    .stop_contract("geography transformation", sprintf("`%s` must be %s", field, qualification))
  }
}

.check_scalar_date <- function(x, field, contract) {
  .check_date(x, field, contract)
  if (length(x) != 1L) {
    .stop_contract(contract, sprintf("`%s` must have length one", field))
  }
}

.transform_geography <- function(data, relations, target_geography,
                                 source_vintage, target_vintage, value_cols,
                                 group_cols, value_semantics, tolerance,
                                 operation, allowed_types) {
  source_ids <- unique(data$geo_id)
  candidates <- relations[
    relations$from_vintage == source_vintage &
      relations$to_vintage == target_vintage &
      relations$from_geo_id %in% source_ids,
    , drop = FALSE
  ]
  unmatched <- setdiff(source_ids, unique(candidates$from_geo_id))
  if (length(unmatched)) {
    .stop_contract(
      operation,
      sprintf(
        "unmatched `geo_id` value(s) for source/target vintages: %s",
        paste(unmatched, collapse = ", ")
      )
    )
  }
  unsupported <- setdiff(unique(candidates$relation_type), allowed_types)
  if (length(unsupported)) {
    .stop_contract(
      operation,
      sprintf("unsupported relation type(s) for this operation: %s",
              paste(unsupported, collapse = ", "))
    )
  }
  fractional <- !is.na(candidates$weight) & candidates$weight != 1
  if (any(fractional)) {
    .stop_contract(
      operation,
      "fractional relation weights require allocation, which is unsupported in v0.1"
    )
  }

  mapping_fields <- c("from_geo_id", "to_geo_id", "relation_type")
  mapping <- unique(candidates[mapping_fields])
  counts <- table(mapping$from_geo_id)
  ambiguous <- names(counts[counts > 1L])
  if (length(ambiguous)) {
    .stop_contract(
      operation,
      sprintf("ambiguous mapping for `geo_id` value(s): %s",
              paste(ambiguous, collapse = ", "))
    )
  }
  mapping <- mapping[match(source_ids, mapping$from_geo_id), , drop = FALSE]
  targets <- .target_rows_at_vintage(
    target_geography, unique(mapping$to_geo_id), target_vintage, operation
  )
  map_index <- match(data$geo_id, mapping$from_geo_id)
  target_index <- match(mapping$to_geo_id[map_index], targets$geo_id)

  grouping <- data.frame(
    geo_id = targets$geo_id[target_index],
    geo_name = targets$geo_name[target_index],
    geo_level = targets$geo_level[target_index],
    geo_vintage = rep(target_vintage, nrow(data)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (length(group_cols)) {
    grouping <- cbind(grouping, data[group_cols])
  }
  transformed <- .sum_by_groups(grouping, data[value_cols], value_cols)
  balance <- .mass_balance(data, transformed, group_cols, value_cols, tolerance)
  if (any(!balance$balanced)) {
    failed <- balance$value_col[!balance$balanced]
    .stop_contract(
      operation,
      sprintf("mass-balance check failed for value column(s): %s",
              paste(failed, collapse = ", "))
    )
  }

  list(
    data = transformed,
    diagnostics = list(
      operation = operation,
      source_vintage = source_vintage,
      target_vintage = target_vintage,
      input_rows = nrow(data),
      input_units = length(source_ids),
      mapped_units = length(unique(mapping$from_geo_id)),
      output_rows = nrow(transformed),
      output_units = length(unique(transformed$geo_id)),
      relation_rows_used = nrow(candidates),
      relations_used = candidates,
      relation_types_used = sort(unique(candidates$relation_type)),
      unmatched_units = character(),
      unmatched_unit_count = 0L,
      ambiguous_units = character(),
      ambiguous_unit_count = 0L,
      value_semantics = value_semantics,
      mass_balance = balance
    )
  )
}

.target_rows_at_vintage <- function(geography, target_ids, vintage, operation) {
  applicable <- geography[
    geography$geo_id %in% target_ids &
      geography$valid_from <= vintage &
      (is.na(geography$valid_to) | geography$valid_to >= vintage),
    , drop = FALSE
  ]
  counts <- table(factor(applicable$geo_id, levels = target_ids))
  missing <- names(counts[counts == 0L])
  overlapping <- names(counts[counts > 1L])
  if (length(missing)) {
    .stop_contract(
      operation,
      sprintf("no target geography row is valid at the target vintage for: %s",
              paste(missing, collapse = ", "))
    )
  }
  if (length(overlapping)) {
    .stop_contract(
      operation,
      sprintf("multiple target geography rows are valid at the target vintage for: %s",
              paste(overlapping, collapse = ", "))
    )
  }
  applicable[match(target_ids, applicable$geo_id), , drop = FALSE]
}

.sum_by_groups <- function(groups, values, value_cols) {
  if (!nrow(groups)) {
    output <- groups
    for (field in value_cols) output[[field]] <- values[[field]]
    return(output)
  }
  if (!ncol(groups)) {
    output <- groups[1L, , drop = FALSE]
    for (field in value_cols) output[[field]] <- sum(values[[field]])
    rownames(output) <- NULL
    return(output)
  }
  factors <- lapply(groups, function(x) addNA(factor(x, exclude = NULL)))
  key <- do.call(interaction, c(factors, list(drop = TRUE, lex.order = TRUE)))
  levels_in_order <- unique(as.character(key))
  pieces <- lapply(levels_in_order, function(level) {
    rows <- which(as.character(key) == level)
    out <- groups[rows[1L], , drop = FALSE]
    for (field in value_cols) out[[field]] <- sum(values[[field]][rows])
    out
  })
  result <- do.call(rbind, pieces)
  rownames(result) <- NULL
  result
}

.mass_balance <- function(before, after, group_cols, value_cols, tolerance) {
  before_groups <- before[group_cols]
  after_groups <- after[group_cols]
  before_summary <- .sum_by_groups(before_groups, before[value_cols], value_cols)
  after_summary <- .sum_by_groups(after_groups, after[value_cols], value_cols)
  if (length(group_cols)) {
    before_order <- do.call(order, c(before_summary[group_cols], list(na.last = TRUE)))
    after_order <- do.call(order, c(after_summary[group_cols], list(na.last = TRUE)))
    before_summary <- before_summary[before_order, , drop = FALSE]
    after_summary <- after_summary[after_order, , drop = FALSE]
    same_groups <- identical(before_summary[group_cols], after_summary[group_cols])
  } else {
    same_groups <- nrow(before_summary) == nrow(after_summary)
  }
  rows <- lapply(value_cols, function(field) {
    before_values <- before_summary[[field]]
    after_values <- after_summary[[field]]
    differences <- if (length(before_values) == length(after_values)) {
      abs(before_values - after_values)
    } else {
      Inf
    }
    limits <- if (length(before_values) == length(after_values)) {
      tolerance * pmax(1, abs(before_values), abs(after_values))
    } else {
      0
    }
    data.frame(
      value_col = field,
      before_total = sum(before[[field]]),
      after_total = sum(after[[field]]),
      groups_checked = nrow(before_summary),
      max_group_difference = if (length(differences)) max(differences) else 0,
      balanced = same_groups && all(differences <= limits),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.empty_transform_result <- function(data, operation, source_vintage,
                                    target_vintage, value_cols,
                                    value_semantics) {
  balance <- data.frame(
    value_col = value_cols,
    before_total = rep(0, length(value_cols)),
    after_total = rep(0, length(value_cols)),
    groups_checked = rep(0L, length(value_cols)),
    max_group_difference = rep(0, length(value_cols)),
    balanced = rep(TRUE, length(value_cols)),
    stringsAsFactors = FALSE
  )
  list(
    data = data,
    diagnostics = list(
      operation = operation,
      source_vintage = source_vintage,
      target_vintage = target_vintage,
      input_rows = 0L,
      input_units = 0L,
      mapped_units = 0L,
      output_rows = 0L,
      output_units = 0L,
      relation_rows_used = 0L,
      relations_used = NULL,
      relation_types_used = character(),
      unmatched_units = character(),
      unmatched_unit_count = 0L,
      ambiguous_units = character(),
      ambiguous_unit_count = 0L,
      value_semantics = value_semantics,
      mass_balance = balance
    )
  )
}
