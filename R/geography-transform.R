#' Aggregate additive values to a coarser geography
#'
#' `aggregate_geography()` handles source-specific differences in spatial
#' resolution using reviewed source spatial relations. Rows without an
#' applicable relation pass through only when their current identifier has one
#' uniquely applicable same-ID row in the canonical target register.
#'
#' Target names and levels always come from the single row of
#' `target_geography` that is valid at `reference_date`. The reference date is
#' relation/register applicability provenance, not a territorial vintage.
#' `geo_vintage` is optional, is never inferred or changed, and automatically
#' prevents rows with different values from being combined.
#'
#' The known resolver provenance columns `source_geo_id`, `source_geo_name`, and
#' `source_geo_level` are omitted from aggregated output rather than becoming
#' grouping dimensions. Their authoritative detail remains in the separate
#' resolution audit. Any other unclassified column is an error.
#'
#' @param data A data frame or tibble containing `geo_id`, `geo_name`,
#'   `geo_level`, `source`, `source_version`, optional `geo_vintage`, grouping
#'   columns, additive value columns, and optionally the three known resolver
#'   provenance columns.
#' @param source_spatial_relations A data frame or tibble satisfying the
#'   `source_spatial_relations` contract.
#' @param target_geography A canonical geography register satisfying the
#'   `geography` contract. Exactly one row per target ID must be valid at the
#'   relevant reference date; the ID may occur in other historical rows.
#' @param reference_date Complete scalar `Date` selecting applicable relation
#'   and canonical-register rows. It is not a territorial vintage.
#' @param value_cols Character vector naming numeric additive value columns.
#' @param group_cols Character vector naming every non-geographic dimension to
#'   preserve. No dimensions are inferred.
#' @param value_semantics The declared value semantics. Only `"additive"` is
#'   supported in v0.1.
#' @param tolerance A finite positive numeric tolerance for mass-balance checks.
#' @return A list with elements `data` and `diagnostics`.
#' @export
aggregate_geography <- function(data, source_spatial_relations,
                                target_geography, reference_date, value_cols,
                                group_cols, value_semantics = "additive",
                                tolerance = sqrt(.Machine$double.eps)) {
  .validate_aggregate_arguments(
    data, source_spatial_relations, target_geography, reference_date,
    value_cols, group_cols, value_semantics, tolerance
  )
  if (!nrow(data)) {
    return(.empty_aggregate_result(data, reference_date, value_cols, group_cols,
                                   value_semantics))
  }
  .aggregate_source_geography(
    data, source_spatial_relations, target_geography, reference_date,
    value_cols, group_cols, value_semantics, tolerance
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
#' @param data A data frame or tibble containing complete canonical `geo_id`,
#'   `geo_name`, `geo_level`, and `geo_vintage` columns, grouping columns, and
#'   additive value columns.
#' @param relations Historical relations satisfying the `geography_relations`
#'   contract.
#' @param target_geography A canonical geography register.
#' @param source_vintage A complete scalar `Date` identifying the input vintage.
#' @param target_vintage A complete scalar `Date` identifying the output vintage.
#' @param value_cols Character vector naming numeric additive value columns.
#' @param group_cols Character vector naming every non-geographic dimension.
#' @param value_semantics Only `"additive"` is supported in v0.1.
#' @param tolerance A finite positive numeric mass-balance tolerance.
#' @return A list with elements `data` and `diagnostics`.
#' @export
harmonize_vintage <- function(data, relations, target_geography,
                              source_vintage, target_vintage, value_cols,
                              group_cols, value_semantics = "additive",
                              tolerance = sqrt(.Machine$double.eps)) {
  .validate_historical_transform_arguments(
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

.validate_historical_transform_arguments <- function(
    data, relations, target_geography, value_cols, group_cols,
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

.validate_aggregate_arguments <- function(
    data, relations, target_geography, reference_date, value_cols, group_cols,
    value_semantics, tolerance) {
  contract <- "aggregate_geography"
  .require_data_frame(data, contract)
  .require_columns(
    data, c("geo_id", "geo_name", "geo_level", "source", "source_version"),
    contract
  )
  if (anyDuplicated(names(data))) {
    .stop_contract(contract, "input column names must be unique")
  }
  for (field in c("geo_id", "geo_name", "geo_level", "source")) {
    .check_character(data[[field]], field, contract)
  }
  .check_character(data$source_version, "source_version", contract, allow_na = TRUE)
  if ("geo_vintage" %in% names(data)) {
    .check_date(data$geo_vintage, "geo_vintage", contract, allow_na = TRUE)
  }
  validate_source_spatial_relations(relations)
  validate_geography(target_geography)
  .check_scalar_date(reference_date, "reference_date", contract)
  .validate_value_and_group_arguments(
    data, value_cols, group_cols, value_semantics, tolerance, contract,
    allowed_unclassified = c(
      "geo_id", "geo_name", "geo_level", "geo_vintage",
      "source_geo_id", "source_geo_name", "source_geo_level"
    )
  )
  provenance <- c("source_geo_id", "source_geo_name", "source_geo_level")
  grouped_provenance <- intersect(group_cols, provenance)
  if (length(grouped_provenance)) {
    .stop_contract(
      contract,
      sprintf("source-geography provenance columns cannot be grouping columns: %s",
              paste(grouped_provenance, collapse = ", "))
    )
  }
  invisible(TRUE)
}

.validate_value_and_group_arguments <- function(
    data, value_cols, group_cols, value_semantics, tolerance, contract,
    allowed_unclassified) {
  .check_column_argument(value_cols, "value_cols", allow_empty = FALSE)
  .check_column_argument(group_cols, "group_cols", allow_empty = TRUE)
  overlap <- intersect(value_cols, group_cols)
  if (length(overlap)) {
    .stop_contract(
      contract,
      sprintf("columns cannot be both values and groups: %s",
              paste(overlap, collapse = ", "))
    )
  }
  geographic <- c("geo_id", "geo_name", "geo_level", "geo_vintage")
  reserved_arguments <- intersect(c(value_cols, group_cols), geographic)
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
  unclassified <- setdiff(names(data), c(allowed_unclassified, requested))
  if (length(unclassified)) {
    .stop_contract(
      contract,
      sprintf("unclassified input column(s): %s",
              paste(unclassified, collapse = ", "))
    )
  }
  for (field in value_cols) .check_numeric(data[[field]], field, contract)
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

.aggregate_source_geography <- function(
    data, relations, target_geography, reference_date, value_cols, group_cols,
    value_semantics, tolerance) {
  units <- unique(data.frame(
    source = data$source,
    source_version = data$source_version,
    from_geo_id = data$geo_id,
    from_geo_level = data$geo_level,
    stringsAsFactors = FALSE
  ))
  mappings <- lapply(seq_len(nrow(units)), function(index) {
    unit <- units[index, , drop = FALSE]
    candidate_rows <- which(
      relations$source == unit$source &
        .transform_na_equal(relations$source_version, unit$source_version) &
        relations$from_geo_id == unit$from_geo_id &
        relations$from_geo_level == unit$from_geo_level &
        relations$valid_from <= reference_date &
        (is.na(relations$valid_to) | relations$valid_to >= reference_date)
    )
    candidates <- relations[candidate_rows, , drop = FALSE]
    if (nrow(candidates) > 1L) {
      .stop_contract(
        "aggregate_geography",
        sprintf("ambiguous applicable source spatial relation for `%s`",
                unit$from_geo_id)
      )
    }
    if (nrow(candidates) == 1L) {
      return(data.frame(
        unit, to_geo_id = candidates$to_geo_id,
        mapping_method = paste0("explicit_", candidates$relation_type),
        relation_row = candidate_rows,
        stringsAsFactors = FALSE
      ))
    }
    target <- .target_rows_at_date(
      target_geography, unit$from_geo_id, reference_date,
      "aggregate_geography", "reference date"
    )
    data.frame(
      unit, to_geo_id = target$geo_id,
      mapping_method = "canonical_passthrough", relation_row = NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  mapping <- do.call(rbind, mappings)
  rownames(mapping) <- NULL
  target_ids <- unique(mapping$to_geo_id)
  targets <- .target_rows_at_date(
    target_geography, target_ids, reference_date,
    "aggregate_geography", "reference date"
  )
  data_keys <- .source_spatial_unit_keys(
    data$source, data$source_version, data$geo_id, data$geo_level
  )
  mapping_keys <- .source_spatial_unit_keys(
    mapping$source, mapping$source_version,
    mapping$from_geo_id, mapping$from_geo_level
  )
  map_index <- match(data_keys, mapping_keys)
  target_index <- match(mapping$to_geo_id[map_index], targets$geo_id)
  grouping <- data.frame(
    geo_id = targets$geo_id[target_index],
    geo_name = targets$geo_name[target_index],
    geo_level = targets$geo_level[target_index],
    stringsAsFactors = FALSE, check.names = FALSE
  )
  vintage_groups <- character()
  if ("geo_vintage" %in% names(data)) {
    grouping$geo_vintage <- data$geo_vintage
    vintage_groups <- "geo_vintage"
  }
  if (length(group_cols)) grouping <- cbind(grouping, data[group_cols])
  transformed <- .sum_by_groups(grouping, data[value_cols], value_cols)
  balance_groups <- c(vintage_groups, group_cols)
  balance <- .mass_balance(data, transformed, balance_groups, value_cols, tolerance)
  if (any(!balance$balanced)) {
    .stop_contract("aggregate_geography", "mass-balance check failed")
  }
  used_rows <- unique(mapping$relation_row[!is.na(mapping$relation_row)])
  used_relations <- relations[used_rows, , drop = FALSE]
  methods <- sort(unique(mapping$mapping_method))
  list(
    data = transformed,
    diagnostics = list(
      operation = "aggregate_geography",
      reference_date = reference_date,
      input_rows = nrow(data),
      input_units = nrow(units),
      mapped_units = nrow(mapping),
      canonical_passthrough_units = sum(
        mapping$mapping_method == "canonical_passthrough"
      ),
      explicit_relation_units = sum(
        mapping$mapping_method != "canonical_passthrough"
      ),
      output_rows = nrow(transformed),
      output_units = length(unique(transformed$geo_id)),
      relation_rows_used = nrow(used_relations),
      relations_used = used_relations,
      mapping_methods_used = methods,
      relation_types_used = sort(unique(used_relations$relation_type)),
      source_geography_provenance_omitted = intersect(
        c("source_geo_id", "source_geo_name", "source_geo_level"), names(data)
      ),
      geo_vintage_present = "geo_vintage" %in% names(data),
      geo_vintage_inferred_or_changed = FALSE,
      unmatched_units = character(), unmatched_unit_count = 0L,
      ambiguous_units = character(), ambiguous_unit_count = 0L,
      value_semantics = value_semantics,
      mass_balance = balance
    )
  )
}

.transform_na_equal <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

.source_spatial_unit_keys <- function(source, version, id, level) {
  encoded_version <- ifelse(is.na(version), "<NA>",
                            paste0(nchar(version), ":", version))
  paste(source, encoded_version, id, level, sep = "\r")
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
  .target_rows_at_date(
    geography, target_ids, vintage, operation, "target vintage"
  )
}

.target_rows_at_date <- function(
    geography, target_ids, date, operation, date_label) {
  applicable <- geography[
    geography$geo_id %in% target_ids &
      geography$valid_from <= date &
      (is.na(geography$valid_to) | geography$valid_to >= date),
    , drop = FALSE
  ]
  counts <- table(factor(applicable$geo_id, levels = target_ids))
  missing <- names(counts[counts == 0L])
  overlapping <- names(counts[counts > 1L])
  if (length(missing)) {
    .stop_contract(
      operation,
      sprintf("no target geography row is valid at the %s for: %s", date_label,
              paste(missing, collapse = ", "))
    )
  }
  if (length(overlapping)) {
    .stop_contract(
      operation,
      sprintf("multiple target geography rows are valid at the %s for: %s", date_label,
              paste(overlapping, collapse = ", "))
    )
  }
  applicable[match(target_ids, applicable$geo_id), , drop = FALSE]
}

.empty_aggregate_result <- function(
    data, reference_date, value_cols, group_cols, value_semantics) {
  columns <- c("geo_id", "geo_name", "geo_level")
  if ("geo_vintage" %in% names(data)) columns <- c(columns, "geo_vintage")
  columns <- c(columns, group_cols, value_cols)
  output <- data[0, columns, drop = FALSE]
  result <- .empty_transform_result(
    output, "aggregate_geography", as.Date(NA), as.Date(NA),
    value_cols, value_semantics
  )
  result$diagnostics$source_vintage <- NULL
  result$diagnostics$target_vintage <- NULL
  result$diagnostics$reference_date <- reference_date
  result$diagnostics$canonical_passthrough_units <- 0L
  result$diagnostics$explicit_relation_units <- 0L
  result$diagnostics$mapping_methods_used <- character()
  result$diagnostics$source_geography_provenance_omitted <- intersect(
    c("source_geo_id", "source_geo_name", "source_geo_level"), names(data)
  )
  result$diagnostics$geo_vintage_present <- "geo_vintage" %in% names(data)
  result$diagnostics$geo_vintage_inferred_or_changed <- FALSE
  result
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
  encoded <- lapply(groups, function(x) match(x, unique(x)))
  key <- do.call(paste, c(encoded, list(sep = "\r")))
  first <- !duplicated(key)
  result <- groups[first, , drop = FALSE]
  sums <- rowsum(
    as.matrix(values[value_cols]), group = key, reorder = FALSE, na.rm = FALSE
  )
  for (field in value_cols) result[[field]] <- sums[, field]
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
