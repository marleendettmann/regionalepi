.dynamic_profile_alignment <- function(fit,
    alignment = "demographic_profile_v1") {
  contract <- "dynamic typology transition alignment"
  .validate_dynamic_fit_result(fit)
  if (!identical(alignment, "demographic_profile_v1")) {
    .stop_contract(contract, "alignment must be demographic_profile_v1")
  }
  if (!identical(fit$diagnostics$k, 3L)) {
    .stop_contract(contract, "profile alignment requires a k = 3 dynamic fit")
  }
  anchors <- c(
    older = "mean_age",
    family = "youth_dependency_ratio",
    dense = "population_density"
  )
  profiles <- fit$profiles
  mapping <- lapply(names(anchors), function(profile_class) {
    rows <- profiles[profiles$indicator_id == anchors[[profile_class]],
      c("display_cluster_id", "standardized_center"), drop = FALSE]
    if (nrow(rows) != 3L || anyDuplicated(rows$display_cluster_id) ||
        anyNA(rows) || any(!is.finite(rows$standardized_center))) {
      .stop_contract(contract, "each profile anchor must cover three unique clusters")
    }
    maximum <- max(rows$standardized_center)
    winner <- rows$display_cluster_id[rows$standardized_center == maximum]
    if (length(winner) != 1L) {
      .stop_contract(contract, "profile anchors must have unique maxima")
    }
    data.frame(
      dynamic_cluster_id = winner,
      profile_class = profile_class,
      anchor_indicator_id = anchors[[profile_class]],
      anchor_standardized_center = maximum,
      stringsAsFactors = FALSE
    )
  })
  mapping <- do.call(rbind, mapping)
  if (anyDuplicated(mapping$dynamic_cluster_id) ||
      !setequal(mapping$dynamic_cluster_id,
        unique(fit$assignments$display_cluster_id))) {
    .stop_contract(contract,
      "profile anchors must identify three distinct complete clusters")
  }
  rownames(mapping) <- NULL
  mapping
}

.validate_transition_geo_ids <- function(geo_ids, from_fit, to_fit) {
  contract <- "dynamic typology transition comparison"
  .check_character(geo_ids, "geo_ids", contract)
  if (!length(geo_ids) || anyNA(geo_ids) || any(!grepl("^[0-9]{5}$", geo_ids)) ||
      anyDuplicated(geo_ids)) {
    .stop_contract(contract,
      "geo_ids must be unique complete five-character identifiers")
  }
  for (entry in list(from = from_fit$assignments, to = to_fit$assignments)) {
    if (anyDuplicated(entry$geo_id)) {
      .stop_contract(contract, "fit assignments must contain unique geo_id values")
    }
    missing <- setdiff(geo_ids, entry$geo_id)
    if (length(missing)) {
      .stop_contract(contract,
        "every requested comparison geo_id must occur in both fits")
    }
  }
  sort(geo_ids, method = "radix")
}

#' Compare dynamic demographic typologies across reference periods
#'
#' Aligns two independently fitted dynamic three-cluster typologies using the
#' reviewed demographic profile anchors: highest mean age (`older`), highest
#' youth dependency ratio (`family`), and highest population density (`dense`).
#' Raw or neutral k-means cluster identifiers are never treated as stable
#' identities across fits.
#'
#' @param from_fit,to_fit Valid results from [fit_dynamic_typology()] with
#'   `k = 3`.
#' @param geo_ids Unique five-character district identifiers defining the
#'   comparison geography. Fit assignments outside this set remain untouched
#'   and are excluded from the comparison.
#' @param alignment Alignment definition. Version 1 supports only
#'   `"demographic_profile_v1"`.
#' @return A list containing district-level assignments, long-form indicator
#'   changes, count and row-percentage matrices, profile mappings, diagnostics,
#'   and provenance.
#' @export
compare_dynamic_typology_transitions <- function(
    from_fit, to_fit, geo_ids,
    alignment = "demographic_profile_v1") {
  contract <- "dynamic typology transition comparison"
  .validate_dynamic_fit_result(from_fit)
  .validate_dynamic_fit_result(to_fit)
  if (identical(from_fit$provenance$fit_id, to_fit$provenance$fit_id)) {
    .stop_contract(contract, "from_fit and to_fit must be independent fits")
  }
  ids <- .validate_transition_geo_ids(geo_ids, from_fit, to_fit)
  from_mapping <- .dynamic_profile_alignment(from_fit, alignment)
  to_mapping <- .dynamic_profile_alignment(to_fit, alignment)
  classes <- c("older", "family", "dense")
  from <- from_fit$assignments[match(ids, from_fit$assignments$geo_id), ]
  to <- to_fit$assignments[match(ids, to_fit$assignments$geo_id), ]
  from_class <- from_mapping$profile_class[
    match(from$display_cluster_id, from_mapping$dynamic_cluster_id)]
  to_class <- to_mapping$profile_class[
    match(to$display_cluster_id, to_mapping$dynamic_cluster_id)]
  if (anyNA(from_class) || anyNA(to_class)) {
    .stop_contract(contract, "profile mappings must cover every assignment")
  }
  assignments <- data.frame(
    geo_id = ids,
    from_period_start = as.Date(from_fit$provenance$period_start),
    from_period_end = as.Date(from_fit$provenance$period_end),
    to_period_start = as.Date(to_fit$provenance$period_start),
    to_period_end = as.Date(to_fit$provenance$period_end),
    from_fit_id = from_fit$provenance$fit_id,
    to_fit_id = to_fit$provenance$fit_id,
    from_dynamic_cluster_id = from$display_cluster_id,
    to_dynamic_cluster_id = to$display_cluster_id,
    from_profile_class = from_class,
    to_profile_class = to_class,
    changed = from_class != to_class,
    alignment_version = alignment,
    stringsAsFactors = FALSE
  )
  count_matrix <- table(
    from = factor(assignments$from_profile_class, levels = classes),
    to = factor(assignments$to_profile_class, levels = classes)
  )
  row_percentage_matrix <- 100 * prop.table(count_matrix, margin = 1L)

  indicator_ids <- from_fit$matrix$indicator_order
  if (!identical(indicator_ids, to_fit$matrix$indicator_order) ||
      any(!ids %in% rownames(from_fit$matrix$original)) ||
      any(!ids %in% rownames(to_fit$matrix$original))) {
    .stop_contract(contract,
      "fit indicator matrices must cover one compatible indicator definition")
  }
  indicator_changes <- do.call(rbind, lapply(indicator_ids, function(indicator) {
    from_value <- from_fit$matrix$original[ids, indicator]
    to_value <- to_fit$matrix$original[ids, indicator]
    data.frame(
      geo_id = ids, indicator_id = indicator,
      from_value = as.numeric(from_value), to_value = as.numeric(to_value),
      absolute_change = as.numeric(to_value - from_value),
      stringsAsFactors = FALSE
    )
  }))
  rownames(indicator_changes) <- NULL
  comparison_id <- paste0("dynamic_typology_transition_",
    .stable_text_hash(c(from_fit$provenance$fit_id,
      to_fit$provenance$fit_id, ids, alignment)))
  result <- list(
    assignments = assignments,
    indicator_changes = indicator_changes,
    count_matrix = count_matrix,
    row_percentage_matrix = row_percentage_matrix,
    profile_alignment = list(from = from_mapping, to = to_mapping),
    diagnostics = list(
      compared_districts = length(ids),
      unchanged_districts = sum(!assignments$changed),
      changed_districts = sum(assignments$changed),
      changed_percentage = 100 * mean(assignments$changed),
      from_fit_only_geo_ids = sort(setdiff(
        from_fit$assignments$geo_id, ids), method = "radix"),
      to_fit_only_geo_ids = sort(setdiff(
        to_fit$assignments$geo_id, ids), method = "radix")
    ),
    provenance = list(
      comparison_id = comparison_id,
      alignment_version = alignment,
      from_fit_id = from_fit$provenance$fit_id,
      to_fit_id = to_fit$provenance$fit_id,
      from_reference_period = c(from_fit$provenance$period_start,
        from_fit$provenance$period_end),
      to_reference_period = c(to_fit$provenance$period_start,
        to_fit$provenance$period_end),
      indicator_set_id = from_fit$provenance$indicator_set_id,
      indicator_set_version = from_fit$provenance$indicator_set_version,
      fitting_specification_id = from_fit$provenance$fitting_specification_id,
      package_version = as.character(utils::packageVersion("regionalepi"))
    )
  )
  result
}
