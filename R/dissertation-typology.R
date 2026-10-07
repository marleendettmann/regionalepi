.dissertation_indicator_contract <- data.frame(
  indicator_id = c(
    "population_density", "mean_age", "youth_dependency_ratio"
  ),
  definition_version = rep("dissertation_v1", 3L),
  indicator_unit = c(
    "persons_per_km2", "years", "persons_under_20_per_100_persons_20_64"
  ),
  matrix_column = c(
    "Einwohnerdichte", "Durchschnittsalter", "AbhaengigenquoteJunge"
  ),
  stringsAsFactors = FALSE
)

.validate_dissertation_specification <- function(specification) {
  validate_typology_spec(specification)
  reference <- dissertation_typology_spec()
  if (!identical(specification, reference)) {
    .stop_contract(
      "dissertation typology",
      "`specification` must be the frozen dissertation_v1 specification"
    )
  }
  invisible(specification)
}

.validate_dissertation_summary <- function(indicator_summary, specification) {
  contract <- "dissertation typology input"
  if (!is.list(indicator_summary) || is.data.frame(indicator_summary) ||
      !all(c("data", "diagnostics", "provenance") %in% names(indicator_summary))) {
    .stop_contract(
      contract,
      "`indicator_summary` must contain data, diagnostics, and provenance"
    )
  }
  data <- indicator_summary$data
  required <- c(
    "geo_id", "indicator_id", "indicator_value", "indicator_unit",
    "definition_version", "period_start", "period_end",
    "aggregation_method", "annual_observation_count", "provenance_id"
  )
  .require_data_frame(data, contract)
  .require_columns(data, required, contract)
  for (field in c(
    "geo_id", "indicator_id", "indicator_unit", "definition_version",
    "aggregation_method", "provenance_id"
  )) .check_character(data[[field]], field, contract)
  .check_numeric(data$indicator_value, "indicator_value", contract)
  .check_date(data$period_start, "period_start", contract)
  .check_date(data$period_end, "period_end", contract)
  .check_numeric(
    data$annual_observation_count, "annual_observation_count", contract,
    whole = TRUE
  )
  if (any(!grepl("^[0-9]{5}$", data$geo_id))) {
    .stop_contract(contract, "`geo_id` must be a five-character AGS")
  }
  key <- paste(data$geo_id, data$indicator_id, sep = "\r")
  if (anyDuplicated(key)) {
    .stop_contract(contract, "each geo_id and indicator must occur exactly once")
  }
  expected <- .dissertation_indicator_contract
  if (!setequal(unique(data$indicator_id), expected$indicator_id)) {
    .stop_contract(contract, "the three exact dissertation indicators are required")
  }
  matched <- match(data$indicator_id, expected$indicator_id)
  if (any(data$definition_version != expected$definition_version[matched]) ||
      any(data$indicator_unit != expected$indicator_unit[matched])) {
    .stop_contract(contract, "indicator units or definition versions are incompatible")
  }
  period_start <- as.Date(sprintf("%d-12-31", min(specification$reference_years)))
  period_end <- as.Date(sprintf("%d-12-31", max(specification$reference_years)))
  if (any(data$period_start != period_start) ||
      any(data$period_end != period_end) ||
      any(data$aggregation_method != specification$temporal_aggregation) ||
      any(data$annual_observation_count != length(specification$reference_years))) {
    .stop_contract(contract, "period definition must be the complete 2017-2020 arithmetic mean")
  }
  sets <- lapply(expected$indicator_id, function(id) {
    sort(data$geo_id[data$indicator_id == id])
  })
  if (any(vapply(sets[-1L], function(x) !identical(x, sets[[1L]]), logical(1L)))) {
    .stop_contract(contract, "indicator geo_id sets must match exactly")
  }
  invisible(data)
}

.dissertation_matrix <- function(data) {
  ids <- sort(unique(data$geo_id))
  contract <- .dissertation_indicator_contract
  matrix <- vapply(contract$indicator_id, function(indicator) {
    rows <- data[data$indicator_id == indicator, , drop = FALSE]
    rows$indicator_value[match(ids, rows$geo_id)]
  }, numeric(length(ids)))
  colnames(matrix) <- contract$matrix_column
  rownames(matrix) <- ids
  matrix
}

.base_scale_explicit <- function(matrix) {
  center <- colMeans(matrix)
  scale <- apply(matrix, 2L, stats::sd)
  if (any(!is.finite(scale)) || any(scale == 0)) {
    .stop_contract(
      "dissertation typology input",
      "every indicator must have a finite, non-zero sample standard deviation"
    )
  }
  standardized <- sweep(sweep(matrix, 2L, center, "-"), 2L, scale, "/")
  list(data = standardized, center = center, scale = scale)
}

.with_preserved_random_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(code)
}

.historical_mapping_frame <- function(specification) {
  rows <- specification$method_parameters$historical_label_mapping
  do.call(rbind, lapply(rows, function(x) {
    data.frame(
      raw_cluster = x$raw_cluster, cluster_code = x$cluster_code,
      cluster_label = x$cluster_label, stringsAsFactors = FALSE
    )
  }))
}

#' Fit the frozen historical reference typology
#'
#' Constructs the three-indicator 2017--2020 matrix used for the historical
#' reference typology, explicitly reproduces base R sample-standard-deviation
#' scaling, and fits the frozen three-cluster k-means. Historical labels are
#' applied only when explicitly requested for reproduction of the historical
#' fitted solution.
#'
#' @param indicator_summary Result returned by [summarize_indicator_period()].
#' @param specification The exact frozen [dissertation_typology_spec()].
#' @param label_mapping Either `"none"` or `"historical_reference"`.
#' @return An ordinary list with `data`, matrix and scaling diagnostics,
#'   k-means diagnostics, and specification provenance.
#' @export
fit_dissertation_typology <- function(
    indicator_summary, specification = dissertation_typology_spec(),
    label_mapping = c("none", "historical_reference")) {
  label_mapping <- match.arg(label_mapping)
  .validate_dissertation_specification(specification)
  data <- .validate_dissertation_summary(indicator_summary, specification)
  matrix <- .dissertation_matrix(data)
  scaled <- .base_scale_explicit(matrix)
  parameters <- specification$method_parameters
  fit <- .with_preserved_random_seed(parameters$seed, stats::kmeans(
    scaled$data, centers = parameters$centers, nstart = parameters$nstart,
    algorithm = parameters$algorithm, iter.max = parameters$iter.max
  ))
  assignments <- data.frame(
    geo_id = rownames(matrix), raw_cluster = as.integer(fit$cluster),
    cluster_code = NA_character_, cluster_label = NA_character_,
    stringsAsFactors = FALSE
  )
  applied_mapping <- NULL
  if (identical(label_mapping, "historical_reference")) {
    applied_mapping <- .historical_mapping_frame(specification)
    position <- match(assignments$raw_cluster, applied_mapping$raw_cluster)
    assignments$cluster_code <- applied_mapping$cluster_code[position]
    assignments$cluster_label <- applied_mapping$cluster_label[position]
  }
  list(
    data = assignments,
    matrix = list(
      data = matrix, standardized = scaled$data,
      indicator_order = .dissertation_indicator_contract$indicator_id,
      matrix_columns = colnames(matrix), row_order = rownames(matrix),
      center = scaled$center, scale = scaled$scale,
      scale_semantics = parameters$scale_semantics
    ),
    diagnostics = list(
      label_mapping = label_mapping, applied_mapping = applied_mapping,
      centers = fit$centers, cluster_sizes = fit$size,
      withinss = fit$withinss, tot.withinss = fit$tot.withinss,
      betweenss = fit$betweenss, totss = fit$totss,
      iter = fit$iter, ifault = fit$ifault
    ),
    provenance = list(
      typology_id = specification$typology_id,
      definition_version = specification$definition_version,
      reference_years = specification$reference_years,
      input_provenance_ids = sort(unique(data$provenance_id)),
      package_version = as.character(utils::packageVersion("regionalepi"))
    ),
    specification = specification
  )
}

.permutations_three <- function(values) {
  rbind(
    values[c(1L, 2L, 3L)], values[c(1L, 3L, 2L)],
    values[c(2L, 1L, 3L)], values[c(2L, 3L, 1L)],
    values[c(3L, 1L, 2L)], values[c(3L, 2L, 1L)]
  )
}

.adjusted_rand_index <- function(x, y) {
  table <- table(x, y)
  choose_two <- function(value) value * (value - 1) / 2
  total <- sum(table)
  if (total < 2L) return(NA_real_)
  index <- sum(choose_two(table))
  row_pairs <- sum(choose_two(rowSums(table)))
  column_pairs <- sum(choose_two(colSums(table)))
  expected <- row_pairs * column_pairs / choose_two(total)
  maximum <- (row_pairs + column_pairs) / 2
  if (maximum == expected) return(if (identical(x, y)) 1 else 0)
  (index - expected) / (maximum - expected)
}

.reference_assignments <- function(reference) {
  if (is.list(reference) && !is.data.frame(reference) &&
      "data" %in% names(reference)) reference <- reference$data
  .require_data_frame(reference, "typology reference")
  .require_columns(reference, c("geo_id", "cluster_code"), "typology reference")
  .check_character(reference$geo_id, "geo_id", "typology reference")
  .check_character(reference$cluster_code, "cluster_code", "typology reference")
  if (anyDuplicated(reference$geo_id) ||
      !setequal(unique(reference$cluster_code), c("ClD", "ClJ", "ClA"))) {
    .stop_contract(
      "typology reference",
      "geo_ids must be unique and all three historical cluster codes are required"
    )
  }
  reference[, c("geo_id", "cluster_code"), drop = FALSE]
}

#' Compare a fitted typology with the historical reference partition
#'
#' Exhaustively evaluates the six possible mappings from candidate raw cluster
#' numbers to the three historical codes and selects the mapping with greatest
#' agreement on shared geographic identifiers. Candidate raw assignments are
#' never changed.
#'
#' @param fitted Result returned by [fit_dissertation_typology()].
#' @param reference A data frame, or fit-like list, containing unique `geo_id`
#'   and historical `cluster_code` values.
#' @return An ordinary named diagnostics list.
#' @export
compare_typology <- function(fitted, reference) {
  contract <- "typology comparison"
  if (!is.list(fitted) || !"data" %in% names(fitted) ||
      !is.data.frame(fitted$data)) {
    .stop_contract(contract, "`fitted` must contain assignment data")
  }
  candidate <- fitted$data
  .require_columns(candidate, c("geo_id", "raw_cluster"), contract)
  .check_character(candidate$geo_id, "geo_id", contract)
  .check_numeric(candidate$raw_cluster, "raw_cluster", contract, whole = TRUE)
  if (anyDuplicated(candidate$geo_id) ||
      !setequal(unique(candidate$raw_cluster), 1:3)) {
    .stop_contract(contract, "candidate must have unique IDs and raw clusters 1, 2, 3")
  }
  reference <- .reference_assignments(reference)
  ids_in_both <- sort(intersect(candidate$geo_id, reference$geo_id))
  if (!length(ids_in_both)) {
    .stop_contract(contract, "candidate and reference have no IDs in common")
  }
  candidate_common <- candidate[match(ids_in_both, candidate$geo_id), ]
  reference_common <- reference[match(ids_in_both, reference$geo_id), ]
  permutations <- .permutations_three(c("ClD", "ClJ", "ClA"))
  agreements <- apply(permutations, 1L, function(mapping) {
    sum(mapping[candidate_common$raw_cluster] == reference_common$cluster_code)
  })
  best <- which.max(agreements)
  best_codes <- permutations[best, ]
  labels <- c(
    ClD = "dichte Regionen", ClJ = "familiengepr\u00e4gte Regionen",
    ClA = "\u00e4ltere, l\u00e4ndliche Regionen"
  )
  mapping <- data.frame(
    raw_cluster = 1:3, cluster_code = best_codes,
    cluster_label = unname(labels[best_codes]), stringsAsFactors = FALSE
  )
  mapped <- best_codes[candidate_common$raw_cluster]
  changed <- ids_in_both[mapped != reference_common$cluster_code]
  levels <- c("ClD", "ClJ", "ClA")
  confusion <- table(
    candidate = factor(mapped, levels = levels),
    reference = factor(reference_common$cluster_code, levels = levels)
  )
  all_mapped <- best_codes[candidate$raw_cluster]
  list(
    ids_in_both = ids_in_both,
    ids_only_in_candidate = sort(setdiff(candidate$geo_id, reference$geo_id)),
    ids_only_in_reference = sort(setdiff(reference$geo_id, candidate$geo_id)),
    mapping = mapping, exact_agreement = sum(mapped == reference_common$cluster_code),
    disagreement = length(changed),
    agreement_proportion = mean(mapped == reference_common$cluster_code),
    confusion_matrix = confusion, changed_geo_ids = changed,
    candidate_cluster_sizes_raw = table(candidate$raw_cluster),
    candidate_cluster_sizes_mapped = table(factor(all_mapped, levels = levels)),
    reference_cluster_sizes = table(factor(reference$cluster_code, levels = levels)),
    adjusted_rand_index = .adjusted_rand_index(
      candidate_common$raw_cluster, reference_common$cluster_code
    )
  )
}
