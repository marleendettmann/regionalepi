.validate_dynamic_indicator_summary <- function(summary, indicator_set) {
  contract <- "dynamic typology input"
  if (!is.list(summary) || is.data.frame(summary) ||
      !all(c("data", "diagnostics", "provenance") %in% names(summary))) {
    .stop_contract(contract, "indicator_summary must contain data, diagnostics, and provenance")
  }
  data <- summary$data
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
  .check_numeric(data$annual_observation_count, "annual_observation_count",
                 contract, whole = TRUE)
  if (anyNA(data[, required]) || any(!is.finite(data$indicator_value)) ||
      any(!nzchar(data$geo_id))) {
    .stop_contract(contract, "summary fields and finite indicator values must be complete")
  }
  key <- paste(data$geo_id, data$indicator_id, sep = "\r")
  if (anyDuplicated(key)) {
    .stop_contract(contract, "each geo_id and indicator must occur exactly once")
  }
  refs <- indicator_set$indicators
  ids <- vapply(refs, `[[`, character(1L), "indicator_id")
  if (!setequal(unique(data$indicator_id), ids)) {
    .stop_contract(contract, "summary must contain exactly the selected indicator set")
  }
  for (ref in refs) {
    rows <- data$indicator_id == ref$indicator_id
    if (any(data$definition_version[rows] != ref$definition_version) ||
        any(data$indicator_unit[rows] != ref$indicator_unit)) {
      .stop_contract(contract, "indicator definition versions or units are incompatible")
    }
  }
  sets <- lapply(ids, function(id) sort(data$geo_id[data$indicator_id == id]))
  if (any(vapply(sets[-1L], function(x) !identical(x, sets[[1L]]), logical(1L)))) {
    .stop_contract(contract, "all indicators must share one complete geo_id set")
  }
  for (field in c("period_start", "period_end", "aggregation_method",
                  "annual_observation_count")) {
    if (length(unique(data[[field]])) != 1L) {
      .stop_contract(contract, "all indicators must share one compatible reference period")
    }
  }
  if (unique(data$period_start) > unique(data$period_end)) {
    .stop_contract(contract, "reference period is invalid")
  }
  data
}

.dynamic_indicator_matrix <- function(data, indicator_set) {
  ids <- sort(unique(data$geo_id), method = "radix")
  indicator_ids <- vapply(
    indicator_set$indicators, `[[`, character(1L), "indicator_id"
  )
  matrix <- vapply(indicator_ids, function(indicator) {
    rows <- data[data$indicator_id == indicator, , drop = FALSE]
    rows$indicator_value[match(ids, rows$geo_id)]
  }, numeric(length(ids)))
  colnames(matrix) <- indicator_ids
  rownames(matrix) <- ids
  matrix
}

.stable_text_hash <- function(parts) {
  bytes <- utf8ToInt(paste(parts, collapse = "\u001f"))
  value <- 0
  for (byte in bytes) value <- (value * 131 + byte) %% 2147483647
  sprintf("%08x", as.integer(value))
}

.dynamic_fit_id <- function(matrix, data, indicator_set, fitting, k) {
  parts <- c(
    indicator_set$indicator_set_id, indicator_set$definition_version,
    vapply(indicator_set$indicators, function(ref) paste(
      ref$indicator_id, ref$definition_version, ref$indicator_unit, sep = "@"
    ), character(1L)),
    format(unique(data$period_start)), format(unique(data$period_end)),
    unique(data$aggregation_method), unique(data$annual_observation_count),
    sort(unique(data$provenance_id)),
    fitting$fitting_specification_id, fitting$definition_version,
    fitting$algorithm, fitting$nstart, fitting$iter_max, fitting$seed, k,
    rownames(matrix), sprintf("%.17g", as.vector(t(matrix)))
  )
  paste0("dynamic_typology_", .stable_text_hash(parts))
}

.display_cluster_mapping <- function(centers) {
  raw <- seq_len(nrow(centers))
  ordering <- do.call(order, c(
    lapply(seq_len(ncol(centers)), function(column) centers[, column]),
    list(raw, method = "radix")
  ))
  data.frame(
    raw_cluster = raw,
    display_cluster_id = sprintf("C%02d", match(raw, ordering)),
    stringsAsFactors = FALSE
  )
}

.dynamic_profiles <- function(matrix, standardized, assignments, fit, mapping,
                              indicator_set, fit_id) {
  n <- nrow(matrix)
  k <- nrow(fit$centers)
  cluster_ranks <- apply(fit$centers, 2L, rank, ties.method = "min")
  if (is.null(dim(cluster_ranks))) {
    cluster_ranks <- matrix(cluster_ranks, nrow = k)
  }
  rows <- vector("list", k * ncol(matrix))
  at <- 0L
  for (raw in seq_len(k)) {
    members <- assignments$raw_cluster == raw
    center <- fit$centers[raw, ]
    strongest <- rank(-abs(center), ties.method = "min")
    for (column in seq_len(ncol(matrix))) {
      at <- at + 1L
      ref <- indicator_set$indicators[[column]]
      rows[[at]] <- data.frame(
        fit_id = fit_id,
        display_cluster_id = mapping$display_cluster_id[raw],
        raw_cluster = as.integer(raw), cluster_size = as.integer(sum(members)),
        cluster_proportion = sum(members) / n,
        indicator_id = ref$indicator_id,
        definition_version = ref$definition_version,
        indicator_unit = ref$indicator_unit,
        original_mean = mean(matrix[members, column]),
        original_median = stats::median(matrix[members, column]),
        standardized_center = center[[column]],
        indicator_rank = as.integer(cluster_ranks[raw, column]),
        absolute_standardized_center = abs(center[[column]]),
        strongest_indicator_rank = as.integer(strongest[[column]]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

.validate_dynamic_fit_result <- function(result) {
  contract <- "dynamic typology result"
  required <- c(
    "assignments", "profiles", "cluster_diagnostics", "matrix",
    "diagnostics", "provenance", "indicator_set", "fitting_specification"
  )
  .require_named_list(result, required, contract)
  assignments <- result$assignments
  .require_data_frame(assignments, contract)
  .require_columns(assignments, c(
    "fit_id", "geo_id", "raw_cluster", "display_cluster_id"
  ), contract)
  if (anyDuplicated(assignments$geo_id) || anyNA(assignments) ||
      any(grepl("ClD|ClJ|ClA", assignments$display_cluster_id))) {
    .stop_contract(contract, "assignments must be unique, complete, and neutrally labelled")
  }
  profiles <- result$profiles
  .require_data_frame(profiles, contract)
  .require_columns(profiles, c(
    "fit_id", "display_cluster_id", "raw_cluster", "cluster_size",
    "cluster_proportion", "indicator_id", "definition_version",
    "indicator_unit", "original_mean", "original_median",
    "standardized_center", "indicator_rank",
    "absolute_standardized_center", "strongest_indicator_rank"
  ), contract)
  if (anyNA(profiles) || any(!is.finite(profiles$original_mean)) ||
      any(!is.finite(profiles$original_median)) ||
      any(!is.finite(profiles$standardized_center))) {
    .stop_contract(contract, "profile values must be complete and finite")
  }
  invisible(result)
}

#' Fit a reviewed dynamic demographic typology
#'
#' Fits deterministic k-means to a complete reviewed demographic indicator
#' set. Algorithmic cluster numbers are preserved; neutral `C01`, `C02`, ...
#' identifiers are deterministic only within a fit and carry no historical
#' reference or natural-language meaning.
#'
#' @param indicator_summary A complete result from [summarize_indicator_period()].
#' @param indicator_set A validated reviewed indicator-set specification.
#' @param fitting_specification A validated dynamic fitting specification.
#' @param k One integer among 2, 3, 4, or 5 and smaller than the unit count.
#' @return A list containing assignments, profiles, cluster and fit diagnostics,
#'   the original and standardized matrix, provenance, and specifications.
#' @export
fit_dynamic_typology <- function(
    indicator_summary, indicator_set = demographic_structure_spec(),
    fitting_specification = dynamic_kmeans_spec(), k = 3L) {
  contract <- "dynamic typology"
  validate_indicator_set_spec(indicator_set)
  validate_dynamic_fitting_spec(fitting_specification)
  .check_whole_number(k, "k", contract, non_negative = TRUE)
  if (length(k) != 1L || !k %in% fitting_specification$supported_k) {
    .stop_contract(contract, "k must be one of 2, 3, 4, or 5")
  }
  k <- as.integer(k)
  data <- .validate_dynamic_indicator_summary(indicator_summary, indicator_set)
  matrix <- .dynamic_indicator_matrix(data, indicator_set)
  if (k >= nrow(matrix)) {
    .stop_contract(contract, "k must be smaller than the number of eligible units")
  }
  scaled <- .base_scale_explicit(matrix)
  fit <- .with_preserved_random_seed(fitting_specification$seed, stats::kmeans(
    scaled$data, centers = k, nstart = fitting_specification$nstart,
    algorithm = fitting_specification$algorithm,
    iter.max = fitting_specification$iter_max
  ))
  if (length(fit$cluster) != nrow(matrix) ||
      !identical(dim(fit$centers), c(k, ncol(matrix))) ||
      length(fit$size) != k || any(fit$size <= 0) ||
      (!is.null(fit$ifault) && fit$ifault != 0L)) {
    .stop_contract(contract, "k-means returned a malformed or non-converged result")
  }
  mapping <- .display_cluster_mapping(fit$centers)
  fit_id <- .dynamic_fit_id(matrix, data, indicator_set, fitting_specification, k)
  assignments <- data.frame(
    fit_id = fit_id, geo_id = rownames(matrix),
    raw_cluster = as.integer(fit$cluster),
    display_cluster_id = mapping$display_cluster_id[fit$cluster],
    stringsAsFactors = FALSE
  )
  minimum_size <- as.integer(max(5, ceiling(0.02 * nrow(matrix))))
  cluster_diagnostics <- data.frame(
    fit_id = fit_id,
    display_cluster_id = mapping$display_cluster_id,
    raw_cluster = mapping$raw_cluster,
    size = as.integer(fit$size), proportion = as.numeric(fit$size / nrow(matrix)),
    minimum_size_threshold = minimum_size,
    minimum_size_warning = fit$size < minimum_size,
    withinss = as.numeric(fit$withinss), stringsAsFactors = FALSE
  )
  profiles <- .dynamic_profiles(
    matrix, scaled$data, assignments, fit, mapping, indicator_set, fit_id
  )
  result <- list(
    assignments = assignments,
    profiles = profiles,
    cluster_diagnostics = cluster_diagnostics,
    matrix = list(
      original = matrix, standardized = scaled$data,
      indicator_order = colnames(matrix), row_order = rownames(matrix),
      center = scaled$center, scale = scaled$scale,
      scale_semantics = indicator_set$standardization$scale_semantics
    ),
    diagnostics = list(
      k = k, totss = fit$totss, withinss = fit$withinss,
      tot.withinss = fit$tot.withinss, betweenss = fit$betweenss,
      explained_between_proportion = fit$betweenss / fit$totss,
      iterations = fit$iter, ifault = fit$ifault,
      seed = fitting_specification$seed,
      fitting_specification_id = fitting_specification$fitting_specification_id,
      minimum_size_threshold = minimum_size,
      minimum_size_warning_count = sum(cluster_diagnostics$minimum_size_warning),
      display_mapping = mapping
    ),
    provenance = list(
      fit_id = fit_id,
      indicator_set_id = indicator_set$indicator_set_id,
      indicator_set_version = indicator_set$definition_version,
      fitting_specification_id = fitting_specification$fitting_specification_id,
      fitting_specification_version = fitting_specification$definition_version,
      period_start = unique(data$period_start), period_end = unique(data$period_end),
      aggregation_method = unique(data$aggregation_method),
      input_provenance_ids = sort(unique(data$provenance_id)),
      package_version = as.character(utils::packageVersion("regionalepi"))
    ),
    indicator_set = indicator_set,
    fitting_specification = fitting_specification
  )
  .validate_dynamic_fit_result(result)
  result
}

#' Compare two dynamic partitions without semantic label mapping
#'
#' Computes a contingency table and adjusted Rand index for two partitions of
#' exactly the same units. Different values of `k` are allowed; no one-to-one
#' mapping or historical reference label is applied.
#'
#' @param x,y Results from [fit_dynamic_typology()].
#' @return Label-neutral partition diagnostics.
#' @export
compare_dynamic_partitions <- function(x, y) {
  contract <- "dynamic partition comparison"
  for (fit in list(x, y)) .validate_dynamic_fit_result(fit)
  x_data <- x$assignments
  y_data <- y$assignments
  if (!setequal(x_data$geo_id, y_data$geo_id)) {
    .stop_contract(contract, "partitions must contain exactly the same geo_id set")
  }
  ids <- sort(x_data$geo_id, method = "radix")
  x_cluster <- x_data$raw_cluster[match(ids, x_data$geo_id)]
  y_cluster <- y_data$raw_cluster[match(ids, y_data$geo_id)]
  list(
    x_fit_id = unique(x_data$fit_id), y_fit_id = unique(y_data$fit_id),
    common_geo_ids = ids,
    x_k = length(unique(x_cluster)), y_k = length(unique(y_cluster)),
    contingency_table = table(x = x_cluster, y = y_cluster),
    adjusted_rand_index = .adjusted_rand_index(x_cluster, y_cluster),
    x_cluster_sizes = table(x_cluster), y_cluster_sizes = table(y_cluster),
    semantic_mapping_performed = FALSE,
    historical_labels_applied = FALSE
  )
}
