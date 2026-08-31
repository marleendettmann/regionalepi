.indicator_set_fields <- c(
  "indicator_set_id", "definition_version", "indicators",
  "standardization", "completeness", "provenance"
)

#' Validate a reviewed demographic indicator-set specification
#'
#' Indicator sets define ordered, versioned inputs and preprocessing but do
#' not freeze a demographic reference period or clustering result.
#'
#' @param x A named indicator-set list.
#' @return `x`, invisibly.
#' @export
validate_indicator_set_spec <- function(x) {
  contract <- "indicator_set_spec"
  .require_named_list(x, .indicator_set_fields, contract)
  if (!identical(names(x), .indicator_set_fields)) {
    .stop_contract(contract, "fields are missing, unexpected, or reordered")
  }
  for (field in c("indicator_set_id", "definition_version")) {
    .check_scalar_nonempty_character(x[[field]], field, contract)
  }
  if (!is.list(x$indicators) || !length(x$indicators)) {
    .stop_contract(contract, "`indicators` must be a non-empty ordered list")
  }
  references <- lapply(seq_along(x$indicators), function(i) {
    ref <- x$indicators[[i]]
    fields <- c("indicator_id", "definition_version", "indicator_unit")
    .require_named_list(ref, fields, paste(contract, "indicator", i))
    if (!identical(names(ref), fields)) {
      .stop_contract(contract, "indicator references have unexpected or reordered fields")
    }
    for (field in fields) .check_scalar_nonempty_character(ref[[field]], field, contract)
    unlist(ref, use.names = TRUE)
  })
  ids <- vapply(x$indicators, `[[`, character(1L), "indicator_id")
  if (anyDuplicated(ids)) {
    .stop_contract(contract, "indicator IDs must be unique and order is significant")
  }
  authoritative <- lapply(list(
    dissertation_population_density_spec(),
    dissertation_mean_age_spec(),
    dissertation_youth_dependency_ratio_spec()
  ), function(spec) c(
    indicator_id = spec$indicator_id,
    definition_version = spec$definition_version,
    indicator_unit = spec$parameters$unit
  ))
  for (ref in references) {
    if (!any(vapply(authoritative, identical, logical(1L), ref))) {
      .stop_contract(contract, "indicator reference is unknown or not authoritative")
    }
  }
  standardization_fields <- c("method", "center", "scale", "scale_semantics")
  .require_named_list(x$standardization, standardization_fields,
                      paste(contract, "standardization"))
  if (!identical(names(x$standardization), standardization_fields) ||
      !identical(x$standardization$method, "z_score") ||
      !identical(x$standardization$center, TRUE) ||
      !identical(x$standardization$scale, TRUE) ||
      !identical(x$standardization$scale_semantics,
                 "base_r_sample_sd_n_minus_1")) {
    .stop_contract(contract, "unsupported or incomplete standardization settings")
  }
  completeness_fields <- c(
    "all_indicators_required", "common_geo_id_set_required",
    "minimum_fraction"
  )
  .require_named_list(x$completeness, completeness_fields,
                      paste(contract, "completeness"))
  if (!identical(names(x$completeness), completeness_fields) ||
      !identical(x$completeness$all_indicators_required, TRUE) ||
      !identical(x$completeness$common_geo_id_set_required, TRUE) ||
      !identical(x$completeness$minimum_fraction, 1)) {
    .stop_contract(contract, "v1 requires complete indicators on one common geo_id set")
  }
  .require_named_list(x$provenance, c("review_status", "description"),
                      paste(contract, "provenance"))
  for (field in c("review_status", "description")) {
    .check_scalar_nonempty_character(x$provenance[[field]], field, contract)
  }
  if (!identical(x$provenance$review_status, "reviewed")) {
    .stop_contract(contract, "indicator set must be reviewed")
  }
  invisible(references)
  invisible(x)
}

#' Reviewed demographic-structure indicator set
#'
#' Defines the ordered population-density, mean-age, and youth-dependency
#' inputs used for dynamic demographic clustering. It reuses their authoritative
#' `dissertation_v1` indicator definitions but is separate from the frozen
#' dissertation clustering result and contains no reference period.
#'
#' @return A validated `demographic_structure_v1` indicator-set specification.
#' @export
demographic_structure_spec <- function() {
  specs <- list(
    dissertation_population_density_spec(),
    dissertation_mean_age_spec(),
    dissertation_youth_dependency_ratio_spec()
  )
  x <- list(
    indicator_set_id = "demographic_structure_v1",
    definition_version = "v1",
    indicators = lapply(specs, function(spec) list(
      indicator_id = spec$indicator_id,
      definition_version = spec$definition_version,
      indicator_unit = spec$parameters$unit
    )),
    standardization = list(
      method = "z_score", center = TRUE, scale = TRUE,
      scale_semantics = "base_r_sample_sd_n_minus_1"
    ),
    completeness = list(
      all_indicators_required = TRUE,
      common_geo_id_set_required = TRUE,
      minimum_fraction = 1
    ),
    provenance = list(
      review_status = "reviewed",
      description = paste(
        "Reviewed dynamic demographic structure inputs; indicator definitions",
        "reuse dissertation_v1 but period and fitted clusters are not frozen."
      )
    )
  )
  validate_indicator_set_spec(x)
  x
}

.dynamic_fitting_fields <- c(
  "fitting_specification_id", "definition_version", "method", "algorithm",
  "nstart", "iter_max", "seed", "row_order", "supported_k"
)

#' Validate a dynamic typology fitting specification
#'
#' @param x A named dynamic fitting specification.
#' @return `x`, invisibly.
#' @export
validate_dynamic_fitting_spec <- function(x) {
  contract <- "dynamic_fitting_spec"
  .require_named_list(x, .dynamic_fitting_fields, contract)
  if (!identical(names(x), .dynamic_fitting_fields)) {
    .stop_contract(contract, "fields are missing, unexpected, or reordered")
  }
  for (field in c(
    "fitting_specification_id", "definition_version", "method", "algorithm",
    "row_order"
  )) .check_scalar_nonempty_character(x[[field]], field, contract)
  if (!identical(x$method, "k_means") || !identical(x$algorithm, "Lloyd") ||
      !identical(x$row_order, "ascending_character_geo_id")) {
    .stop_contract(contract, "unsupported dynamic fitting method or ordering")
  }
  for (field in c("nstart", "iter_max", "seed")) {
    .check_whole_number(x[[field]], field, contract, non_negative = TRUE)
    if (length(x[[field]]) != 1L) {
      .stop_contract(contract, paste0("`", field, "` must have length one"))
    }
  }
  if (x$nstart < 1 || x$iter_max < 1) {
    .stop_contract(contract, "nstart and iter_max must be positive")
  }
  if (!is.integer(x$supported_k) || !identical(x$supported_k, 2:5)) {
    .stop_contract(contract, "v1 supports exactly integer k values 2, 3, 4, and 5")
  }
  invisible(x)
}

#' Reviewed dynamic k-means fitting specification
#'
#' Uses an explicit seed derived from the reviewed map/reference date
#' (`20241231`), Lloyd k-means, 50 starts, 100 iterations, and deterministic
#' character `geo_id` ordering. These parameters are independent of the frozen
#' dissertation fit.
#'
#' @return A validated `dynamic_kmeans_v1` specification.
#' @export
dynamic_kmeans_spec <- function() {
  x <- list(
    fitting_specification_id = "dynamic_kmeans_v1",
    definition_version = "v1",
    method = "k_means", algorithm = "Lloyd", nstart = 50L,
    iter_max = 100L, seed = 20241231L,
    row_order = "ascending_character_geo_id", supported_k = 2:5
  )
  validate_dynamic_fitting_spec(x)
  x
}
