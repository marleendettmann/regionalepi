# Internal while the generic specification schema evolves.
new_indicator_spec <- function(indicator_id, indicator_type,
                               definition_version, parameters) {
  x <- list(
    indicator_id = indicator_id,
    indicator_type = indicator_type,
    definition_version = definition_version,
    parameters = parameters
  )
  validate_indicator_spec(x)
  x
}

#' Dissertation youth dependency ratio specification
#'
#' Preserves population aged 0--19 divided by population aged 20--64,
#' multiplied by 100. This function defines the specification but performs no
#' calculation.
#'
#' @return A validated indicator specification.
#' @export
dissertation_youth_dependency_ratio_spec <- function() {
  new_indicator_spec(
    indicator_id = "youth_dependency_ratio",
    indicator_type = "ratio",
    definition_version = "dissertation_v1",
    parameters = list(
      numerator = list(variable = "population", age_from = 0, age_to = 19),
      denominator = list(variable = "population", age_from = 20, age_to = 64),
      multiplier = 100,
      unit = "persons_under_20_per_100_persons_20_64",
      interpretation = "quotient"
    )
  )
}

#' Dissertation mean-age specification
#'
#' References the source-provided Regionaldatenbank mean age calculated from
#' all available individual ages. No mean-age calculation is performed.
#'
#' @return A validated indicator specification.
#' @export
dissertation_mean_age_spec <- function() {
  new_indicator_spec(
    indicator_id = "mean_age",
    indicator_type = "source_provided",
    definition_version = "dissertation_v1",
    parameters = list(
      statistic = "12411",
      table = "12411-07-01-4",
      measure = "BEV519",
      selection = list(sex = "Insgesamt"),
      unit = "years",
      reference_date_semantics = "stock_at_31_december"
    )
  )
}

#' Dissertation population-density specification
#'
#' Defines population divided by district area in square kilometres at the
#' same 31 December reference date. No rounding is requested.
#'
#' @return A validated indicator specification.
#' @export
dissertation_population_density_spec <- function() {
  new_indicator_spec(
    indicator_id = "population_density",
    indicator_type = "density",
    definition_version = "dissertation_v1",
    parameters = list(
      numerator = list(variable = "population", unit = "persons"),
      denominator = list(variable = "area", unit = "km2"),
      multiplier = 1,
      unit = "persons_per_km2",
      reference_date_alignment = "exact"
    )
  )
}

# Internal while the generic specification schema evolves.
new_typology_spec <- function(typology_id, indicators, reference_years,
                              temporal_aggregation, standardization, method,
                              method_parameters, definition_version) {
  x <- list(
    typology_id = typology_id,
    indicators = indicators,
    reference_years = reference_years,
    temporal_aggregation = temporal_aggregation,
    standardization = standardization,
    method = method,
    method_parameters = method_parameters,
    definition_version = definition_version
  )
  validate_typology_spec(x)
  x
}

#' Dissertation regional typology specification
#'
#' Preserves the 2017--2020 arithmetic-mean, z-score, three-center k-means
#' specification. All three indicators reference their authoritative
#' `dissertation_v1` definitions. The youth age boundaries remain authoritative
#' only in [dissertation_youth_dependency_ratio_spec()]. No clustering is
#' performed.
#'
#' @return A validated typology specification.
#' @export
dissertation_typology_spec <- function() {
  mean_age <- dissertation_mean_age_spec()
  youth <- dissertation_youth_dependency_ratio_spec()
  density <- dissertation_population_density_spec()
  refs <- list(
    list(
      indicator_id = density$indicator_id,
      definition_version = density$definition_version
    ),
    list(
      indicator_id = mean_age$indicator_id,
      definition_version = mean_age$definition_version
    ),
    list(
      indicator_id = youth$indicator_id,
      definition_version = youth$definition_version
    )
  )
  new_typology_spec(
    typology_id = "dissertation_v1",
    indicators = refs,
    reference_years = 2017:2020,
    temporal_aggregation = "arithmetic_mean",
    standardization = "z_score",
    method = "k_means",
    method_parameters = list(
      centers = 3L,
      nstart = 25L,
      algorithm = "Lloyd",
      iter.max = 50L,
      seed = 123L,
      center = TRUE,
      scale = TRUE,
      scale_semantics = "base_r_sample_sd_n_minus_1",
      row_order = "ascending_five_character_geo_id",
      historical_label_mapping = list(
        list(raw_cluster = 1L, cluster_code = "ClD",
             cluster_label = "dichte Regionen"),
        list(raw_cluster = 2L, cluster_code = "ClJ",
             cluster_label = "familiengepr\u00e4gte Regionen"),
        list(raw_cluster = 3L, cluster_code = "ClA",
             cluster_label = "\u00e4ltere, l\u00e4ndliche Regionen")
      ),
      label_mapping_scope = "historical_fitted_solution_only"
    ),
    definition_version = "dissertation_v1"
  )
}

# Internal while the generic specification schema evolves.
new_analysis_spec <- function(target_geo_level, target_geo_vintage,
                              surveillance_source, surveillance_version,
                              context_source, context_version,
                              indicator_spec_version, typology_spec_version,
                              package_version) {
  x <- list(
    target_geo_level = target_geo_level,
    target_geo_vintage = target_geo_vintage,
    surveillance_source = surveillance_source,
    surveillance_version = surveillance_version,
    context_source = context_source,
    context_version = context_version,
    indicator_spec_version = indicator_spec_version,
    typology_spec_version = typology_spec_version,
    package_version = package_version
  )
  validate_analysis_spec(x)
  x
}
