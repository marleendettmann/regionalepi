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
      unit = "percent"
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
#' specification. Population density and mean age are currently identified but
#' do not claim definition versions. The youth dependency ratio references the
#' authoritative version returned by
#' [dissertation_youth_dependency_ratio_spec()]. No clustering is performed.
#'
#' @return A validated typology specification.
#' @export
dissertation_typology_spec <- function() {
  youth <- dissertation_youth_dependency_ratio_spec()
  refs <- list(
    list(indicator_id = "population_density"),
    list(indicator_id = "mean_age"),
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
      seed = 123L
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
