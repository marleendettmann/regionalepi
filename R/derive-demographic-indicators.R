#' Derive annual population density
#'
#' Divides official district population by official district area in square
#' kilometres after exact identifier and reference-date alignment. No name
#' matching, rounding, weighting, or geographic harmonization is performed.
#'
#' @param population Result returned by [fetch_regional_population()].
#' @param area Result returned by [fetch_regional_area()].
#' @param specification A validated density indicator specification.
#' @return A list with `data`, `diagnostics`, and derived `provenance`.
#' @export
derive_population_density <- function(
    population, area,
    specification = dissertation_population_density_spec()) {
  contract <- "population density derivation"
  .require_result_parts(population, contract)
  .require_result_parts(area, contract)
  validate_population_denominator(population$data)
  validate_regional_area(area$data)
  validate_indicator_spec(specification)
  if (!identical(specification$indicator_type, "density")) {
    .stop_contract(contract, "specification must have density type")
  }
  pop <- population$data
  areas <- area$data
  pop_key <- paste(pop$geo_id, format(pop$reference_date), sep = "\r")
  area_key <- paste(areas$geo_id, format(areas$reference_date), sep = "\r")
  if (anyDuplicated(pop_key) || anyDuplicated(area_key) ||
      !setequal(pop_key, area_key)) {
    .stop_contract(contract, "population and area keys must match exactly")
  }
  areas <- areas[match(pop_key, area_key), , drop = FALSE]
  if (!identical(pop$geo_id, areas$geo_id) ||
      !identical(pop$reference_date, areas$reference_date)) {
    .stop_contract(contract, "component alignment failed")
  }
  if (!identical(pop$geo_name, areas$geo_name) ||
      !identical(pop$geo_level, areas$geo_level)) {
    .stop_contract(contract, "component names or geographic levels disagree")
  }
  vintage_compatible <- (is.na(pop$geo_vintage) & is.na(areas$geo_vintage)) |
    (!is.na(pop$geo_vintage) & !is.na(areas$geo_vintage) &
       pop$geo_vintage == areas$geo_vintage)
  if (any(!vintage_compatible)) {
    .stop_contract(contract, "component geographic vintages disagree")
  }
  if (anyNA(pop$population) || anyNA(areas$area_km2) ||
      any(areas$area_km2 <= 0)) {
    .stop_contract(contract, "population and strictly positive area are required")
  }
  provenance_id <- paste0(
    "derived_population_density_", specification$definition_version
  )
  values <- pop$population / areas$area_km2 * specification$parameters$multiplier
  data <- data.frame(
    geo_id = pop$geo_id, geo_name = pop$geo_name, geo_level = pop$geo_level,
    geo_vintage = pop$geo_vintage, reference_date = pop$reference_date,
    indicator_id = specification$indicator_id, indicator_value = values,
    indicator_unit = specification$parameters$unit,
    definition_version = specification$definition_version,
    value_origin = "derived", source = .regional_source,
    population_basis = pop$population_basis, provenance_id = provenance_id,
    stringsAsFactors = FALSE
  )
  validate_demographic_indicator(data)
  area_provenance_ids <- unique(areas$provenance_id)
  list(
    data = data,
    diagnostics = list(
      operation = "derive_population_density", joined_observations = nrow(data),
      unmatched_population = 0L, unmatched_area = 0L,
      rounding_performed = FALSE,
      geo_vintage_unresolved = all(is.na(data$geo_vintage))
    ),
    provenance = stats::setNames(list(list(
      operation = "population_divided_by_area_km2",
      definition_version = specification$definition_version,
      population_source = list(
        source = unique(pop$source), source_table = unique(pop$source_table),
        provenance = population$provenance
      ),
      area_source = list(
        source = unique(areas$source), source_table = unique(areas$source_table),
        provenance_ids = area_provenance_ids, provenance = area$provenance
      ),
      multiplier = specification$parameters$multiplier,
      output_unit = specification$parameters$unit,
      rounding = "none"
    )), provenance_id)
  )
}

.require_result_parts <- function(x, contract) {
  if (!is.list(x) || !all(c("data", "diagnostics", "provenance") %in% names(x))) {
    .stop_contract(contract, "inputs must contain data, diagnostics, and provenance")
  }
}

.validate_youth_dependency_reproduction <- function(source_indicator,
                                                      age_population) {
  contract <- "youth dependency validation"
  .require_result_parts(source_indicator, contract)
  .require_result_parts(age_population, contract)
  validate_demographic_indicator(source_indicator$data)
  validate_context_population(age_population$data)
  .validate_age_population_intervals(age_population$data)
  source <- source_indicator$data
  if (any(source$indicator_id != "youth_dependency_ratio") ||
      any(source$value_origin != "source_provided")) {
    .stop_contract(contract, "authoritative source youth quotient is required")
  }
  ages <- age_population$data
  age_key <- paste(ages$geo_id, format(ages$reference_date), sep = "\r")
  pieces <- lapply(split(seq_len(nrow(ages)), age_key), function(rows) {
    group <- ages[rows, , drop = FALSE]
    numerator <- sum(group$population[
      group$age_from >= 0L & !is.na(group$age_to) & group$age_to <= 19L
    ])
    denominator <- sum(group$population[
      group$age_from >= 20L & !is.na(group$age_to) & group$age_to <= 64L
    ])
    if (denominator <= 0) {
      .stop_contract(contract, "age 20-64 denominator must be positive")
    }
    data.frame(
      geo_id = group$geo_id[[1L]], reference_date = group$reference_date[[1L]],
      derived_value = numerator / denominator * 100,
      numerator = numerator, denominator = denominator,
      stringsAsFactors = FALSE
    )
  })
  derived <- do.call(rbind, pieces)
  rownames(derived) <- NULL
  source_key <- paste(source$geo_id, format(source$reference_date), sep = "\r")
  derived_key <- paste(derived$geo_id, format(derived$reference_date), sep = "\r")
  if (!setequal(source_key, derived_key)) {
    .stop_contract(contract, "source and age-population keys must match exactly")
  }
  age_match <- match(source_key, age_key)
  vintage_compatible <-
    (is.na(source$geo_vintage) & is.na(ages$geo_vintage[age_match])) |
    (!is.na(source$geo_vintage) & !is.na(ages$geo_vintage[age_match]) &
       source$geo_vintage == ages$geo_vintage[age_match])
  if (!identical(source$geo_name, ages$geo_name[age_match]) ||
      !identical(source$geo_level, ages$geo_level[age_match]) ||
      any(!vintage_compatible)) {
    .stop_contract(contract, "source and age-population geography must agree")
  }
  derived <- derived[match(source_key, derived_key), , drop = FALSE]
  difference <- abs(source$indicator_value - round(derived$derived_value, 1L))
  if (any(difference > sqrt(.Machine$double.eps))) {
    .stop_contract(contract, "source quotient is not reproduced at one decimal")
  }
  list(
    matched_observations = nrow(source), all_matched = TRUE,
    maximum_rounded_difference = max(difference), comparison = data.frame(
      geo_id = source$geo_id, reference_date = source$reference_date,
      source_value = source$indicator_value,
      derived_value = derived$derived_value,
      rounded_derived_value = round(derived$derived_value, 1L),
      numerator = derived$numerator, denominator = derived$denominator,
      stringsAsFactors = FALSE
    )
  )
}

.combine_demographic_indicators <- function(mean_age, youth, density) {
  for (x in list(mean_age, youth, density)) validate_demographic_indicator(x)
  expected <- c("mean_age", "youth_dependency_ratio", "population_density")
  data <- rbind(mean_age, youth, density)
  if (!setequal(unique(data$indicator_id), expected)) {
    .stop_contract("demographic indicator combination",
                   "the three authoritative indicators are required")
  }
  .validate_indicator_geography_sets(data)
  rownames(data) <- NULL
  data
}

.validate_indicator_geography_sets <- function(data) {
  validate_demographic_indicator(data)
  group <- paste(format(data$reference_date), data$indicator_id,
                 data$definition_version, sep = "\r")
  sets <- lapply(split(data$geo_id, group), sort)
  if (length(sets) > 1L && any(!vapply(sets[-1L], identical, logical(1L), sets[[1L]]))) {
    .stop_contract("demographic indicator combination",
                   "annual indicator geographic identifier sets must match exactly")
  }
  invisible(data)
}

#' Summarize annual indicators over an explicit reference period
#'
#' Calculates only an unweighted arithmetic mean. Every geographic indicator
#' must have exactly one complete observation for each supplied year. The
#' dissertation workflow supplies its years and method from
#' [dissertation_typology_spec()].
#'
#' @param indicators A validated annual `demographic_indicator` data frame.
#' @param reference_years Unique whole-valued years to summarize.
#' @param method Exactly `"arithmetic_mean"`.
#' @return A list with period-summary `data`, `diagnostics`, and `provenance`.
#' @export
summarize_indicator_period <- function(
    indicators, reference_years, method = "arithmetic_mean") {
  contract <- "indicator period summary"
  validate_demographic_indicator(indicators)
  if (!is.numeric(reference_years) || !length(reference_years) ||
      anyNA(reference_years) || any(reference_years != floor(reference_years)) ||
      anyDuplicated(reference_years)) {
    .stop_contract(contract, "`reference_years` must be unique whole values")
  }
  reference_years <- sort(as.integer(reference_years))
  if (!identical(method, "arithmetic_mean")) {
    .stop_contract(contract, "only arithmetic_mean is supported")
  }
  dates <- as.Date(sprintf("%d-12-31", reference_years))
  if (!setequal(unique(indicators$reference_date), dates)) {
    .stop_contract(contract, "annual observations must exactly match reference years")
  }
  .validate_indicator_geography_sets(indicators)
  group <- paste(indicators$geo_id, indicators$indicator_id,
                 indicators$definition_version, sep = "\r")
  rows <- lapply(split(seq_len(nrow(indicators)), group), function(index) {
    x <- indicators[index, , drop = FALSE]
    if (nrow(x) != length(reference_years) ||
        !setequal(x$reference_date, dates) ||
        length(unique(x$indicator_unit)) != 1L ||
        length(unique(x$geo_name)) != 1L || length(unique(x$geo_level)) != 1L) {
      .stop_contract(contract, "each indicator must have one compatible row per year")
    }
    provenance_id <- paste0(
      "arithmetic_mean_", x$indicator_id[[1L]], "_", x$definition_version[[1L]]
    )
    data.frame(
      geo_id = x$geo_id[[1L]], geo_name = x$geo_name[[1L]],
      geo_level = x$geo_level[[1L]], indicator_id = x$indicator_id[[1L]],
      indicator_value = mean(x$indicator_value),
      indicator_unit = x$indicator_unit[[1L]],
      definition_version = x$definition_version[[1L]],
      period_start = min(dates), period_end = max(dates),
      aggregation_method = method,
      annual_observation_count = as.integer(length(reference_years)),
      provenance_id = provenance_id, stringsAsFactors = FALSE
    )
  })
  data <- do.call(rbind, rows)
  rownames(data) <- NULL
  provenance_ids <- unique(data$provenance_id)
  provenance <- stats::setNames(lapply(provenance_ids, function(id) {
    output_row <- data[data$provenance_id == id, , drop = FALSE][1L, ]
    selected <- indicators$indicator_id == output_row$indicator_id &
      indicators$definition_version == output_row$definition_version
    list(
      operation = "unweighted arithmetic mean",
      reference_years = reference_years,
      missing_value_removal = FALSE,
      input_provenance_ids = sort(unique(indicators$provenance_id[selected]))
    )
  }), provenance_ids)
  list(
    data = data,
    diagnostics = list(
      operation = "summarize_indicator_period", method = method,
      reference_years = reference_years,
      input_observations = nrow(indicators), output_observations = nrow(data),
      missing_values_removed = FALSE, weighting_performed = FALSE
    ),
    provenance = provenance
  )
}
