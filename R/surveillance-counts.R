#' Assemble reviewed SurvStat case-count observations
#'
#' Applies the reviewed 399-plus-one construction to additive case counts:
#' source-level units listed in `exclude_geo_ids` are removed from the resolved
#' Kreis query and one independently queried canonical replacement observation
#' is appended per week. The excluded additive sum is retained only as a
#' validation diagnostic and never replaces the independent count.
#'
#' @param base_data Resolved Kreis count observations.
#' @param replacement_data Resolved replacement count observations.
#' @param base_provenance Query-level provenance for `base_data`.
#' @param replacement_provenance Query-level provenance for replacement data.
#' @param exclude_geo_ids Reviewed source-unit identifiers to remove.
#' @param replacement_geo_id One canonical replacement identifier.
#' @param expected_output_units Expected units per week.
#' @return A list with `data`, `diagnostics`, and two-query `provenance`.
#' @export
assemble_surveillance_cases <- function(
    base_data, replacement_data, base_provenance, replacement_provenance,
    exclude_geo_ids, replacement_geo_id,
    expected_output_units = 400L) {
  contract <- "surveillance cases assembly"
  validate_surveillance(base_data)
  validate_surveillance(replacement_data)
  .validate_cases_query_provenance(base_provenance, contract)
  .validate_cases_query_provenance(replacement_provenance, contract)
  .validate_count_query_pair(base_provenance, replacement_provenance, contract)
  .check_character(exclude_geo_ids, "exclude_geo_ids", contract)
  .check_scalar_nonempty_character(replacement_geo_id, "replacement_geo_id", contract)
  .check_scalar_number(expected_output_units, "expected_output_units", contract,
                       non_negative = TRUE)
  if (expected_output_units != floor(expected_output_units) ||
      anyDuplicated(exclude_geo_ids)) {
    .stop_contract(contract, "reviewed unit counts and exclusions must be whole and unique")
  }
  if (anyNA(c(base_data$geo_id, replacement_data$geo_id))) {
    .stop_contract(contract, "geographic identifiers must be resolved before assembly")
  }
  temporal <- c("date", "reporting_year", "reporting_week")
  .require_columns(base_data, temporal, contract)
  .require_columns(replacement_data, temporal, contract)
  key <- function(x) do.call(paste, c(lapply(x[temporal], as.character), sep = "\r"))
  base_key <- key(base_data)
  replacement_key <- key(replacement_data)
  weeks <- unique(base_key)
  if (!setequal(weeks, unique(replacement_key))) {
    .stop_contract(contract, "base and replacement temporal coverage must match exactly")
  }
  output <- vector("list", length(weeks))
  comparison <- vector("list", length(weeks))
  for (i in seq_along(weeks)) {
    b <- base_data[base_key == weeks[[i]], , drop = FALSE]
    r <- replacement_data[replacement_key == weeks[[i]], , drop = FALSE]
    if (anyDuplicated(b$geo_id) || nrow(r) != 1L ||
        !identical(r$geo_id, replacement_geo_id)) {
      .stop_contract(contract, "each week requires unique base IDs and one replacement ID")
    }
    excluded <- b[b$geo_id %in% exclude_geo_ids, , drop = FALSE]
    if (!setequal(excluded$geo_id, exclude_geo_ids) ||
        any(b$geo_id[!b$geo_id %in% exclude_geo_ids] == replacement_geo_id)) {
      .stop_contract(contract, "reviewed exclusion or replacement coverage is incompatible")
    }
    retained <- b[!b$geo_id %in% exclude_geo_ids, , drop = FALSE]
    output[[i]] <- rbind(retained, r)
    if (nrow(output[[i]]) != expected_output_units ||
        anyDuplicated(output[[i]]$geo_id)) {
      .stop_contract(contract, "assembled weekly geography is not the reviewed output")
    }
    complete <- !anyNA(excluded$cases)
    excluded_sum <- if (complete) sum(excluded$cases) else NA_real_
    comparison[[i]] <- data.frame(
      date = r$date, reporting_year = r$reporting_year,
      reporting_week = r$reporting_week,
      excluded_source_sum = excluded_sum,
      replacement_cases = r$cases,
      comparable = complete && !is.na(r$cases),
      equal = if (complete && !is.na(r$cases)) excluded_sum == r$cases else NA,
      stringsAsFactors = FALSE
    )
  }
  data <- do.call(rbind, output)
  data <- data[order(data$date, data$geo_id, method = "radix"), , drop = FALSE]
  rownames(data) <- NULL
  validate_surveillance(data)
  comparison <- do.call(rbind, comparison)
  provenance <- stats::setNames(
    list(base_provenance, replacement_provenance),
    c(base_provenance$query_id, replacement_provenance$query_id)
  )
  list(data = data, diagnostics = list(
    operation = "assemble_surveillance_cases",
    output_units_per_week = expected_output_units,
    excluded_source_units = length(exclude_geo_ids),
    berlin_additive_comparison = comparison,
    all_comparable_sums_equal = if (any(comparison$comparable))
      all(comparison$equal[comparison$comparable]) else NA,
    incidence_calculation_performed = FALSE
  ), provenance = provenance)
}

#' Combine source-provided incidence and reported cases
#'
#' Joins already assembled canonical observations exclusively by `geo_id` and
#' `date`, after validating exact key equality and compatible reviewed query
#' semantics. Incidence and counts are preserved verbatim; no rate is computed.
#'
#' @param incidence An assembled result with `data` and query `provenance`.
#' @param counts An assembled result with `data` and query `provenance`.
#' @return A list with combined `data`, diagnostics, and both provenance registries.
#' @export
combine_surveillance_incidence_counts <- function(incidence, counts) {
  contract <- "incidence count compatibility"
  .require_named_list(incidence, c("data", "provenance"), contract)
  .require_named_list(counts, c("data", "provenance"), contract)
  validate_surveillance_incidence(incidence$data)
  validate_surveillance(counts$data)
  for (query in incidence$provenance) .validate_incidence_query_provenance(query, contract)
  for (query in counts$provenance) .validate_cases_query_provenance(query, contract)
  incidence_semantics <- .query_registry_semantics(incidence$provenance, contract)
  count_semantics <- .query_registry_semantics(counts$provenance, contract)
  compared <- c("source", "source_version", "pathogen", "reporting_years",
                "reference_definition", "reporting_path", "data_status")
  bad <- compared[!vapply(compared, function(field) {
    identical(incidence_semantics[[field]], count_semantics[[field]])
  }, logical(1L))]
  if (length(bad)) {
    .stop_contract(contract, paste0("incompatible query semantics: ",
      paste(bad, collapse = ", ")))
  }
  observation_fields <- c("geo_id", "date")
  full_fields <- c(observation_fields, "reporting_year", "reporting_week")
  .require_columns(incidence$data, full_fields, contract)
  .require_columns(counts$data, full_fields, contract)
  make_key <- function(x) paste(x$geo_id, x$date, sep = "\r")
  incidence_key <- make_key(incidence$data)
  count_key <- make_key(counts$data)
  if (anyDuplicated(incidence_key) || anyDuplicated(count_key)) {
    .stop_contract(contract, "observation keys must be unique")
  }
  if (!setequal(incidence_key, count_key)) {
    .stop_contract(contract, "incidence and count key sets must match exactly")
  }
  position <- match(incidence_key, count_key)
  if (!identical(incidence$data$reporting_year,
                 counts$data$reporting_year[position]) ||
      !identical(incidence$data$reporting_week,
                 counts$data$reporting_week[position])) {
    .stop_contract(contract, "date and reporting year/week metadata must match exactly")
  }
  output <- incidence$data
  output$cases <- counts$data$cases[position]
  output$count_query_id <- counts$data$query_id[position]
  list(data = output, diagnostics = list(
    observation_count = nrow(output), exact_key_equality = TRUE,
    incidence_unchanged = identical(output$incidence, incidence$data$incidence),
    rate_calculation_performed = FALSE,
    data_status_policy = "exact_cube_status_equality"
  ), provenance = list(incidence = incidence$provenance, counts = counts$provenance))
}

.validate_cases_query_provenance <- function(x, contract) {
  .validate_incidence_query_provenance_structure(x, contract)
  if (!identical(x$measure, "cases") || !identical(x$value_semantics, "additive") ||
      !identical(x$source_measure_id, .survstat_cases_measure$source_id) ||
      !identical(x$source_measure_label, .survstat_cases_measure$source_label)) {
    .stop_contract(contract, "query provenance must describe reviewed additive cases")
  }
  invisible(x)
}

.validate_incidence_query_provenance_structure <- function(x, contract) {
  required <- c("query_id", "query_role", "source", "source_version", "measure",
    "value_semantics", "pathogen", "reporting_years", "time_unit",
    "source_geography_dimension", "reference_definition", "reporting_path",
    "retrieved_at", "data_status", "reporting_week_coverage")
  .require_named_list(x, required, contract)
  for (field in c("query_id", "query_role", "source", "source_version", "measure",
                  "value_semantics", "pathogen", "time_unit",
                  "source_geography_dimension")) {
    .check_scalar_nonempty_character(x[[field]], field, contract)
  }
  .check_scalar_logical_or_na(x$reference_definition, "reference_definition", contract)
  .check_scalar_character_or_na(x$reporting_path, "reporting_path", contract)
  .check_scalar_posixct_or_na(x$retrieved_at, "retrieved_at", contract)
  .check_scalar_posixct_or_na(x$data_status, "data_status", contract)
  if (!is.numeric(x$reporting_years) || !length(x$reporting_years) ||
      anyNA(x$reporting_years) || any(x$reporting_years != floor(x$reporting_years)) ||
      anyDuplicated(x$reporting_years) || !is.list(x$reporting_week_coverage) ||
      !identical(names(x$reporting_week_coverage), as.character(x$reporting_years))) {
    .stop_contract(contract, "query reporting years/week coverage are malformed")
  }
  invisible(x)
}

.validate_count_query_pair <- function(base, replacement, contract) {
  if (!identical(base$query_role, "kreis_cases") ||
      !identical(replacement$query_role, "bundesland_cases")) {
    .stop_contract(contract, "count query roles must be kreis_cases and bundesland_cases")
  }
  fields <- c("source", "source_version", "measure", "value_semantics", "pathogen",
              "reporting_years", "reference_definition", "reporting_path", "data_status")
  if (any(!vapply(fields, function(field) identical(base[[field]], replacement[[field]]),
                  logical(1L)))) {
    .stop_contract(contract, "base and replacement count queries are incompatible")
  }
  invisible(TRUE)
}

.query_registry_semantics <- function(registry, contract) {
  if (!is.list(registry) || !length(registry)) {
    .stop_contract(contract, "query provenance registry must not be empty")
  }
  fields <- c("source", "source_version", "pathogen", "reporting_years",
              "reference_definition", "reporting_path", "data_status")
  out <- lapply(fields, function(field) {
    values <- lapply(registry, `[[`, field)
    if (!all(vapply(values[-1L], identical, logical(1L), values[[1L]]))) {
      .stop_contract(contract, paste0("query registry is inconsistent for `", field, "`"))
    }
    values[[1L]]
  })
  stats::setNames(out, fields)
}
