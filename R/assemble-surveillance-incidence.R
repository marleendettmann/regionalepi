#' Assemble reviewed source-provided incidence replacements
#'
#' Removes explicitly reviewed source units from a base incidence data set and
#' appends explicitly reviewed replacement observations. The operation performs
#' no summation, averaging, interpolation, or other rate calculation. All
#' source-specific knowledge is supplied in `specification`.
#'
#' Both inputs must already have the geographic identities required by the
#' specification. Query-level provenance is supplied separately and retained
#' once in the result; observation rows retain their original compact
#' `query_id`. Temporal compatibility is derived from the observations actually
#' supplied to this call, not from the immutable scope of either original
#' source query. Callers must therefore subset both inputs to the intended
#' assembly scope before calling this function.
#'
#' @param base_data Canonical `surveillance_incidence` observations for the
#'   reviewed base query.
#' @param replacement_data Canonical `surveillance_incidence` observations for
#'   the reviewed replacement query.
#' @param base_provenance Internal query-level provenance returned by
#'   [read_survstat_incidence()] for the base query.
#' @param replacement_provenance Query-level provenance for the replacement.
#' @param specification Reviewed incidence-assembly specification data.
#' @return An ordinary list with assembled `data`, compact `diagnostics`, and a
#'   two-query `provenance` registry.
#' @export
assemble_surveillance_incidence <- function(
    base_data, replacement_data, base_provenance, replacement_provenance,
    specification) {
  contract <- "surveillance incidence assembly"
  validate_surveillance_incidence(base_data)
  validate_surveillance_incidence(replacement_data)
  spec <- .validate_incidence_assembly_spec(specification, contract)
  .validate_incidence_query_provenance(base_provenance, contract)
  .validate_incidence_query_provenance(replacement_provenance, contract)
  .validate_incidence_query_compatibility(
    base_provenance, replacement_provenance, spec, contract
  )
  if (!identical(names(base_data), names(replacement_data))) {
    .stop_contract(contract, "base and replacement observation columns must match exactly")
  }
  if (any(base_data$query_id != base_provenance$query_id)) {
    .stop_contract(contract, "base observation `query_id` does not match its provenance")
  }
  if (any(replacement_data$query_id != replacement_provenance$query_id)) {
    .stop_contract(
      contract,
      "replacement observation `query_id` does not match its provenance"
    )
  }
  if (identical(base_provenance$query_id, replacement_provenance$query_id)) {
    .stop_contract(contract, "base and replacement queries must have distinct identifiers")
  }

  temporal_keys <- spec$temporal_keys
  .require_columns(base_data, temporal_keys, contract)
  .require_columns(replacement_data, temporal_keys, contract)
  if (anyNA(base_data[temporal_keys]) || anyNA(replacement_data[temporal_keys])) {
    .stop_contract(contract, "temporal join keys must be complete")
  }
  base_keys <- .incidence_temporal_keys(base_data, temporal_keys)
  replacement_keys <- .incidence_temporal_keys(replacement_data, temporal_keys)
  assembly_coverage <- .incidence_assembly_coverage(
    base_data, replacement_data, contract
  )
  if (!identical(
    assembly_coverage$base_coverage,
    assembly_coverage$replacement_coverage
  )) {
    .stop_contract(contract, "base and replacement temporal coverage must match exactly")
  }

  excluded_ids <- spec$exclude_geo_id
  retained_rows <- rep(TRUE, nrow(base_data))
  output_parts <- vector("list", length(unique(base_keys)))
  keys <- unique(base_keys)
  for (index in seq_along(keys)) {
    key <- keys[[index]]
    base_index <- which(base_keys == key)
    replacement_index <- which(replacement_keys == key)
    base_ids <- base_data$geo_id[base_index]
    replacement_ids <- replacement_data$geo_id[replacement_index]
    if (anyNA(base_ids) || anyNA(replacement_ids)) {
      .stop_contract(contract, "assembly geographic identifiers must be complete")
    }
    if (anyDuplicated(base_ids) || anyDuplicated(replacement_ids)) {
      .stop_contract(contract, "each query must have one row per geography and temporal key")
    }
    if (length(base_ids) != spec$expected_base_units) {
      .stop_contract(contract, "unexpected base source-unit coverage")
    }
    present_exclusions <- base_ids[base_ids %in% excluded_ids]
    if (!setequal(present_exclusions, excluded_ids) ||
        length(present_exclusions) != spec$expected_excluded_units) {
      .stop_contract(contract, "missing or unexpected reviewed exclusion coverage")
    }
    if (length(replacement_ids) != spec$expected_replacement_units ||
        !identical(replacement_ids, spec$replacement_target_geo_id)) {
      .stop_contract(contract, "missing, duplicate, or unexpected replacement coverage")
    }
    keep <- !base_ids %in% excluded_ids
    retained_rows[base_index[!keep]] <- FALSE
    combined_ids <- c(base_ids[keep], replacement_ids)
    if (anyDuplicated(combined_ids) ||
        length(combined_ids) != spec$expected_output_units) {
      .stop_contract(contract, "assembled geographic coverage is not the reviewed output")
    }
  }
  if (any(base_data$geo_id[retained_rows] == spec$replacement_target_geo_id)) {
    .stop_contract(contract, "replacement target already exists among retained base units")
  }

  output <- rbind(base_data[retained_rows, , drop = FALSE], replacement_data)
  rownames(output) <- NULL
  validate_surveillance_incidence(output)
  provenance <- stats::setNames(
    list(base_provenance, replacement_provenance),
    c(base_provenance$query_id, replacement_provenance$query_id)
  )
  list(
    data = output,
    diagnostics = list(
      operation = "assemble_surveillance_incidence",
      specification_id = spec$spec_id,
      specification_version = spec$spec_version,
      base_query_id = base_provenance$query_id,
      replacement_query_id = replacement_provenance$query_id,
      assembly_reporting_years = assembly_coverage$reporting_years,
      assembly_reporting_week_coverage = assembly_coverage$week_coverage,
      base_query_reporting_years = base_provenance$reporting_years,
      replacement_query_reporting_years = replacement_provenance$reporting_years,
      temporal_groups = length(keys),
      excluded_source_units = length(excluded_ids),
      replacement_units_per_temporal_group = spec$expected_replacement_units,
      output_units_per_temporal_group = spec$expected_output_units,
      rate_summation_performed = FALSE,
      rate_averaging_performed = FALSE
    ),
    provenance = provenance
  )
}

.validate_incidence_assembly_spec <- function(x, contract) {
  required <- c(
    "spec_id", "spec_version", "measure", "base_query_role",
    "replacement_query_role", "exclude_geo_id", "replacement_target_geo_id",
    "temporal_keys", "expected_base_units", "expected_excluded_units",
    "expected_replacement_units", "expected_output_units", "review_status",
    "reason"
  )
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  if (!nrow(x)) .stop_contract(contract, "specification must contain exclusions")
  for (field in c(
    "spec_id", "spec_version", "measure", "base_query_role",
    "replacement_query_role", "exclude_geo_id", "replacement_target_geo_id",
    "temporal_keys", "review_status", "reason"
  )) {
    .check_character(x[[field]], field, contract)
  }
  for (field in c(
    "expected_base_units", "expected_excluded_units",
    "expected_replacement_units", "expected_output_units"
  )) {
    .check_whole_number(x[[field]], field, contract, non_negative = TRUE)
  }
  uniform <- setdiff(required, "exclude_geo_id")
  if (any(vapply(x[uniform], function(value) length(unique(value)) != 1L,
                 logical(1L)))) {
    .stop_contract(contract, "all non-exclusion specification fields must be uniform")
  }
  if (anyDuplicated(x$exclude_geo_id)) {
    .stop_contract(contract, "reviewed exclusion identifiers must be unique")
  }
  if (!all(x$measure == "incidence") || !all(x$review_status == "reviewed")) {
    .stop_contract(contract, "measure must be incidence and review status reviewed")
  }
  if (unique(x$expected_excluded_units) != nrow(x)) {
    .stop_contract(contract, "expected excluded-unit count must equal specification rows")
  }
  expected <- unique(x$expected_base_units) - unique(x$expected_excluded_units) +
    unique(x$expected_replacement_units)
  if (unique(x$expected_replacement_units) != 1L ||
      unique(x$expected_output_units) != expected) {
    .stop_contract(
      contract,
      "reviewed counts must describe one replacement and base - excluded + replacement"
    )
  }
  if (unique(x$replacement_target_geo_id) %in% x$exclude_geo_id) {
    .stop_contract(contract, "replacement target must not be an excluded source ID")
  }
  temporal_keys <- strsplit(unique(x$temporal_keys), ";", fixed = TRUE)[[1L]]
  if (!length(temporal_keys) || any(!nzchar(temporal_keys)) ||
      anyDuplicated(temporal_keys)) {
    .stop_contract(contract, "temporal keys must be unique semicolon-separated names")
  }
  list(
    spec_id = unique(x$spec_id),
    spec_version = unique(x$spec_version),
    base_query_role = unique(x$base_query_role),
    replacement_query_role = unique(x$replacement_query_role),
    exclude_geo_id = x$exclude_geo_id,
    replacement_target_geo_id = unique(x$replacement_target_geo_id),
    temporal_keys = temporal_keys,
    expected_base_units = unique(x$expected_base_units),
    expected_excluded_units = unique(x$expected_excluded_units),
    expected_replacement_units = unique(x$expected_replacement_units),
    expected_output_units = unique(x$expected_output_units)
  )
}

.validate_incidence_query_provenance <- function(x, contract) {
  required <- c(
    "query_id", "query_role", "source", "source_version", "measure",
    "value_semantics", "pathogen", "reporting_years", "time_unit",
    "source_geography_dimension", "row_dimension", "column_dimension",
    "reference_definition", "reporting_path", "epidemiological_filters",
    "retrieved_at", "data_status", "reporting_week_coverage"
  )
  .require_named_list(x, required, contract)
  for (field in c(
    "query_id", "query_role", "source", "source_version", "measure",
    "value_semantics", "pathogen", "time_unit", "source_geography_dimension",
    "row_dimension", "column_dimension"
  )) .check_scalar_nonempty_character(x[[field]], field, contract)
  if (!identical(x$measure, "incidence") ||
      !identical(x$value_semantics, "non_additive")) {
    .stop_contract(contract, "query provenance must describe non-additive incidence")
  }
  .check_scalar_logical_or_na(x$reference_definition, "reference_definition", contract)
  .check_scalar_character_or_na(x$reporting_path, "reporting_path", contract)
  .check_scalar_posixct_or_na(x$retrieved_at, "retrieved_at", contract)
  .check_scalar_posixct_or_na(x$data_status, "data_status", contract)
  if (!is.list(x$reporting_week_coverage) ||
      !identical(names(x$reporting_week_coverage), as.character(x$reporting_years))) {
    .stop_contract(contract, "reporting-week coverage must be named by reporting year")
  }
  if (!is.numeric(x$reporting_years) || !length(x$reporting_years) ||
      anyNA(x$reporting_years) || any(x$reporting_years != floor(x$reporting_years)) ||
      anyDuplicated(x$reporting_years)) {
    .stop_contract(contract, "query reporting years must be unique whole values")
  }
  invisible(x)
}

.validate_incidence_query_compatibility <- function(base, replacement, spec,
                                                     contract) {
  if (!identical(base$query_role, spec$base_query_role) ||
      !identical(replacement$query_role, spec$replacement_query_role)) {
    .stop_contract(contract, "query roles do not match the reviewed specification")
  }
  identical_fields <- c(
    "source", "source_version", "pathogen", "measure", "value_semantics",
    "reference_definition", "reporting_path", "epidemiological_filters",
    "time_unit", "data_status"
  )
  incompatible <- identical_fields[!vapply(
    identical_fields,
    function(field) identical(base[[field]], replacement[[field]]),
    logical(1L)
  )]
  if (length(incompatible)) {
    .stop_contract(
      contract,
      sprintf("incompatible query provenance: %s", paste(incompatible, collapse = ", "))
    )
  }
  invisible(TRUE)
}

.incidence_assembly_coverage <- function(base, replacement, contract) {
  coverage_fields <- c("reporting_year", "reporting_week", "date")
  .require_columns(base, coverage_fields, contract)
  .require_columns(replacement, coverage_fields, contract)
  if (anyNA(base[coverage_fields]) || anyNA(replacement[coverage_fields])) {
    .stop_contract(contract, "observed temporal coverage must be complete")
  }
  normalize <- function(x) {
    coverage <- unique(x[coverage_fields])
    coverage <- coverage[order(
      coverage$reporting_year, coverage$reporting_week, coverage$date
    ), , drop = FALSE]
    rownames(coverage) <- NULL
    coverage
  }
  base_coverage <- normalize(base)
  replacement_coverage <- normalize(replacement)
  years <- sort(unique(base_coverage$reporting_year))
  week_coverage <- stats::setNames(
    lapply(years, function(year) {
      sort(base_coverage$reporting_week[base_coverage$reporting_year == year])
    }),
    as.character(years)
  )
  list(
    base_coverage = base_coverage,
    replacement_coverage = replacement_coverage,
    reporting_years = years,
    week_coverage = week_coverage
  )
}

.incidence_temporal_keys <- function(x, fields) {
  do.call(paste, c(lapply(x[fields], as.character), sep = "\r"))
}
