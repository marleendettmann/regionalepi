.demographic_snapshot_source <- "Regionaldatenbank Deutschland"
.demographic_snapshot_default <- "reviewed_default"

.snapshot_without_volatile_fields <- function(x) {
  if (!is.list(x)) return(x)
  x[c("retrieved_at", "created_at", "builder_package_version",
      "snapshot_id", "content_checksum")] <- NULL
  lapply(x, .snapshot_without_volatile_fields)
}

.snapshot_identity_provenance <- function(x) {
  fields <- c(
    "snapshot_version", "source", "source_data_status",
    "source_status_compatibility", "source_status_display_date", "source_tables",
    "source_measures", "covered_reference_dates", "population_basis",
    "component_provenance", "prior_snapshot_id", "refresh_semantics"
  )
  x[fields]
}

.snapshot_checksum <- function(data, identity_provenance) {
  payload <- list(
    data = data,
    provenance = .snapshot_without_volatile_fields(
      .snapshot_identity_provenance(identity_provenance))
  )
  path <- tempfile("regionalepi-snapshot-", fileext = ".bin")
  on.exit(unlink(path), add = TRUE)
  connection <- file(path, open = "wb")
  writeBin(serialize(payload, NULL, version = 3L), connection)
  close(connection)
  unname(tools::md5sum(path))
}

.snapshot_component_status <- function(component, name) {
  if (name %in% c("mean_age", "youth_dependency")) {
    status <- unique(vapply(component$provenance, function(x) x$data_status,
                            character(1L)))
  } else {
    status <- unique(component$data$data_status)
  }
  if (length(status) != 1L || is.na(status) || !nzchar(status)) {
    .stop_contract("demographic snapshot", paste(name,
      "must have exactly one complete source data status"))
  }
  status
}

.snapshot_component_retrieved_at <- function(component, name) {
  if (name %in% c("mean_age", "youth_dependency")) {
    values <- lapply(component$provenance, `[[`, "retrieved_at")
    value <- unique(do.call(c, values))
  } else {
    value <- unique(component$data$retrieved_at)
  }
  if (length(value) != 1L || is.na(value)) {
    .stop_contract("demographic snapshot", paste(name,
      "must have one retrieval timestamp"))
  }
  as.POSIXct(value, origin = "1970-01-01", tz = "UTC")
}

.validate_snapshot_statuses <- function(statuses) {
  parsed <- as.POSIXct(strptime(
    unname(statuses), format = "%d.%m.%Y / %H:%M:%S",
    tz = "Europe/Berlin"
  ))
  if (anyNA(parsed) || length(unique(format(parsed, "%Y-%m-%d"))) != 1L ||
      as.numeric(difftime(max(parsed), min(parsed), units = "mins")) > 15) {
    .stop_contract("demographic snapshot", paste(
      "component source statuses must be on one calendar date and within",
      "a 15-minute reviewed build window"))
  }
  list(
    rule = "same_calendar_date_within_15_minutes",
    parsed = parsed,
    display_date = format(max(parsed), "%d.%m.%Y")
  )
}

.compact_snapshot_component <- function(component, name) {
  data <- component$data
  if (name %in% c("mean_age", "youth_dependency")) {
    data <- data[, c("geo_id", "geo_name", "reference_date",
                     "indicator_value", "population_basis"), drop = FALSE]
  } else if (identical(name, "population")) {
    data <- data[, c("geo_id", "geo_name", "reference_date", "population",
                     "population_basis"), drop = FALSE]
  } else {
    data <- data[, c("geo_id", "geo_name", "reference_date", "area_km2"),
                 drop = FALSE]
  }
  data[order(data$reference_date, data$geo_id), , drop = FALSE]
}

.validate_snapshot_components <- function(components) {
  needed <- c("mean_age", "youth_dependency", "population", "area")
  if (!is.list(components) || !identical(sort(names(components)), sort(needed))) {
    .stop_contract("demographic snapshot", paste(
      "components must be exactly", paste(needed, collapse = ", ")))
  }
  validate_demographic_indicator(components$mean_age$data)
  validate_demographic_indicator(components$youth_dependency$data)
  validate_population_denominator(components$population$data)
  validate_regional_area(components$area$data)
  if (any(components$mean_age$data$indicator_id != "mean_age") ||
      any(components$youth_dependency$data$indicator_id !=
          "youth_dependency_ratio")) {
    .stop_contract("demographic snapshot",
                   "source indicator components have unexpected identities")
  }
  invisible(components)
}

.build_demographic_snapshot <- function(
    components, snapshot_version, prior_snapshot_id = NA_character_,
    builder = "data-raw/build-demographic-snapshot.R") {
  .validate_snapshot_components(components)
  if (!is.character(snapshot_version) || length(snapshot_version) != 1L ||
      is.na(snapshot_version) || !nzchar(snapshot_version)) {
    .stop_contract("demographic snapshot", "snapshot_version must be one string")
  }
  statuses <- vapply(names(components), function(name) {
    .snapshot_component_status(components[[name]], name)
  }, character(1L))
  status_compatibility <- .validate_snapshot_statuses(statuses)
  retrieved <- lapply(names(components), function(name) {
    .snapshot_component_retrieved_at(components[[name]], name)
  })
  names(retrieved) <- names(components)
  compact <- lapply(names(components), function(name) {
    .compact_snapshot_component(components[[name]], name)
  })
  names(compact) <- names(components)
  dates <- sort(unique(compact$population$reference_date))
  expected_dates <- as.Date(c(sprintf("%d-12-31", 2017:2020),
                              sprintf("%d-12-31", 2022:2024)))
  if (!identical(dates, expected_dates)) {
    .stop_contract("demographic snapshot",
      "the reviewed v1 snapshot must cover 2017-2020 and 2022-2024 exactly")
  }
  identity_provenance <- list(
    snapshot_version = snapshot_version,
    source = .demographic_snapshot_source,
    source_data_status = statuses,
    source_status_compatibility = status_compatibility$rule,
    source_status_display_date = status_compatibility$display_date,
    source_tables = c(
      mean_age = "12411-07-01-4", youth_dependency = "12411-08-01-4",
      population = "12411-01-01-4", area = "11111-01-01-4"),
    source_measures = c(
      mean_age = "BEV519", youth_dependency = "BEV216",
      population = "Bev\u00f6lkerungsstand", area = "FLC006"),
    covered_reference_dates = dates,
    population_basis = c("census_2011", "census_2022"),
    component_provenance = lapply(components, `[[`, "provenance"),
    prior_snapshot_id = prior_snapshot_id,
    refresh_semantics = paste(
      "A refresh is a reviewed development/release build of a new immutable",
      "snapshot; live Shiny retrieval never mutates this resource."))
  checksum <- .snapshot_checksum(compact, identity_provenance)
  snapshot_id <- paste0("regionalepi_demography_", substr(checksum, 1L, 16L))
  provenance <- c(identity_provenance, list(
    snapshot_id = snapshot_id,
    created_at = as.POSIXct(Sys.time(), tz = "UTC"),
    component_retrieved_at = retrieved,
    builder = builder,
    builder_package_version = as.character(utils::packageVersion("regionalepi")),
    content_checksum = checksum,
    versioning_policy = paste(
      "Create a new version when source status, dates, tables/measures, adapter",
      "interpretation, geography, values, or population-basis semantics change.")))
  snapshot <- list(
    data = compact,
    provenance = provenance,
    diagnostics = list(
      component_row_counts = vapply(compact, nrow, integer(1L)),
      geographic_unit_counts = stats::setNames(vapply(dates, function(date) {
        length(unique(compact$population$geo_id[
          compact$population$reference_date == date]))
      }, integer(1L)), format(dates)),
      source_status_coherent = TRUE,
      source_status_compatibility = status_compatibility$rule,
      exact_component_geography = TRUE,
      duplicate_count = 0L,
      geo_vintage_unresolved = TRUE,
      checksum_verified = TRUE))
  validate_demographic_snapshot(snapshot)
  snapshot
}

.snapshot_reconstruct_components <- function(snapshot, reference_dates = NULL) {
  validate_demographic_snapshot(snapshot)
  if (is.null(reference_dates)) {
    reference_dates <- snapshot$provenance$covered_reference_dates
  }
  if (!inherits(reference_dates, "Date") || anyNA(reference_dates) ||
      !all(reference_dates %in% snapshot$provenance$covered_reference_dates)) {
    .stop_contract("demographic snapshot",
                   "requested dates are not covered by the snapshot")
  }
  select <- function(x) x[x$reference_date %in% reference_dates, , drop = FALSE]
  source <- snapshot$provenance$source
  statuses <- snapshot$provenance$source_data_status
  retrieved <- snapshot$provenance$component_retrieved_at
  indicator <- function(name, spec, provenance_id, table) {
    x <- select(snapshot$data[[name]])
    data <- data.frame(
      geo_id = x$geo_id, geo_name = x$geo_name, geo_level = "district",
      geo_vintage = as.Date(NA), reference_date = x$reference_date,
      indicator_id = spec$indicator_id, indicator_value = x$indicator_value,
      indicator_unit = spec$parameters$unit,
      definition_version = spec$definition_version,
      value_origin = "source_provided", source = source,
      population_basis = x$population_basis, provenance_id = provenance_id,
      stringsAsFactors = FALSE)
    validate_demographic_indicator(data)
    list(data = data,
      diagnostics = list(source = source, source_table = table,
        data_status = statuses[[name]],
        snapshot_id = snapshot$provenance$snapshot_id),
      provenance = snapshot$provenance$component_provenance[[name]])
  }
  mean_age <- indicator("mean_age", dissertation_mean_age_spec(),
    "regional_mean_age_12411-07-01-4", "12411-07-01-4")
  youth <- indicator("youth_dependency",
    dissertation_youth_dependency_ratio_spec(),
    "regional_youth_dependency_12411-08-01-4", "12411-08-01-4")
  pop <- select(snapshot$data$population)
  population_data <- data.frame(
    geo_id = pop$geo_id, geo_name = pop$geo_name, geo_level = "district",
    geo_vintage = as.Date(NA), reference_date = pop$reference_date,
    population = pop$population, population_basis = pop$population_basis,
    source = source, source_table = "12411-01-01-4",
    retrieved_at = rep(retrieved$population, nrow(pop)),
    data_status = statuses[["population"]], stringsAsFactors = FALSE)
  validate_population_denominator(population_data)
  area <- select(snapshot$data$area)
  area_data <- data.frame(
    geo_id = area$geo_id, geo_name = area$geo_name, geo_level = "district",
    geo_vintage = as.Date(NA), reference_date = area$reference_date,
    area_km2 = area$area_km2, source = source,
    source_table = "11111-01-01-4", source_measure = "FLC006",
    retrieved_at = rep(retrieved$area, nrow(area)),
    data_status = statuses[["area"]],
    provenance_id = "regional_area_11111-01-01-4", stringsAsFactors = FALSE)
  validate_regional_area(area_data)
  list(
    mean_age = mean_age, youth = youth,
    population = list(data = population_data,
      diagnostics = list(source = source, source_table = "12411-01-01-4",
        data_status = statuses[["population"]],
        snapshot_id = snapshot$provenance$snapshot_id),
      provenance = snapshot$provenance$component_provenance$population),
    area = list(data = area_data,
      diagnostics = list(source = source, source_table = "11111-01-01-4",
        data_status = statuses[["area"]],
        snapshot_id = snapshot$provenance$snapshot_id),
      provenance = snapshot$provenance$component_provenance$area))
}

#' Validate a reviewed demographic snapshot
#'
#' @param x A demographic snapshot list.
#' @return `x`, invisibly.
#' @export
validate_demographic_snapshot <- function(x) {
  contract <- "demographic snapshot"
  if (!is.list(x) || !identical(sort(names(x)),
                                sort(c("data", "provenance", "diagnostics")))) {
    .stop_contract(contract, "snapshot must contain data, provenance, diagnostics")
  }
  needed <- c("snapshot_id", "snapshot_version", "created_at", "source",
    "source_data_status", "source_status_compatibility",
    "source_status_display_date", "source_tables", "source_measures",
    "covered_reference_dates", "population_basis", "component_provenance",
    "component_retrieved_at", "builder", "builder_package_version",
    "content_checksum", "versioning_policy", "refresh_semantics")
  if (!all(needed %in% names(x$provenance))) {
    .stop_contract(contract, "snapshot provenance is incomplete")
  }
  if (!identical(x$provenance$source, .demographic_snapshot_source) ||
      !grepl("^[0-9a-f]{32}$", x$provenance$content_checksum) ||
      !identical(x$provenance$snapshot_id, paste0(
        "regionalepi_demography_",
        substr(x$provenance$content_checksum, 1L, 16L)))) {
    .stop_contract(contract, "snapshot identity or checksum is malformed")
  }
  .check_posixct(x$provenance$created_at, "created_at", contract)
  if (length(x$provenance$created_at) != 1L ||
      !identical(names(x$provenance$source_data_status),
                 c("mean_age", "youth_dependency", "population", "area")) ||
      !identical(unname(x$provenance$source_tables), c(
        "12411-07-01-4", "12411-08-01-4", "12411-01-01-4",
        "11111-01-01-4")) ||
      !identical(unname(x$provenance$source_measures), c(
        "BEV519", "BEV216", "Bev\u00f6lkerungsstand", "FLC006"))) {
    .stop_contract(contract, "component source metadata is not the reviewed set")
  }
  if (!is.list(x$provenance$component_retrieved_at) ||
      !identical(names(x$provenance$component_retrieved_at),
                 c("mean_age", "youth_dependency", "population", "area")) ||
      any(vapply(x$provenance$component_retrieved_at, function(value) {
        !inherits(value, "POSIXct") || length(value) != 1L || is.na(value)
      }, logical(1L)))) {
    .stop_contract(contract, "component retrieval timestamps are incomplete")
  }
  dates <- x$provenance$covered_reference_dates
  if (!inherits(dates, "Date") || anyNA(dates) || anyDuplicated(dates) ||
      any(format(dates, "%m-%d") != "12-31")) {
    .stop_contract(contract, "covered reference dates are invalid")
  }
  status_check <- .validate_snapshot_statuses(x$provenance$source_data_status)
  if (!identical(x$provenance$source_status_compatibility,
                 status_check$rule) ||
      !identical(x$provenance$source_status_display_date,
                 status_check$display_date)) {
    .stop_contract(contract, "source-status compatibility metadata disagrees")
  }
  components <- c("mean_age", "youth_dependency", "population", "area")
  if (!identical(sort(names(x$data)), sort(components))) {
    .stop_contract(contract, "snapshot components are incomplete")
  }
  expected_columns <- list(
    mean_age = c("geo_id", "geo_name", "reference_date", "indicator_value",
                 "population_basis"),
    youth_dependency = c("geo_id", "geo_name", "reference_date",
                         "indicator_value", "population_basis"),
    population = c("geo_id", "geo_name", "reference_date", "population",
                   "population_basis"),
    area = c("geo_id", "geo_name", "reference_date", "area_km2"))
  for (name in components) {
    data <- x$data[[name]]
    .require_data_frame(data, contract)
    if (!identical(names(data), expected_columns[[name]])) {
      .stop_contract(contract, paste(name, "has unexpected columns"))
    }
    .check_character(data$geo_id, "geo_id", contract)
    .check_character(data$geo_name, "geo_name", contract)
    .check_date(data$reference_date, "reference_date", contract)
    if (any(!grepl("^[0-9]{5}$", data$geo_id)) ||
        any(!data$reference_date %in% dates) ||
        anyDuplicated(paste(data$geo_id, data$reference_date))) {
      .stop_contract(contract, paste(name,
        "must have unique five-character IDs for every covered date"))
    }
    value <- if (name == "population") data$population else if (name == "area")
      data$area_km2 else data$indicator_value
    .check_numeric(value, paste0(name, " value"), contract, non_negative = TRUE)
    if (identical(name, "area") && any(value <= 0)) {
      .stop_contract(contract, "area values must be strictly positive")
    }
    if ("population_basis" %in% names(data)) {
      .check_character(data$population_basis, "population_basis", contract)
      year <- as.integer(format(data$reference_date, "%Y"))
      expected_basis <- ifelse(year <= 2021L, "census_2011", "census_2022")
      if (!identical(data$population_basis, expected_basis)) {
        .stop_contract(contract, "population basis disagrees with reference date")
      }
    }
  }
  required_diagnostics <- c(
    "component_row_counts", "geographic_unit_counts",
    "source_status_coherent", "source_status_compatibility",
    "exact_component_geography", "duplicate_count",
    "geo_vintage_unresolved", "checksum_verified"
  )
  if (!all(required_diagnostics %in% names(x$diagnostics)) ||
      !identical(unname(x$diagnostics$component_row_counts),
                 unname(vapply(x$data, nrow, integer(1L)))) ||
      !isTRUE(x$diagnostics$source_status_coherent) ||
      !isTRUE(x$diagnostics$exact_component_geography) ||
      !identical(x$diagnostics$duplicate_count, 0L) ||
      !isTRUE(x$diagnostics$geo_vintage_unresolved) ||
      !isTRUE(x$diagnostics$checksum_verified)) {
    .stop_contract(contract, "snapshot diagnostics contradict content")
  }
  keys <- lapply(x$data, function(data) {
    split(paste(data$geo_id, data$geo_name, sep = "\r"), data$reference_date)
  })
  for (date in as.character(dates)) {
    sets <- lapply(keys, function(component) sort(component[[date]]))
    if (any(vapply(sets[-1L], function(value) !identical(value, sets[[1L]]),
                   logical(1L)))) {
      .stop_contract(contract,
                     "component geographic ID/name sets must match by date")
    }
  }
  checksum <- .snapshot_checksum(x$data, x$provenance)
  if (!identical(checksum, x$provenance$content_checksum)) {
    .stop_contract(contract, "content checksum does not match snapshot content")
  }
  invisible(x)
}

#' Access the reviewed demographic snapshot
#'
#' Returns the immutable reviewed Regionaldatenbank source snapshot without a
#' network request. Unsupported identifiers fail rather than falling back.
#'
#' @param snapshot Exactly `"reviewed_default"`.
#' @return A validated demographic snapshot list.
#' @export
regionalepi_demographic_snapshot <- function(snapshot = "reviewed_default") {
  if (!is.character(snapshot) || length(snapshot) != 1L || is.na(snapshot) ||
      !identical(snapshot, .demographic_snapshot_default)) {
    .stop_contract("demographic snapshot accessor",
                   "unsupported snapshot; available value is reviewed_default")
  }
  value <- get("regionalepi_demographic_snapshot_v1", envir = environment(),
               inherits = TRUE)
  validate_demographic_snapshot(value)
  value
}

.demographic_snapshot_period <- function(snapshot, reference_years) {
  dates <- as.Date(sprintf("%d-12-31", as.integer(reference_years)))
  components <- .snapshot_reconstruct_components(snapshot, dates)
  density <- derive_population_density(components$population, components$area)
  annual <- .combine_demographic_indicators(
    components$mean_age$data, components$youth$data, density$data)
  summary <- summarize_indicator_period(annual, reference_years)
  list(
    annual = annual, summary = summary,
    source = list(population = components$population, area = components$area,
      mean_age = components$mean_age, youth_dependency = components$youth),
    source_mode = "snapshot", snapshot_provenance = snapshot$provenance)
}
