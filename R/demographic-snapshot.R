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
    "source_measures", "covered_reference_dates", "covered_reporting_years",
    "population_basis",
    "component_provenance", "prior_snapshot_id", "refresh_semantics"
  )
  x[intersect(fields, names(x))]
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

.validate_snapshot_statuses <- function(
    statuses, independently_reviewed = character()) {
  parsed <- as.POSIXct(strptime(
    unname(statuses), format = "%d.%m.%Y / %H:%M:%S",
    tz = "Europe/Berlin"
  ))
  if (anyNA(parsed)) {
    .stop_contract("demographic snapshot",
                   "component source statuses must be valid timestamps")
  }
  core <- !names(statuses) %in% independently_reviewed
  if (!any(core) ||
      length(unique(format(parsed[core], "%Y-%m-%d"))) != 1L ||
      as.numeric(difftime(max(parsed[core]), min(parsed[core]),
                          units = "mins")) > 15) {
    .stop_contract("demographic snapshot", paste(
      "typology-component source statuses must be on one calendar date and",
      "within a 15-minute reviewed build window"))
  }
  rule <- if (length(independently_reviewed)) {
    paste0(
      "typology_components_same_calendar_date_within_15_minutes;",
      paste(independently_reviewed, collapse = ","), "_independently_reviewed"
    )
  } else "same_calendar_date_within_15_minutes"
  list(
    rule = rule,
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
  } else if (identical(name, "annual_average_population")) {
    data <- data[, c("geo_id", "geo_name", "year", "population",
                     "population_basis"), drop = FALSE]
  } else {
    data <- data[, c("geo_id", "geo_name", "reference_date", "area_km2"),
                 drop = FALSE]
  }
  order_value <- if (identical(name, "annual_average_population")) {
    order(data$year, data$geo_id)
  } else order(data$reference_date, data$geo_id)
  data <- data[order_value, , drop = FALSE]
  rownames(data) <- NULL
  data
}

.validate_snapshot_components <- function(components) {
  needed <- c(
    "mean_age", "youth_dependency", "population", "area",
    "annual_average_population"
  )
  if (!is.list(components) || !identical(sort(names(components)), sort(needed))) {
    .stop_contract("demographic snapshot", paste(
      "components must be exactly", paste(needed, collapse = ", ")))
  }
  validate_demographic_indicator(components$mean_age$data)
  validate_demographic_indicator(components$youth_dependency$data)
  validate_population_denominator(components$population$data)
  validate_regional_area(components$area$data)
  validate_annual_average_population(components$annual_average_population$data)
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
  status_compatibility <- .validate_snapshot_statuses(
    statuses, "annual_average_population"
  )
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
                              sprintf("%d-12-31", 2022:2025)))
  if (!identical(dates, expected_dates)) {
    .stop_contract("demographic snapshot",
      "the reviewed snapshot must cover 2017-2020 and 2022-2025 exactly")
  }
  reporting_years <- sort(unique(compact$annual_average_population$year))
  if (!identical(reporting_years, 2022:2025)) {
    .stop_contract("demographic snapshot",
      "annual-average population must cover reporting years 2022-2025 exactly")
  }
  reference_geography <- paste(
    compact$population$geo_id[
      compact$population$reference_date == as.Date("2025-12-31")],
    compact$population$geo_name[
      compact$population$reference_date == as.Date("2025-12-31")],
    sep = "\r"
  )
  annual_geographies <- split(
    paste(compact$annual_average_population$geo_id,
          compact$annual_average_population$geo_name, sep = "\r"),
    compact$annual_average_population$year
  )
  if (any(vapply(annual_geographies, function(value) {
    !identical(sort(value), sort(reference_geography))
  }, logical(1L)))) {
    .stop_contract("demographic snapshot", paste(
      "annual-average population geography must match the reviewed 2025",
      "district geography for every reporting year"))
  }
  identity_provenance <- list(
    snapshot_version = snapshot_version,
    source = .demographic_snapshot_source,
    source_data_status = statuses,
    source_status_compatibility = status_compatibility$rule,
    source_status_display_date = status_compatibility$display_date,
    source_tables = c(
      mean_age = "12411-07-01-4", youth_dependency = "12411-08-01-4",
      population = "12411-01-01-4", area = "11111-01-01-4",
      annual_average_population = "12411-05-01-4"),
    source_measures = c(
      mean_age = "BEV519", youth_dependency = "BEV216",
      population = "Bev\u00f6lkerungsstand", area = "FLC006",
      annual_average_population = "BEV028"),
    covered_reference_dates = dates,
    covered_reporting_years = reporting_years,
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
      annual_average_population_counts = stats::setNames(vapply(
        reporting_years, function(year) {
          sum(compact$annual_average_population$year == year)
        }, integer(1L)), reporting_years),
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

.demographic_snapshot_average_population <- function(snapshot, years) {
  validate_demographic_snapshot(snapshot)
  years <- sort(unique(as.integer(years)))
  if (!length(years) || anyNA(years) ||
      !all(years %in% snapshot$provenance$covered_reporting_years)) {
    .stop_contract("demographic snapshot annual-average population",
                   "requested years are not covered by the snapshot")
  }
  compact <- snapshot$data$annual_average_population
  compact <- compact[compact$year %in% years, , drop = FALSE]
  retrieved_at <- snapshot$provenance$component_retrieved_at[[
    "annual_average_population"]]
  data <- data.frame(
    geo_id = compact$geo_id, geo_name = compact$geo_name,
    geo_level = "district", geo_vintage = as.Date(NA),
    year = compact$year, population = compact$population,
    population_measure = "annual_average_population",
    population_reference = "reporting_year_annual_average",
    population_basis = compact$population_basis,
    source = snapshot$provenance$source,
    source_table = "12411-05-01-4",
    retrieved_at = rep(retrieved_at, nrow(compact)),
    data_status = snapshot$provenance$source_data_status[[
      "annual_average_population"]],
    provenance_id = .regional_average_population_provenance_id,
    stringsAsFactors = FALSE
  )
  validate_annual_average_population(data)
  list(
    data = data,
    diagnostics = list(
      source = snapshot$provenance$source,
      source_table = "12411-05-01-4", statistic = "12411",
      source_measure = "BEV028", requested_years = years,
      returned_region_count = length(unique(data$geo_id)),
      returned_observation_count = nrow(data),
      observations_by_year = table(data$year),
      geo_vintage_unresolved = all(is.na(data$geo_vintage)),
      source_mode = "snapshot",
      snapshot_id = snapshot$provenance$snapshot_id
    ),
    provenance = snapshot$provenance$component_provenance[[
      "annual_average_population"]]
  )
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
  has_average <- "annual_average_population" %in% names(x$data)
  needed <- c("snapshot_id", "snapshot_version", "created_at", "source",
    "source_data_status", "source_status_compatibility",
    "source_status_display_date", "source_tables", "source_measures",
    "covered_reference_dates", "population_basis", "component_provenance",
    "component_retrieved_at", "builder", "builder_package_version",
    "content_checksum", "versioning_policy", "refresh_semantics")
  if (has_average) needed <- c(needed, "covered_reporting_years")
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
  components <- c("mean_age", "youth_dependency", "population", "area")
  tables <- c("12411-07-01-4", "12411-08-01-4", "12411-01-01-4",
              "11111-01-01-4")
  measures <- c("BEV519", "BEV216", "Bev\u00f6lkerungsstand", "FLC006")
  if (has_average) {
    components <- c(components, "annual_average_population")
    tables <- c(tables, "12411-05-01-4")
    measures <- c(measures, "BEV028")
  }
  if (length(x$provenance$created_at) != 1L ||
      !identical(names(x$provenance$source_data_status), components) ||
      !identical(unname(x$provenance$source_tables), tables) ||
      !identical(unname(x$provenance$source_measures), measures)) {
    .stop_contract(contract, "component source metadata is not the reviewed set")
  }
  if (!is.list(x$provenance$component_retrieved_at) ||
      !identical(names(x$provenance$component_retrieved_at), components) ||
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
  reporting_years <- if (has_average) {
    x$provenance$covered_reporting_years
  } else integer()
  if (has_average && (!is.integer(reporting_years) ||
      !identical(reporting_years, 2022:2025))) {
    .stop_contract(contract, "covered reporting years must be 2022-2025")
  }
  status_check <- .validate_snapshot_statuses(
    x$provenance$source_data_status,
    if (has_average) "annual_average_population" else character()
  )
  if (!identical(x$provenance$source_status_compatibility,
                 status_check$rule) ||
      !identical(x$provenance$source_status_display_date,
                 status_check$display_date)) {
    .stop_contract(contract, "source-status compatibility metadata disagrees")
  }
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
  if (has_average) expected_columns$annual_average_population <- c(
    "geo_id", "geo_name", "year", "population", "population_basis")
  for (name in components) {
    data <- x$data[[name]]
    .require_data_frame(data, contract)
    if (!identical(names(data), expected_columns[[name]])) {
      .stop_contract(contract, paste(name, "has unexpected columns"))
    }
    .check_character(data$geo_id, "geo_id", contract)
    .check_character(data$geo_name, "geo_name", contract)
    is_annual_average <- identical(name, "annual_average_population")
    if (is_annual_average) {
      .check_whole_number(data$year, "year", contract)
      key_period <- data$year
      covered <- data$year %in% reporting_years
    } else {
      .check_date(data$reference_date, "reference_date", contract)
      key_period <- data$reference_date
      covered <- data$reference_date %in% dates
    }
    if (any(!grepl("^[0-9]{5}$", data$geo_id)) || any(!covered) ||
        anyDuplicated(paste(data$geo_id, key_period))) {
      .stop_contract(contract, paste(name,
        "must have unique five-character IDs for every covered period"))
    }
    value <- if (name %in% c("population", "annual_average_population")) {
      data$population
    } else if (name == "area") data$area_km2 else data$indicator_value
    .check_numeric(value, paste0(name, " value"), contract, non_negative = TRUE)
    if (identical(name, "area") && any(value <= 0)) {
      .stop_contract(contract, "area values must be strictly positive")
    }
    if ("population_basis" %in% names(data)) {
      .check_character(data$population_basis, "population_basis", contract)
      year <- if (is_annual_average) data$year else
        as.integer(format(data$reference_date, "%Y"))
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
  if (has_average) required_diagnostics <- c(
    required_diagnostics, "annual_average_population_counts")
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
  keys <- lapply(x$data[c(
    "mean_age", "youth_dependency", "population", "area")], function(data) {
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
  if (has_average) {
    annual <- x$data$annual_average_population
    latest_reference_date <- max(dates)
    reference <- x$data$population[
      x$data$population$reference_date == latest_reference_date, ]
    reference_key <- sort(paste(reference$geo_id, reference$geo_name, sep = "\r"))
    annual_keys <- split(paste(annual$geo_id, annual$geo_name, sep = "\r"),
                         annual$year)
    if (any(vapply(annual_keys, function(value) {
      !identical(sort(value), reference_key)
    }, logical(1L))) ||
        !identical(unname(x$diagnostics$annual_average_population_counts),
                   rep(400L, 4L))) {
      .stop_contract(contract,
        paste("annual-average population geography must match the latest",
              "snapshot geography and year counts must be valid"))
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
  value <- get("regionalepi_demographic_snapshot_v3", envir = environment(),
               inherits = TRUE)
  validate_demographic_snapshot(value)
  value
}

#' Prepare a reviewed demographic snapshot for analysis
#'
#' Prepares selected years from the bundled demographic snapshot without a
#' network request. It reconstructs the demographic source tables, derives
#' population density, combines mean age, youth dependency ratio, and population
#' density, and calculates their unweighted period means. This function performs
#' no clustering.
#'
#' @param snapshot A validated snapshot returned by
#'   [regionalepi_demographic_snapshot()].
#' @param reference_years Explicit years covered by the reviewed snapshot.
#' @return A list with five top-level fields: `annual`, the combined annual
#'   demographic indicators; `summary`, the period-summary result accepted
#'   directly by [fit_dynamic_typology()]; `source`, the reconstructed
#'   population, area, mean-age, and youth-dependency source components;
#'   `source_mode`, which is `"snapshot"`; and `snapshot_provenance`, the
#'   reviewed snapshot provenance.
#' @examples
#' snapshot <- regionalepi_demographic_snapshot()
#'
#' demography <- prepare_demographic_snapshot(
#'   snapshot = snapshot,
#'   reference_years = 2022:2025
#' )
#'
#' head(demography$annual)
#' head(demography$summary$data)
#' demography$snapshot_provenance$snapshot_id
#'
#' fit <- fit_dynamic_typology(demography$summary, k = 3L)
#' head(fit$assignments)
#' fit$profiles
#' @export
prepare_demographic_snapshot <- function(snapshot, reference_years) {
  .demographic_snapshot_period(snapshot, reference_years)
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
