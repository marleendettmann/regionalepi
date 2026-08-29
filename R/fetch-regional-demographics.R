.regional_base_url <- "https://www.regionalstatistik.de/genesisws/rest/2020/"
.regional_source <- "Regionaldatenbank Deutschland"
.regional_table_transport <- NULL

.validate_regional_reference_request <- function(
    reference_dates, regions, first_year, last_year, contract) {
  if (!inherits(reference_dates, "Date") || !length(reference_dates) ||
      anyNA(reference_dates) || anyDuplicated(reference_dates)) {
    .stop_contract(contract, "`reference_dates` must be unique, complete Date values")
  }
  if (any(format(reference_dates, "%m-%d") != "12-31")) {
    .stop_contract(contract, "every reference date must be 31 December")
  }
  years <- as.integer(format(reference_dates, "%Y"))
  if (any(years < first_year | years > last_year)) {
    .stop_contract(contract, sprintf("reference dates must be within %d-%d", first_year, last_year))
  }
  if (!is.null(regions) &&
      (!is.character(regions) || !length(regions) || anyNA(regions) ||
       any(!grepl("^[0-9]{5}$", regions)) || anyDuplicated(regions))) {
    .stop_contract(contract, "`regions` must be unique five-character district identifiers")
  }
  list(reference_dates = sort(reference_dates), regions = regions)
}

.regional_credentials <- function() {
  username <- Sys.getenv("REGIONALSTATISTIK_USER", unset = "")
  password <- Sys.getenv("REGIONALSTATISTIK_PASSWORD", unset = "")
  if (!nzchar(username) || !nzchar(password)) {
    .stop_contract("Regionaldatenbank authentication",
                   "required environment variables are missing or empty")
  }
  list(username = username, password = password)
}

.regional_http_transport <- function(table, reference_dates, regions, credentials) {
  years <- as.integer(format(reference_dates, "%Y"))
  fields <- list(
    name = table, area = "all", compress = "false", transpose = "false",
    startyear = as.character(min(years)), endyear = as.character(max(years)),
    regionalvariable = "KREISE"
  )
  if (!is.null(regions)) fields$regionalkey <- paste(regions, collapse = ",")
  request <- httr2::request(paste0(.regional_base_url, "data/table"))
  request <- httr2::req_headers(request, username = credentials$username,
                                password = credentials$password)
  request <- do.call(httr2::req_body_form,
                     c(list(.req = request), fields, list(.multipart = FALSE)))
  httr2::resp_body_string(httr2::req_perform(request))
}

.perform_regional_table_request <- function(table, reference_dates, regions) {
  credentials <- .regional_credentials()
  transport <- .regional_table_transport
  if (is.null(transport)) transport <- .regional_http_transport
  result <- tryCatch(transport(table, reference_dates, regions, credentials),
                     error = function(error) error)
  if (inherits(result, "error")) {
    .stop_contract("Regionaldatenbank response",
                   "the official service request failed; no credentials are included")
  }
  result
}

.regional_table_object <- function(response, table, expected_codes, contract) {
  if (!is.character(response) || length(response) != 1L || is.na(response)) {
    .stop_contract(contract, "transport must return one JSON character response")
  }
  object <- tryCatch(jsonlite::fromJSON(response, simplifyVector = FALSE),
                     error = function(error) error)
  if (inherits(object, "error") || !is.list(object)) {
    .stop_contract(contract, "response is not valid JSON")
  }
  needed <- c("Ident", "Status", "Parameter", "Object", "Copyright")
  if (!all(needed %in% names(object)) ||
      !identical(as.integer(object$Status$Code), 0L) ||
      !identical(object$Ident$Service, "data") ||
      !identical(object$Ident$Method, "table") ||
      !identical(object$Parameter$name, table) ||
      !is.character(object$Object$Content) ||
      length(object$Object$Content) != 1L || !is.list(object$Object$Structure)) {
    .stop_contract(contract, "API envelope is not the expected successful table")
  }
  missing <- setdiff(expected_codes,
                     unique(.regional_structure_codes(object$Object$Structure)))
  if (length(missing)) {
    .stop_contract(contract, sprintf("structure metadata is missing: %s",
                                     paste(missing, collapse = ", ")))
  }
  object
}

.regional_content_metadata <- function(content, contract) {
  lines <- strsplit(content, "\r?\n", perl = TRUE)[[1L]]
  status <- grep("^Stand: ", trimws(lines), value = TRUE)
  if (length(status) != 1L) {
    .stop_contract(contract, "table content must contain one data status")
  }
  separator <- which(grepl("^_+$", trimws(lines)))
  copyright <- grep("^\"?\u00a9 ", trimws(lines))
  notes <- character()
  if (length(separator) == 1L && length(copyright) == 1L &&
      copyright > separator + 1L) {
    notes <- lines[seq.int(separator + 1L, copyright - 1L)]
    notes <- notes[nzchar(trimws(notes))]
  }
  list(lines = lines, data_status = sub("^Stand: ", "", status),
       source_notes = if (length(notes)) paste(notes, collapse = "\n") else character())
}

.regional_decimal <- function(x, field, contract) {
  clean <- trimws(x)
  if (any(!grepl("^[0-9]+(?:,[0-9]+)?$", clean))) {
    .stop_contract(contract,
                   sprintf("%s contains missing, structural, or quality-marked values", field))
  }
  value <- suppressWarnings(as.numeric(sub(",", ".", clean, fixed = TRUE)))
  if (anyNA(value) || any(!is.finite(value)) || any(value < 0)) {
    .stop_contract(contract, sprintf("%s must be finite and non-negative", field))
  }
  value
}

.regional_long_rows <- function(content, table, reference_dates, regions,
                                value_column, contract) {
  metadata <- .regional_content_metadata(content, contract)
  if (!identical(metadata$lines[[1L]], paste("Tabelle:", table))) {
    .stop_contract(contract, "table content has an unexpected identifier")
  }
  lines <- metadata$lines[grepl("^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4};", metadata$lines)]
  rows <- lapply(lines, function(line) strsplit(line, ";", fixed = TRUE)[[1L]])
  if (!length(rows) || any(lengths(rows) < value_column)) {
    .stop_contract(contract, "table contains no valid district observations")
  }
  all_ids <- vapply(rows, `[[`, character(1L), 2L)
  district <- grepl("^[0-9]{5}$", all_ids) | all_ids %in% c("02", "11")
  if (any(!district & !grepl("^(DG|[0-9]{2,8})$", all_ids))) {
    .stop_contract(contract, "table contains an unexpected regional key")
  }
  rows <- rows[district]
  dates <- as.Date(vapply(rows, `[[`, character(1L), 1L), format = "%d.%m.%Y")
  ids <- vapply(rows, `[[`, character(1L), 2L)
  ids[ids == "02"] <- "02000"
  ids[ids == "11"] <- "11000"
  names <- trimws(vapply(rows, `[[`, character(1L), 3L))
  values <- trimws(vapply(rows, `[[`, character(1L), value_column))
  structural_absence <- values == "-"
  if (!is.null(regions) && any(structural_absence & ids %in% regions)) {
    absent <- unique(ids[structural_absence & ids %in% regions])
    .stop_contract(
      contract,
      sprintf("requested regions have no observation: %s", paste(absent, collapse = ", "))
    )
  }
  keep <- !structural_absence
  if (!is.null(regions)) keep <- keep & ids %in% regions
  rows <- rows[keep]
  dates <- dates[keep]
  ids <- ids[keep]
  names <- names[keep]
  if (anyNA(dates) || any(!dates %in% reference_dates) ||
      any(!grepl("^[0-9]{5}$", ids)) || any(!nzchar(names))) {
    .stop_contract(contract, "district dates, identifiers, or names are malformed")
  }
  if (!setequal(unique(dates), reference_dates)) {
    .stop_contract(contract, "one or more requested reference dates are absent")
  }
  if (!is.null(regions) && length(setdiff(regions, unique(ids)))) {
    .stop_contract(contract, "one or more requested regions are absent")
  }
  if (!is.null(regions)) {
    observed <- unique(paste(ids, format(dates), sep = "\r"))
    expected <- as.vector(outer(
      regions, format(reference_dates), paste, sep = "\r"
    ))
    if (!setequal(observed, expected)) {
      .stop_contract(contract, "requested region/date coverage is incomplete")
    }
  }
  list(rows = rows, dates = dates, ids = ids, names = names,
       data_status = metadata$data_status, source_notes = metadata$source_notes,
       source_quality_markers = if (any(structural_absence)) "-" else character())
}

.regional_population_basis <- function(reference_date) {
  ifelse(as.integer(format(reference_date, "%Y")) <= 2021L,
         "census_2011", "census_2022")
}

.new_source_indicator <- function(parsed, values, spec, provenance_id) {
  data <- data.frame(
    geo_id = parsed$ids, geo_name = parsed$names,
    geo_level = rep("district", length(values)),
    geo_vintage = rep(as.Date(NA), length(values)),
    reference_date = parsed$dates, indicator_id = spec$indicator_id,
    indicator_value = values, indicator_unit = spec$parameters$unit,
    definition_version = spec$definition_version,
    value_origin = "source_provided", source = .regional_source,
    population_basis = .regional_population_basis(parsed$dates),
    provenance_id = provenance_id, stringsAsFactors = FALSE
  )
  validate_demographic_indicator(data)
  data
}

.fetch_regional_source_indicator <- function(
    reference_dates, regions, table, measure, expected_codes, value_column,
    spec, provenance_id) {
  request <- .validate_regional_reference_request(
    reference_dates, regions, 2011L, 2025L,
    paste("Regionaldatenbank", spec$indicator_id, "request"))
  response <- .perform_regional_table_request(table, request$reference_dates,
                                               request$regions)
  retrieved_at <- as.POSIXct(Sys.time(), tz = "UTC")
  object <- .regional_table_object(response, table, expected_codes,
                                   "Regionaldatenbank indicator response")
  parsed <- .regional_long_rows(object$Object$Content, table,
                                request$reference_dates, request$regions,
                                value_column, "Regionaldatenbank indicator response")
  values <- .regional_decimal(vapply(parsed$rows, `[[`, character(1L), value_column),
                              spec$indicator_id, "Regionaldatenbank indicator response")
  data <- .new_source_indicator(parsed, values, spec, provenance_id)
  list(
    data = data,
    diagnostics = list(
      source = .regional_source, source_table = table, source_measure = measure,
      requested_reference_dates = request$reference_dates,
      returned_region_count = length(unique(data$geo_id)),
      returned_observation_count = nrow(data), duplicate_count = 0L,
      source_quality_markers = parsed$source_quality_markers,
      geo_vintage_unresolved = all(is.na(data$geo_vintage))),
    provenance = stats::setNames(list(list(
      source = .regional_source, statistic = "12411", source_table = table,
      source_measure = measure, selection = list(sex = "Insgesamt"),
      unit = spec$parameters$unit,
      reference_date_semantics = "stock at 31 December",
      population_basis = "2017-2021 observations use the Census 2011 basis",
      data_status = parsed$data_status, retrieved_at = retrieved_at,
      source_notes = parsed$source_notes, copyright = object$Copyright)), provenance_id))
}

#' Fetch source-provided district mean age
#'
#' Retrieves `BEV519`, sex `Insgesamt`, from Regionaldatenbank table
#' `12411-07-01-4`. Mean age is not reconstructed from age groups.
#'
#' @param reference_dates Unique 31 December dates from 2011 through 2025.
#' @param regions `NULL` or unique five-character district identifiers.
#' @return A list with `data`, `diagnostics`, and `provenance`.
#' @details Regionaldatenbank represents the district-equivalent city states
#'   Hamburg and Berlin with regional keys `02` and `11` in this table family;
#'   these are deterministically expanded to AGS `02000` and `11000`.
#' @export
fetch_regional_mean_age <- function(reference_dates, regions = NULL) {
  .fetch_regional_source_indicator(
    reference_dates, regions, "12411-07-01-4", "BEV519",
    c("12411", "KREISE", "BEV519", "GES"), 4L,
    dissertation_mean_age_spec(), "regional_mean_age_12411-07-01-4")
}

#' Fetch source-provided district youth dependency quotient
#'
#' Retrieves authoritative `BEV216`, sex `Insgesamt`, from Regionaldatenbank
#' table `12411-08-01-4`. It does not calculate the quotient from age groups.
#'
#' @param reference_dates Unique 31 December dates from 2011 through 2025.
#' @param regions `NULL` or unique five-character district identifiers.
#' @return A list with `data`, `diagnostics`, and `provenance`.
#' @details Regionaldatenbank city-state keys `02` and `11` are
#'   deterministically expanded to AGS `02000` and `11000`.
#' @export
fetch_regional_youth_dependency <- function(reference_dates, regions = NULL) {
  .fetch_regional_source_indicator(
    reference_dates, regions, "12411-08-01-4", "BEV216",
    c("12411", "KREISE", "BEV216", "BEV215", "GES"), 4L,
    dissertation_youth_dependency_ratio_spec(),
    "regional_youth_dependency_12411-08-01-4")
}

#' Fetch annual district area
#'
#' Retrieves `FLC006` from Regionaldatenbank table `11111-01-01-4` and maps
#' source unit `qkm` to square kilometres without conversion or rounding.
#'
#' @param reference_dates Unique 31 December dates from 1995 through 2024.
#' @param regions `NULL` or unique five-character district identifiers.
#' @return A list with `data`, `diagnostics`, and `provenance`.
#' @details Regionaldatenbank city-state keys `02` and `11` are
#'   deterministically expanded to AGS `02000` and `11000`.
#' @export
fetch_regional_area <- function(reference_dates, regions = NULL) {
  request <- .validate_regional_reference_request(
    reference_dates, regions, 1995L, 2024L, "Regionaldatenbank area request")
  table <- "11111-01-01-4"
  response <- .perform_regional_table_request(table, request$reference_dates,
                                               request$regions)
  retrieved_at <- as.POSIXct(Sys.time(), tz = "UTC")
  object <- .regional_table_object(response, table,
                                   c("11111", "KREISE", "FLC006"),
                                   "Regionaldatenbank area response")
  parsed <- .regional_long_rows(object$Object$Content, table,
                                request$reference_dates, request$regions, 4L,
                                "Regionaldatenbank area response")
  values <- .regional_decimal(vapply(parsed$rows, `[[`, character(1L), 4L),
                              "area", "Regionaldatenbank area response")
  provenance_id <- "regional_area_11111-01-01-4"
  data <- data.frame(
    geo_id = parsed$ids, geo_name = parsed$names, geo_level = "district",
    geo_vintage = as.Date(NA), reference_date = parsed$dates,
    area_km2 = values, source = .regional_source, source_table = table,
    source_measure = "FLC006", retrieved_at = retrieved_at,
    data_status = parsed$data_status, provenance_id = provenance_id,
    stringsAsFactors = FALSE)
  validate_regional_area(data)
  list(
    data = data,
    diagnostics = list(
      source = .regional_source, source_table = table, source_measure = "FLC006",
      requested_reference_dates = request$reference_dates,
      returned_region_count = length(unique(data$geo_id)),
      returned_observation_count = nrow(data),
      source_quality_markers = parsed$source_quality_markers,
      geo_vintage_unresolved = all(is.na(data$geo_vintage))),
    provenance = stats::setNames(list(list(
      source = .regional_source, statistic = "11111", source_table = table,
      source_measure = "FLC006", source_unit = "qkm", internal_unit = "km2",
      reference_date_semantics = "territorial area at 31 December",
      data_status = parsed$data_status, retrieved_at = retrieved_at,
      source_notes = parsed$source_notes, copyright = object$Copyright)), provenance_id))
}

.regional_age_labels <- function(labels, contract) {
  from <- to <- rep(NA_integer_, length(labels))
  under <- labels == "unter 5 Jahre"; from[under] <- 0L; to[under] <- 4L
  grouped <- grepl("^[0-9]+ bis unter [0-9]+ Jahre$", labels)
  if (any(grouped)) {
    pieces <- regmatches(labels[grouped],
                         regexec("^([0-9]+) bis unter ([0-9]+) Jahre$", labels[grouped]))
    from[grouped] <- as.integer(vapply(pieces, `[[`, character(1L), 2L))
    to[grouped] <- as.integer(vapply(pieces, `[[`, character(1L), 3L)) - 1L
  }
  oldest <- grepl("^[0-9]+ Jahre und mehr$", labels)
  from[oldest] <- as.integer(sub(" Jahre und mehr$", "", labels[oldest]))
  if (anyNA(from) || any(is.na(to) & !oldest)) {
    .stop_contract(contract, "age-group labels are not the approved intervals")
  }
  list(age_from = from, age_to = to)
}

.fetch_regional_age_population <- function(reference_dates, regions = NULL) {
  request <- .validate_regional_reference_request(
    reference_dates, regions, 2011L, 2025L,
    "Regionaldatenbank age-population request")
  table <- "12411-09-01-4"
  response <- .perform_regional_table_request(table, request$reference_dates,
                                               request$regions)
  retrieved_at <- as.POSIXct(Sys.time(), tz = "UTC")
  object <- .regional_table_object(response, table,
                                   c("12411", "KREISE", "ALTX05", "GES"),
                                   "Regionaldatenbank age-population response")
  parsed <- .regional_long_rows(object$Object$Content, table,
                                request$reference_dates, request$regions, 5L,
                                "Regionaldatenbank age-population response")
  labels <- vapply(parsed$rows, `[[`, character(1L), 4L)
  keep <- labels != "Insgesamt"
  intervals <- .regional_age_labels(labels[keep],
                                    "Regionaldatenbank age-population response")
  values <- .regional_decimal(vapply(parsed$rows[keep], `[[`, character(1L), 5L),
                              "age population",
                              "Regionaldatenbank age-population response")
  data <- data.frame(
    geo_id = parsed$ids[keep], geo_name = parsed$names[keep],
    geo_level = "district", geo_vintage = as.Date(NA),
    reference_date = parsed$dates[keep], age_from = intervals$age_from,
    age_to = intervals$age_to, population = values,
    population_basis = .regional_population_basis(parsed$dates[keep]),
    source = .regional_source, sex = "total", retrieved_at = retrieved_at,
    source_table = table, source_measure = "population",
    data_status = parsed$data_status, stringsAsFactors = FALSE)
  validate_context_population(data)
  .validate_age_population_intervals(data)
  list(
    data = data,
    diagnostics = list(returned_region_count = length(unique(data$geo_id)),
                       returned_observation_count = nrow(data),
                       source_quality_markers = parsed$source_quality_markers,
                       coverage_0_64_complete = TRUE, geo_vintage_unresolved = TRUE),
    provenance = list(source = .regional_source, statistic = "12411",
                      source_table = table, age_variable = "ALTX05",
                      selection = list(sex = "Insgesamt"),
                      data_status = parsed$data_status, retrieved_at = retrieved_at,
                      source_notes = parsed$source_notes, copyright = object$Copyright))
}

.validate_age_population_intervals <- function(data) {
  interval_key <- paste(
    data$geo_id, format(data$reference_date), data$sex, data$age_from,
    ifelse(is.na(data$age_to), "open", data$age_to), sep = "\r"
  )
  if (anyDuplicated(interval_key)) {
    .stop_contract("context_population", "age intervals must be unique")
  }
  key <- paste(data$geo_id, format(data$reference_date), data$sex, sep = "\r")
  for (rows in split(seq_len(nrow(data)), key)) {
    group <- data[rows, , drop = FALSE]
    group <- group[order(group$age_from), , drop = FALSE]
    relevant <- group[group$age_from <= 64L, , drop = FALSE]
    if (!nrow(relevant) || relevant$age_from[[1L]] != 0L ||
        anyNA(relevant$age_to) || relevant$age_to[[nrow(relevant)]] != 64L ||
        any(relevant$age_from[-1L] != relevant$age_to[-nrow(relevant)] + 1L)) {
      .stop_contract("context_population",
                     "age intervals must cover 0 through 64 without gaps")
    }
  }
  invisible(data)
}
