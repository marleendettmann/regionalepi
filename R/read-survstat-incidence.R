#' Read a source-provided SurvStat incidence file
#'
#' Reads source-provided incidence from either of two explicitly supported
#' German-language SurvStat layouts: `kreis_rows` (Kreis rows, Meldewoche
#' columns, one caller-supplied year) or `reporting_year_rows` (Meldejahr rows,
#' Meldewoche columns, one caller-supplied filtered source geography). It does
#' not support arbitrary pivot layouts, parse Info PDFs, retrieve data, resolve
#' geography, or calculate incidence.
#'
#' Blank incidence cells become `NA_real_`; numeric zero remains zero. A blank
#' is never interpreted as a zero rate. Query metadata is supplied explicitly
#' by the caller and returned once in `provenance`, while observation rows carry
#' only the compact `query_id`. German-formatted non-negative values may use a
#' decimal comma and correctly grouped thousands points.
#'
#' @param file Path to one local SurvStat export file.
#' @param layout Exactly `"kreis_rows"` or `"reporting_year_rows"`.
#' @param pathogen One non-empty pathogen label.
#' @param reporting_years Complete whole-valued reporting years. Exactly one is
#'   required for `kreis_rows`; for `reporting_year_rows` the set must exactly
#'   match the year rows in the file.
#' @param query_id One non-empty identifier for this source query.
#' @param query_role One non-empty reviewed role used by an assembly
#'   specification, such as `"kreis_incidence"`.
#' @param source_geography `NULL` for `kreis_rows`. For
#'   `reporting_year_rows`, a named list with `geo_id` (character or `NA`),
#'   `geo_name`, and `geo_level = "survstat_bundesland"` describing the
#'   explicitly reviewed source geography supplied through the query filter.
#' @param geo_vintage Scalar source territorial vintage or `NA_Date_`.
#' @param retrieved_at Scalar query/retrieval time or `NA` `POSIXct`.
#' @param data_status Scalar SurvStat data-status time or `NA` `POSIXct`.
#' @param source_version One non-empty source/application version.
#' @param reference_definition Scalar logical query provenance or `NA`.
#' @param reporting_path Scalar character reporting-path provenance or `NA`.
#' @param epidemiological_filters Named list of additional scalar character
#'   epidemiological filter settings, excluding geography.
#' @param empty_rows_and_columns Scalar logical query setting or `NA`.
#' @param totals Scalar logical query setting or `NA`.
#' @return An ordinary list with `data`, `diagnostics`, and query-level
#'   `provenance`.
#' @export
read_survstat_incidence <- function(
    file, layout, pathogen, reporting_years, query_id, query_role,
    source_geography = NULL,
    geo_vintage = as.Date(NA),
    retrieved_at = as.POSIXct(NA),
    data_status = as.POSIXct(NA),
    source_version = "SurvStat@RKI 2.0",
    reference_definition = NA,
    reporting_path = NA_character_,
    epidemiological_filters = list(),
    empty_rows_and_columns = NA,
    totals = NA) {
  contract <- "SurvStat incidence import"
  .check_survstat_file(file, contract)
  query <- .survstat_incidence_query_spec(
    layout, pathogen, reporting_years, query_id, query_role, source_geography,
    geo_vintage, retrieved_at, data_status, source_version,
    reference_definition, reporting_path, epidemiological_filters,
    empty_rows_and_columns, totals, contract
  )
  table <- .read_survstat_table(file, contract)
  parsed <- if (identical(layout, "kreis_rows")) {
    .read_survstat_incidence_kreis(table, query, contract)
  } else {
    .read_survstat_incidence_years(table, query, contract)
  }
  validate_surveillance_incidence(parsed$data)
  query$reporting_week_coverage <- parsed$week_coverage
  list(
    data = parsed$data,
    diagnostics = c(
      list(
        source_file = basename(file), source = "SurvStat@RKI",
        source_version = source_version, query_id = query_id,
        measure = "incidence", value_semantics = "non_additive",
        encoding = "UTF-16LE", delimiter = "tab", layout = layout,
        physical_rows = nrow(table), physical_columns = ncol(table),
        blank_incidence_cells = sum(is.na(parsed$data$incidence)),
        numeric_zero_incidence_cells = sum(parsed$data$incidence == 0, na.rm = TRUE),
        period_total_column_present = TRUE
      ),
      parsed$diagnostics
    ),
    provenance = query
  )
}

.survstat_incidence_query_spec <- function(
    layout, pathogen, reporting_years, query_id, query_role, source_geography,
    geo_vintage, retrieved_at, data_status, source_version,
    reference_definition, reporting_path, epidemiological_filters,
    empty_rows_and_columns, totals, contract) {
  if (!is.character(layout) || length(layout) != 1L || is.na(layout) ||
      !layout %in% c("kreis_rows", "reporting_year_rows")) {
    .stop_contract(
      contract,
      "`layout` must be exactly \"kreis_rows\" or \"reporting_year_rows\""
    )
  }
  .check_scalar_nonempty_character(pathogen, "pathogen", contract)
  .check_scalar_nonempty_character(query_id, "query_id", contract)
  .check_scalar_nonempty_character(query_role, "query_role", contract)
  .check_scalar_nonempty_character(source_version, "source_version", contract)
  .check_scalar_date_or_na(geo_vintage, "geo_vintage", contract)
  .check_scalar_posixct_or_na(retrieved_at, "retrieved_at", contract)
  .check_scalar_posixct_or_na(data_status, "data_status", contract)
  .check_scalar_logical_or_na(
    reference_definition, "reference_definition", contract
  )
  .check_scalar_character_or_na(reporting_path, "reporting_path", contract)
  .check_query_logical_or_na(
    empty_rows_and_columns, "empty_rows_and_columns", contract
  )
  .check_query_logical_or_na(totals, "totals", contract)
  if (!is.numeric(reporting_years) || !length(reporting_years) ||
      anyNA(reporting_years) || any(!is.finite(reporting_years)) ||
      any(reporting_years != floor(reporting_years)) ||
      any(reporting_years < 1 | reporting_years > 9998) ||
      anyDuplicated(reporting_years)) {
    .stop_contract(
      contract,
      "`reporting_years` must contain unique whole-valued years from 1 through 9998"
    )
  }
  reporting_years <- as.integer(reporting_years)
  if (identical(layout, "kreis_rows") && length(reporting_years) != 1L) {
    .stop_contract(contract, "`kreis_rows` requires exactly one reporting year")
  }
  source_geography <- .validate_incidence_source_geography(
    source_geography, layout, contract
  )
  filters <- .validate_incidence_filters(epidemiological_filters, contract)
  list(
    query_id = query_id,
    query_role = query_role,
    source = "SurvStat@RKI",
    source_version = source_version,
    measure = "incidence",
    value_semantics = "non_additive",
    pathogen = pathogen,
    reporting_years = sort(reporting_years),
    time_unit = "week",
    source_geography_dimension = if (identical(layout, "kreis_rows")) {
      "survstat_kreis"
    } else {
      source_geography$geo_level
    },
    source_geography_filter = source_geography,
    row_dimension = if (identical(layout, "kreis_rows")) "Kreis" else "Meldejahr",
    column_dimension = "Meldewoche",
    reference_definition = reference_definition,
    reporting_path = reporting_path,
    epidemiological_filters = filters,
    empty_rows_and_columns = empty_rows_and_columns,
    totals = totals,
    retrieved_at = retrieved_at,
    data_status = data_status,
    geo_vintage = geo_vintage
  )
}

.check_query_logical_or_na <- function(x, field, contract) {
  if (!is.logical(x) || length(x) != 1L) {
    .stop_contract(contract, sprintf("`%s` must be one logical value or NA", field))
  }
}

.validate_incidence_source_geography <- function(x, layout, contract) {
  if (identical(layout, "kreis_rows")) {
    if (!is.null(x)) {
      .stop_contract(contract, "`source_geography` must be NULL for `kreis_rows`")
    }
    return(NULL)
  }
  .require_named_list(x, c("geo_id", "geo_name", "geo_level"), contract)
  if (!identical(sort(names(x)), sort(c("geo_id", "geo_name", "geo_level")))) {
    .stop_contract(
      contract,
      "`source_geography` must contain only `geo_id`, `geo_name`, and `geo_level`"
    )
  }
  if (!is.character(x$geo_id) || length(x$geo_id) != 1L ||
      (!is.na(x$geo_id) && !nzchar(x$geo_id))) {
    .stop_contract(contract, "filtered source `geo_id` must be character or NA")
  }
  .check_scalar_nonempty_character(x$geo_name, "source_geography$geo_name", contract)
  if (!identical(x$geo_level, "survstat_bundesland")) {
    .stop_contract(
      contract,
      "filtered source `geo_level` must be exactly \"survstat_bundesland\""
    )
  }
  x
}

.validate_incidence_filters <- function(x, contract) {
  if (!is.list(x) || is.data.frame(x) ||
      (length(x) && (is.null(names(x)) || any(!nzchar(names(x))))) ||
      anyDuplicated(names(x))) {
    .stop_contract(
      contract,
      "`epidemiological_filters` must be a uniquely named list"
    )
  }
  if (length(x)) {
    valid <- vapply(x, function(value) {
      is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
    }, logical(1L))
    if (any(!valid)) {
      .stop_contract(
        contract,
        "every epidemiological filter must be one non-empty character value"
      )
    }
    x <- x[order(names(x))]
  }
  x
}

.read_survstat_incidence_kreis <- function(table, query, contract) {
  structure <- .validate_survstat_structure(table, contract)
  year <- query$reporting_years[[1L]]
  weeks <- structure$weeks
  dates <- .iso_week_monday(year, weeks, contract)
  cells <- as.matrix(
    table[structure$geography_rows, structure$weekly_columns, drop = FALSE]
  )
  incidence <- .parse_survstat_incidence(cells, "geographic weekly cells", contract)
  dim(incidence) <- dim(cells)
  names <- table[[1L]][structure$geography_rows]
  observations <- length(names) * length(weeks)
  data <- .new_survstat_incidence_data(
    geo_id = rep(NA_character_, observations),
    geo_name = rep(names, each = length(weeks)),
    geo_level = rep("survstat_kreis", observations),
    geo_vintage = query$geo_vintage,
    dates = rep(dates, times = length(names)),
    query = query,
    reporting_year = rep(year, observations),
    reporting_week = rep(weeks, times = length(names)),
    incidence = as.numeric(t(incidence))
  )
  totals <- .parse_survstat_incidence(
    as.matrix(table[structure$national_row, structure$weekly_columns, drop = FALSE]),
    "presentation total weekly cells", contract
  )
  list(
    data = data,
    week_coverage = stats::setNames(list(weeks), as.character(year)),
    diagnostics = list(
      source_geographic_units = length(names),
      reporting_years = year,
      weeks_imported = stats::setNames(list(weeks), as.character(year)),
      national_total_row_present = TRUE,
      unknown_row_present = length(structure$unknown_row) == 1L,
      presentation_weekly_totals = totals,
      presentation_totals_not_reconciled = TRUE
    )
  )
}

.read_survstat_incidence_years <- function(table, query, contract) {
  structure <- .validate_survstat_year_structure(table, query, contract)
  rows <- lapply(seq_along(structure$years), function(index) {
    year <- structure$years[[index]]
    valid <- vapply(structure$weeks, function(week) {
      !inherits(try(.iso_week_monday(year, week, contract), silent = TRUE), "try-error")
    }, logical(1L))
    invalid_cells <- as.character(table[
      structure$year_rows[[index]], structure$weekly_columns[!valid], drop = TRUE
    ])
    if (length(invalid_cells) && any(!is.na(invalid_cells) & invalid_cells != "")) {
      .stop_contract(
        contract,
        sprintf("reporting year %d has a value in an inapplicable ISO week", year)
      )
    }
    weeks <- structure$weeks[valid]
    columns <- structure$weekly_columns[valid]
    cells <- as.matrix(table[structure$year_rows[[index]], columns, drop = FALSE])
    incidence <- .parse_survstat_incidence(cells, "year weekly cells", contract)
    dates <- .iso_week_monday(year, weeks, contract)
    geography <- query$source_geography_filter
    .new_survstat_incidence_data(
      geo_id = rep(geography$geo_id, length(weeks)),
      geo_name = rep(geography$geo_name, length(weeks)),
      geo_level = rep(geography$geo_level, length(weeks)),
      geo_vintage = query$geo_vintage,
      dates = dates,
      query = query,
      reporting_year = rep(year, length(weeks)),
      reporting_week = weeks,
      incidence = incidence
    )
  })
  data <- do.call(rbind, rows)
  rownames(data) <- NULL
  coverage <- stats::setNames(
    lapply(structure$years, function(year) {
      structure$weeks[vapply(structure$weeks, function(week) {
        !inherits(try(.iso_week_monday(year, week, contract), silent = TRUE), "try-error")
      }, logical(1L))]
    }),
    as.character(structure$years)
  )
  list(
    data = data,
    week_coverage = coverage,
    diagnostics = list(
      source_geographic_units = 1L,
      reporting_years = structure$years,
      weeks_imported = coverage,
      national_total_row_present = TRUE,
      unknown_row_present = FALSE,
      presentation_period_totals = structure$period_totals,
      inapplicable_week_columns_omitted = sum(lengths(coverage)) <
        length(structure$years) * length(structure$weeks),
      presentation_totals_not_reconciled = TRUE
    )
  )
}

.validate_survstat_year_structure <- function(x, query, contract) {
  if (nrow(x) < 4L || ncol(x) < 3L) {
    .stop_contract(contract, "year-row table must contain two headers, total, and years")
  }
  first <- unname(unlist(x[1L, 1:2, drop = FALSE], use.names = FALSE))
  if (!identical(first, c("Meldejahr", "Meldewoche"))) {
    .stop_contract(
      contract,
      "first header row must begin with `Meldejahr` and `Meldewoche`"
    )
  }
  if (any(unname(unlist(x[1L, -(1:2), drop = FALSE], use.names = FALSE)) != "")) {
    .stop_contract(contract, "first header row contains unexpected trailing values")
  }
  second <- unname(unlist(x[2L, , drop = FALSE], use.names = FALSE))
  if (second[[1L]] != "" || second[[2L]] != "Gesamt") {
    .stop_contract(contract, "second header row must begin with blank and `Gesamt`")
  }
  week_labels <- second[-(1:2)]
  if (!length(week_labels) || any(!grepl("^[0-9]{2}$", week_labels)) ||
      anyDuplicated(week_labels)) {
    .stop_contract(contract, "weekly headers must be unique two-digit labels")
  }
  weeks <- as.integer(week_labels)
  if (any(weeks < 1L | weeks > 53L)) {
    .stop_contract(contract, "weekly headers contain an invalid ISO week number")
  }
  labels <- x[[1L]][-(1:2)]
  if (!identical(labels[[1L]], "Gesamt") || sum(labels == "Gesamt") != 1L) {
    .stop_contract(contract, "exactly one `Gesamt` row must follow the headers")
  }
  year_labels <- labels[-1L]
  if (!length(year_labels) || any(!grepl("^[0-9]{4}$", year_labels)) ||
      anyDuplicated(year_labels)) {
    .stop_contract(contract, "data rows must have unique four-digit reporting years")
  }
  years <- as.integer(year_labels)
  if (!setequal(years, query$reporting_years)) {
    .stop_contract(contract, "file years must exactly match `reporting_years`")
  }
  totals <- .parse_survstat_incidence(
    x[seq.int(4L, nrow(x)), 2L, drop = TRUE], "period totals", contract
  )
  stats::setNames(totals, year_labels)
  order <- order(years)
  list(
    years = years[order],
    year_rows = seq.int(4L, nrow(x))[order],
    weeks = weeks,
    weekly_columns = seq.int(3L, ncol(x)),
    period_totals = stats::setNames(totals, year_labels)[order]
  )
}

.parse_survstat_incidence <- function(x, field, contract) {
  values <- as.character(x)
  blank <- is.na(values) | values == ""
  valid <- blank | grepl(
    "^(?:[0-9]+|[0-9]{1,3}(?:\\.[0-9]{3})+)(?:,[0-9]+)?$", values
  )
  if (any(!valid)) {
    .stop_contract(
      contract,
      sprintf("%s contain a value outside the non-negative decimal-comma format", field)
    )
  }
  normalized <- gsub(".", "", values, fixed = TRUE)
  normalized <- sub(",", ".", normalized, fixed = TRUE)
  parsed <- suppressWarnings(as.numeric(normalized))
  parsed[blank] <- NA_real_
  if (any(!is.finite(parsed[!blank])) || any(parsed[!blank] < 0)) {
    .stop_contract(contract, sprintf("%s must be finite and non-negative", field))
  }
  parsed
}

.new_survstat_incidence_data <- function(
    geo_id, geo_name, geo_level, geo_vintage, dates, query,
    reporting_year, reporting_week, incidence) {
  observations <- length(incidence)
  vintage <- if (inherits(geo_vintage, "Date")) {
    geo_vintage
  } else {
    as.Date(NA)
  }
  data.frame(
    geo_id = geo_id,
    geo_name = geo_name,
    geo_level = geo_level,
    geo_vintage = rep(vintage, observations),
    date = dates,
    time_unit = rep("week", observations),
    pathogen = rep(query$pathogen, observations),
    incidence = as.numeric(incidence),
    source = rep(query$source, observations),
    retrieved_at = rep(query$retrieved_at, observations),
    source_version = rep(query$source_version, observations),
    reporting_year = as.integer(reporting_year),
    reporting_week = as.integer(reporting_week),
    data_status = rep(query$data_status, observations),
    reference_definition = rep(query$reference_definition, observations),
    reporting_path = rep(query$reporting_path, observations),
    query_id = rep(query$query_id, observations),
    stringsAsFactors = FALSE
  )
}
