#' Read a SurvStat case-count file
#'
#' Reads the supported German-language SurvStat tabular export format from a
#' local UTF-16LE, tab-delimited file. The adapter imports weekly case counts
#' only. It does not retrieve data, parse query PDFs, calculate incidence, or
#' resolve source geography names.
#'
#' Source geography labels are preserved verbatim. Because the export does not
#' supply canonical identifiers, `geo_id` is `NA_character_`; `geo_vintage` is
#' the caller-supplied value and may be `NA_Date_` before geographic resolution.
#'
#' `retrieved_at` is the time at which the export or query was retrieved or
#' executed. `data_status` is the source data status reported by SurvStat for
#' that query. These are distinct provenance concepts and neither is inferred
#' from the file.
#'
#' @param file Path to one local SurvStat export file.
#' @param pathogen One non-empty pathogen label supplied by the caller.
#' @param reporting_year Whole-valued ISO reporting year.
#' @param geo_vintage Scalar `Date` for the source geography vintage, or
#'   `NA_Date_` when it is not yet known.
#' @param retrieved_at Scalar `POSIXct` retrieval/execution time, or `NA`.
#' @param data_status Scalar `POSIXct` source data status, or `NA`.
#' @param source_version One non-empty source-version label.
#' @param reference_definition Scalar logical provenance: `TRUE` when a
#'   reference definition was selected, `FALSE` when it was not, and `NA` when
#'   not supplied or unknown.
#' @param reporting_path Scalar character reporting-path provenance, or
#'   `NA_character_`.
#' @param blank_is_zero Whether blank geographic weekly cells are explicitly
#'   interpreted as zero. If `FALSE`, such cells are errors.
#' @return An ordinary list with elements `data`, containing validated canonical
#'   surveillance data, and `diagnostics`, containing import diagnostics.
#' @export
read_survstat <- function(
    file,
    pathogen,
    reporting_year,
    geo_vintage = as.Date(NA),
    retrieved_at = as.POSIXct(NA),
    data_status = as.POSIXct(NA),
    source_version = "SurvStat@RKI 2.0",
    reference_definition = NA,
    reporting_path = NA_character_,
    blank_is_zero = TRUE) {
  contract <- "SurvStat import"
  .check_survstat_file(file, contract)
  .check_scalar_nonempty_character(pathogen, "pathogen", contract)
  .check_reporting_year(reporting_year, contract)
  .check_scalar_date_or_na(geo_vintage, "geo_vintage", contract)
  .check_scalar_posixct_or_na(retrieved_at, "retrieved_at", contract)
  .check_scalar_posixct_or_na(data_status, "data_status", contract)
  .check_scalar_nonempty_character(source_version, "source_version", contract)
  .check_scalar_logical_or_na(
    reference_definition, "reference_definition", contract
  )
  .check_scalar_character_or_na(reporting_path, "reporting_path", contract)
  if (!is.logical(blank_is_zero) || length(blank_is_zero) != 1L ||
      is.na(blank_is_zero)) {
    .stop_contract(contract, "`blank_is_zero` must be one non-missing logical value")
  }

  table <- .read_survstat_table(file, contract)
  structure <- .validate_survstat_structure(table, contract)
  weeks <- structure$weeks
  dates <- .iso_week_monday(reporting_year, weeks, contract)
  geography_rows <- structure$geography_rows
  weekly_columns <- structure$weekly_columns
  geographic_cells <- as.matrix(table[geography_rows, weekly_columns, drop = FALSE])
  blank_cells <- is.na(geographic_cells) | geographic_cells == ""
  blank_count <- sum(blank_cells)
  if (blank_count && !blank_is_zero) {
    .stop_contract(
      contract,
      sprintf("found %d blank geographic weekly cell(s) while `blank_is_zero` is FALSE",
              blank_count)
    )
  }
  if (blank_is_zero) {
    geographic_cells[blank_cells] <- "0"
  }
  geographic_counts <- .parse_survstat_counts(
    geographic_cells, "geographic weekly cells", contract, allow_blank = FALSE
  )
  dim(geographic_counts) <- dim(geographic_cells)

  national_cells <- as.matrix(
    table[structure$national_row, weekly_columns, drop = FALSE]
  )
  national_counts <- .parse_survstat_counts(
    national_cells, "national weekly totals", contract, allow_blank = FALSE
  )
  imported_totals <- if (nrow(geographic_counts)) {
    colSums(geographic_counts)
  } else {
    numeric(length(weeks))
  }
  reconciled <- imported_totals == national_counts
  if (any(!reconciled)) {
    failed <- sprintf(
      "W%02d (declared %s, imported %s)",
      weeks[!reconciled], national_counts[!reconciled], imported_totals[!reconciled]
    )
    .stop_contract(
      contract,
      sprintf("national weekly total mismatch: %s", paste(failed, collapse = "; "))
    )
  }

  geo_names <- table[[1L]][geography_rows]
  observations <- length(geo_names) * length(weeks)
  data <- data.frame(
    geo_id = rep(NA_character_, observations),
    geo_name = rep(geo_names, each = length(weeks)),
    geo_level = rep("survstat_kreis", observations),
    geo_vintage = rep(geo_vintage, observations),
    date = rep(dates, times = length(geo_names)),
    time_unit = rep("week", observations),
    pathogen = rep(pathogen, observations),
    cases = as.numeric(t(geographic_counts)),
    source = rep("SurvStat@RKI", observations),
    retrieved_at = rep(retrieved_at, observations),
    source_version = rep(source_version, observations),
    reporting_year = rep(as.integer(reporting_year), observations),
    reporting_week = rep(weeks, times = length(geo_names)),
    data_status = rep(data_status, observations),
    reference_definition = rep(reference_definition, observations),
    reporting_path = rep(reporting_path, observations),
    stringsAsFactors = FALSE
  )
  validate_surveillance(data)

  diagnostics <- list(
    source_file = basename(file),
    source = "SurvStat@RKI",
    source_version = source_version,
    encoding = "UTF-16LE",
    delimiter = "tab",
    reporting_year = as.integer(reporting_year),
    weeks_imported = weeks,
    source_geographic_units = length(geo_names),
    observation_rows = observations,
    blank_is_zero = blank_is_zero,
    blank_observation_cells = blank_count,
    blanks_converted_to_zero = if (blank_is_zero) blank_count else 0L,
    period_total_column_present = TRUE,
    national_total_row_present = TRUE,
    unknown_row_present = length(structure$unknown_row) == 1L,
    national_totals_reconciled = TRUE,
    national_weekly_totals = data.frame(
      reporting_week = weeks,
      declared_cases = as.numeric(national_counts),
      imported_cases = as.numeric(imported_totals),
      reconciled = reconciled
    ),
    geographic_ids_unresolved = observations > 0L,
    geographic_vintage_unresolved = is.na(geo_vintage)
  )
  list(data = data, diagnostics = diagnostics)
}

.check_survstat_file <- function(file, contract) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    .stop_contract(contract, "`file` must be one non-empty character path")
  }
  if (!file.exists(file) || dir.exists(file)) {
    .stop_contract(contract, "`file` must identify an existing regular file")
  }
}

.check_scalar_nonempty_character <- function(x, field, contract) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .stop_contract(contract, sprintf("`%s` must be one non-empty character value", field))
  }
}

.check_scalar_character_or_na <- function(x, field, contract) {
  if (!is.character(x) || length(x) != 1L || (!is.na(x) && !nzchar(x))) {
    .stop_contract(contract, sprintf("`%s` must be one character value or NA", field))
  }
}

.check_scalar_logical_or_na <- function(x, field, contract) {
  if (!is.logical(x) || length(x) != 1L) {
    .stop_contract(contract, sprintf("`%s` must be one logical value or NA", field))
  }
}

.check_scalar_date_or_na <- function(x, field, contract) {
  if (!inherits(x, "Date") || length(x) != 1L) {
    .stop_contract(contract, sprintf("`%s` must be one Date value or NA_Date_", field))
  }
}

.check_scalar_posixct_or_na <- function(x, field, contract) {
  if (!inherits(x, "POSIXct") || length(x) != 1L) {
    .stop_contract(contract, sprintf("`%s` must be one POSIXct value or NA", field))
  }
}

.check_reporting_year <- function(x, contract) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < 1 || x > 9998) {
    .stop_contract(
      contract,
      "`reporting_year` must be one whole-valued year from 1 through 9998"
    )
  }
}

.read_survstat_table <- function(file, contract) {
  size <- file.info(file)$size
  if (is.na(size) || size < 2L) {
    .stop_contract(contract, "file is empty or too short to contain a UTF-16LE BOM")
  }
  connection <- file(file, open = "rb")
  on.exit(close(connection), add = TRUE)
  bytes <- readBin(connection, what = "raw", n = size)
  if (length(bytes) < 2L || !identical(bytes[1:2], as.raw(c(0xff, 0xfe)))) {
    .stop_contract(contract, "file must have a UTF-16LE BOM")
  }
  decoded <- iconv(list(bytes[-c(1L, 2L)]), from = "UTF-16LE", to = "UTF-8")[[1L]]
  if (is.na(decoded)) {
    .stop_contract(contract, "file could not be decoded as UTF-16LE")
  }
  parsed <- tryCatch(
    utils::read.delim(
      text = decoded, header = FALSE, sep = "\t", quote = "\"",
      fill = TRUE, colClasses = "character", check.names = FALSE,
      comment.char = "", blank.lines.skip = FALSE, na.strings = NULL,
      stringsAsFactors = FALSE
    ),
    error = function(error) error
  )
  if (inherits(parsed, "error")) {
    .stop_contract(contract, sprintf("could not parse tabular content: %s", parsed$message))
  }
  parsed[is.na(parsed)] <- ""
  parsed
}

.validate_survstat_structure <- function(x, contract) {
  if (nrow(x) < 3L || ncol(x) < 3L) {
    .stop_contract(contract, "table must contain two header rows and weekly data")
  }
  if (!identical(x[1L, 1:2], data.frame(V1 = "Kreis", V2 = "Meldewoche"))) {
    if (!identical(unname(unlist(x[1L, 1:2], use.names = FALSE)),
                   c("Kreis", "Meldewoche"))) {
      .stop_contract(contract, "first header row must begin with `Kreis` and `Meldewoche`")
    }
  }
  first_tail <- unname(unlist(x[1L, -(1:2), drop = FALSE], use.names = FALSE))
  if (any(first_tail != "")) {
    .stop_contract(contract, "first header row contains unexpected trailing values")
  }
  second <- unname(unlist(x[2L, , drop = FALSE], use.names = FALSE))
  if (second[1L] != "" || second[2L] != "Gesamt") {
    .stop_contract(contract, "second header row must begin with a blank cell and `Gesamt`")
  }
  week_labels <- second[-(1:2)]
  if (!length(week_labels) || any(!grepl("^[0-9]{2}$", week_labels))) {
    .stop_contract(contract, "weekly headers must be unique two-digit ISO week labels")
  }
  if (anyDuplicated(week_labels)) {
    .stop_contract(contract, "weekly headers must be unique two-digit ISO week labels")
  }
  weeks <- as.integer(week_labels)
  labels <- x[[1L]][-(1:2)]
  national <- which(labels == "Gesamt") + 2L
  unknown <- which(labels == "Unbekannt") + 2L
  if (length(national) != 1L) {
    .stop_contract(contract, "table must contain exactly one national `Gesamt` row")
  }
  if (national != 3L) {
    .stop_contract(contract, "national `Gesamt` row must immediately follow the headers")
  }
  if (length(unknown) > 1L) {
    .stop_contract(contract, "table must contain at most one `Unbekannt` row")
  }
  if (length(unknown) && unknown != nrow(x)) {
    .stop_contract(contract, "`Unbekannt` row must be the final row")
  }
  geography <- setdiff(seq.int(3L, nrow(x)), c(national, unknown))
  geography_labels <- x[[1L]][geography]
  if (any(is.na(geography_labels) | geography_labels == "")) {
    .stop_contract(contract, "source geography labels must not be blank")
  }
  if (anyDuplicated(geography_labels)) {
    .stop_contract(contract, "source geography labels must be unique")
  }
  list(
    weeks = weeks,
    weekly_columns = seq.int(3L, ncol(x)),
    national_row = national,
    unknown_row = unknown,
    geography_rows = geography
  )
}

.parse_survstat_counts <- function(x, field, contract, allow_blank) {
  values <- as.character(x)
  blank <- is.na(values) | values == ""
  if (!allow_blank && any(blank)) {
    .stop_contract(contract, sprintf("%s must not contain blank values", field))
  }
  parsed <- suppressWarnings(as.numeric(values))
  failed <- !blank & is.na(parsed) & !values %in% c("NA", "NaN")
  if (any(failed) || any(values == "NA")) {
    .stop_contract(contract, sprintf("%s contain an unexpected non-numeric value", field))
  }
  if (any(!is.finite(parsed[!blank]))) {
    .stop_contract(contract, sprintf("%s must contain only finite values", field))
  }
  if (any(parsed[!blank] < 0)) {
    .stop_contract(contract, sprintf("%s must be non-negative", field))
  }
  if (any(parsed[!blank] != floor(parsed[!blank]))) {
    .stop_contract(contract, sprintf("%s must be whole-valued case counts", field))
  }
  parsed
}

.iso_week_monday <- function(year, week, contract) {
  if (any(is.na(week)) || any(week < 1L) || any(week > 53L)) {
    .stop_contract(contract, "weekly headers contain an invalid ISO week number")
  }
  week_one <- .iso_week_one_monday(year)
  next_week_one <- .iso_week_one_monday(year + 1)
  dates <- week_one + 7 * (week - 1L)
  invalid <- dates >= next_week_one
  if (any(invalid)) {
    .stop_contract(
      contract,
      sprintf("invalid ISO week(s) for reporting year %d: %s",
              year, paste(sprintf("%02d", week[invalid]), collapse = ", "))
    )
  }
  dates
}

.iso_week_one_monday <- function(year) {
  january_fourth <- as.Date(sprintf("%04d-01-04", as.integer(year)))
  monday_offset <- (as.POSIXlt(january_fourth, tz = "UTC")$wday + 6L) %% 7L
  january_fourth - monday_offset
}
