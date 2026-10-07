.regional_population_base_url <-
  "https://www.regionalstatistik.de/genesisws/rest/2020/"
.regional_population_table <- "12411-01-01-4"
.regional_population_statistic <- "12411"
.regional_population_source <- "Regionaldatenbank Deutschland"
.regional_population_transport <- NULL

#' Fetch district total population from Regionaldatenbank Deutschland
#'
#' Retrieves reviewed total-population observations from Regionaldatenbank
#' table `12411-01-01-4` for 31 December reference dates from 2017 through
#' 2025. The public function is deliberately narrow and is not a generic
#' GENESIS client.
#'
#' Authentication is read internally from the `REGIONALSTATISTIK_USER` and
#' `REGIONALSTATISTIK_PASSWORD` environment variables. Credentials are never
#' returned or stored by the package, and the package does not parse `.env`
#' files.
#'
#' Regional identifiers and names come directly from the source response.
#' Identifiers are preserved as character values. The source observation date
#' does not establish a canonical territorial vintage, so `geo_vintage` remains
#' `NA_Date_`.
#' Regionaldatenbank represents the district-equivalent city states Hamburg and
#' Berlin with regional keys `02` and `11` in this response; these are
#' deterministically expanded to AGS `02000` and `11000`.
#'
#' @section Data source and license:
#' Data are obtained from Regionaldatenbank Deutschland, provided by the
#' Statistische Ämter des Bundes und der Länder, under Datenlizenz Deutschland
#' – Namensnennung – Version 2.0 (`dl-de/by-2-0`). regionalepi selects and
#' validates the reviewed source table and measure. Returned provenance retains
#' the source table, retrieval time, source data status, and available source
#' notes. Selection and normalization do not relicense the source data; GPL-3
#' applies to regionalepi package code.
#' @param reference_dates A non-empty `Date` vector containing unique
#'   31 December dates from 2017 through 2025.
#' @param regions `NULL` to return all supplied district observations, or a
#'   non-empty character vector of unique five-character district AGS values.
#' @return An ordinary list with validated `data`, compact `diagnostics`, and
#'   dataset-level `provenance`.
#' @references [Regionaldatenbank Deutschland](https://www.regionalstatistik.de/),
#'   [Datenlizenz Deutschland – Namensnennung – Version 2.0](https://www.govdata.de/dl-de/by-2-0)
#' @export
fetch_regional_population <- function(reference_dates, regions = NULL) {
  request <- .validate_regional_population_request(reference_dates, regions)
  credentials <- .regional_population_credentials()
  response <- .perform_regional_population_request(
    reference_dates = request$reference_dates,
    regions = request$regions,
    credentials = credentials
  )
  retrieved_at <- as.POSIXct(Sys.time(), tz = "UTC")
  .regional_population_result(
    response,
    reference_dates = request$reference_dates,
    regions = request$regions,
    retrieved_at = retrieved_at
  )
}

.validate_regional_population_request <- function(reference_dates, regions) {
  contract <- "Regionaldatenbank population request"
  if (!inherits(reference_dates, "Date") || !length(reference_dates) ||
      anyNA(reference_dates)) {
    .stop_contract(contract, "`reference_dates` must be a non-empty complete Date vector")
  }
  if (anyDuplicated(reference_dates)) {
    .stop_contract(contract, "`reference_dates` must not contain duplicates")
  }
  dates_text <- format(reference_dates, "%Y-%m-%d")
  if (any(substr(dates_text, 6L, 10L) != "12-31")) {
    .stop_contract(contract, "every `reference_dates` value must be 31 December")
  }
  years <- as.integer(format(reference_dates, "%Y"))
  if (any(years < 2017L | years > 2025L)) {
    .stop_contract(contract, "`reference_dates` must be within the reviewed 2017-2025 scope")
  }
  if (!is.null(regions)) {
    if (!is.character(regions) || !length(regions) || anyNA(regions) ||
        any(!nzchar(regions))) {
      .stop_contract(contract, "`regions` must be NULL or a non-empty complete character vector")
    }
    if (any(!grepl("^[0-9]{5}$", regions))) {
      .stop_contract(contract, "`regions` must contain five-character district identifiers")
    }
    if (anyDuplicated(regions)) {
      .stop_contract(contract, "`regions` must not contain duplicates")
    }
  }
  list(reference_dates = reference_dates, regions = regions)
}

.regional_population_credentials <- function() {
  .regional_credentials()
}

.perform_regional_population_request <- function(reference_dates, regions,
                                                   credentials) {
  transport <- .regional_population_transport
  if (is.null(transport)) {
    transport <- .regional_population_http_transport
  }
  result <- tryCatch(
    transport(
      reference_dates = reference_dates,
      regions = regions,
      credentials = credentials
    ),
    error = function(error) error
  )
  if (inherits(result, "error")) {
    .stop_contract(
      "Regionaldatenbank response",
      "the official service request failed; no credentials are included"
    )
  }
  result
}

.regional_population_http_transport <- function(reference_dates, regions,
                                                  credentials) {
  .regional_http_transport(
    .regional_population_table, reference_dates, regions, credentials
  )
}

.regional_population_result <- function(response, reference_dates, regions,
                                         retrieved_at) {
  contract <- "Regionaldatenbank response"
  if (!is.character(response) || length(response) != 1L || is.na(response)) {
    .stop_contract(contract, "transport must return one JSON character response")
  }
  object <- tryCatch(
    jsonlite::fromJSON(response, simplifyVector = FALSE),
    error = function(error) error
  )
  if (inherits(object, "error") || !is.list(object)) {
    .stop_contract(contract, "response is not valid JSON with the expected object form")
  }
  .validate_regional_population_envelope(object, contract)
  parsed <- .parse_regional_population_content(
    object$Object$Content, reference_dates, regions, contract
  )
  quality_markers <- attr(parsed, "quality_markers", exact = TRUE)
  data_status <- attr(parsed, "data_status", exact = TRUE)
  source_notes <- attr(parsed, "source_notes", exact = TRUE)
  attr(parsed, "quality_markers") <- NULL
  attr(parsed, "data_status") <- NULL
  attr(parsed, "source_notes") <- NULL
  data <- data.frame(
    geo_id = parsed$geo_id,
    geo_name = parsed$geo_name,
    geo_level = rep("district", nrow(parsed)),
    geo_vintage = rep(as.Date(NA), nrow(parsed)),
    reference_date = parsed$reference_date,
    population = parsed$population,
    population_basis = ifelse(
      as.integer(format(parsed$reference_date, "%Y")) <= 2021L,
      "census_2011", "census_2022"
    ),
    source = rep(.regional_population_source, nrow(parsed)),
    source_table = rep(.regional_population_table, nrow(parsed)),
    retrieved_at = rep(retrieved_at, nrow(parsed)),
    data_status = rep(data_status, nrow(parsed)),
    stringsAsFactors = FALSE
  )
  validate_population_denominator(data)

  requested_region_count <- if (is.null(regions)) NA_integer_ else length(regions)
  diagnostics <- list(
    source = .regional_population_source,
    source_table = .regional_population_table,
    statistic = .regional_population_statistic,
    api_endpoint_family = .regional_population_base_url,
    requested_reference_dates = reference_dates,
    requested_region_count = requested_region_count,
    returned_region_count = length(unique(data$geo_id)),
    returned_observation_count = nrow(data),
    api_status_code = as.integer(object$Status$Code),
    api_status = as.character(object$Status$Content),
    data_status = data_status,
    population_basis_codes = sort(unique(data$population_basis)),
    geo_vintage_unresolved = all(is.na(data$geo_vintage)),
    source_quality_markers = quality_markers
  )
  provenance <- list(
    source = .regional_population_source,
    source_table = .regional_population_table,
    statistic = .regional_population_statistic,
    table_title = "Bev\u00f6lkerung nach Geschlecht - Stichtag 31.12. - regionale Tiefe: Kreise und kreisfreie St\u00e4dte",
    measure = "Bev\u00f6lkerungsstand",
    unit = "Anzahl",
    reference_date_semantics = "Population stock at 31 December",
    population_basis = c(
      census_2011 = "2017-2021 observations are based on the 2011 Census population basis.",
      census_2022 = "Observations from 2022 are based on the 2022 Census population basis."
    ),
    methodological_note = paste(
      "The source documents a break between the Census 2011 and Census 2022",
      "population bases; values across that break require methodological care."
    ),
    source_notes = source_notes,
    geo_vintage = "Not independently established; returned as NA_Date_.",
    retrieved_at = retrieved_at,
    copyright = object$Copyright
  )
  list(data = data, diagnostics = diagnostics, provenance = provenance)
}

.validate_regional_population_envelope <- function(object, contract) {
  needed <- c("Ident", "Status", "Parameter", "Object", "Copyright")
  if (!all(needed %in% names(object))) {
    .stop_contract(contract, "API envelope is missing required components")
  }
  if (!is.list(object$Status) || !identical(as.integer(object$Status$Code), 0L)) {
    .stop_contract(contract, "API status is not successful")
  }
  if (!is.list(object$Ident) || !identical(object$Ident$Service, "data") ||
      !identical(object$Ident$Method, "table")) {
    .stop_contract(contract, "API identity is not the expected data/table operation")
  }
  if (!is.list(object$Parameter) ||
      !identical(object$Parameter$name, .regional_population_table)) {
    .stop_contract(contract, "response is not for the expected source table")
  }
  if (!is.list(object$Object) || !is.character(object$Object$Content) ||
      length(object$Object$Content) != 1L || !is.list(object$Object$Structure)) {
    .stop_contract(contract, "Object must contain table Content and Structure metadata")
  }
  codes <- unique(.regional_structure_codes(object$Object$Structure))
  expected <- c(.regional_population_statistic, "KREISE", "STAG", "GES", "BEVSTD")
  if (!all(expected %in% codes)) {
    .stop_contract(
      contract,
      sprintf(
        "Structure metadata is missing expected code(s): %s",
        paste(setdiff(expected, codes), collapse = ", ")
      )
    )
  }
  invisible(object)
}

.regional_structure_codes <- function(x) {
  if (!is.list(x)) {
    return(character())
  }
  own <- if (is.character(x$Code) && length(x$Code) == 1L) x$Code else character()
  children <- unlist(lapply(x, .regional_structure_codes), use.names = FALSE)
  c(own, children)
}

.parse_regional_population_content <- function(content, reference_dates, regions,
                                                contract) {
  lines <- strsplit(content, "\\r?\\n", perl = TRUE)[[1L]]
  if (!length(lines) || !identical(lines[1L], paste("Tabelle:", .regional_population_table))) {
    .stop_contract(contract, "table Content does not identify the expected source table")
  }
  status_line <- grep("^Stand: ", trimws(lines), value = TRUE)
  if (length(status_line) != 1L || !nzchar(sub("^Stand: ", "", status_line))) {
    .stop_contract(contract, "table Content must contain exactly one non-empty data status")
  }
  data_status <- sub("^Stand: ", "", status_line)
  separator <- which(grepl("^_+$", trimws(lines)))
  copyright_line <- grep("^\"?\u00a9 ", trimws(lines))
  note_lines <- character()
  if (length(separator) == 1L && length(copyright_line) == 1L &&
      copyright_line > separator) {
    note_lines <- lines[seq.int(separator + 1L, copyright_line - 1L)]
    note_lines <- note_lines[nzchar(trimws(note_lines))]
  }
  source_notes <- if (length(note_lines)) paste(note_lines, collapse = "\n") else character()
  date_line <- which(grepl("^;;[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}", lines))
  sex_line <- which(grepl("^;;Insgesamt(?:;|$)", lines))
  if (length(date_line) != 1L || length(sex_line) != 1L ||
      sex_line != date_line + 1L) {
    .stop_contract(contract, "table Content has unexpected date/sex headers")
  }
  split_fields <- function(line) strsplit(line, ";", fixed = TRUE)[[1L]]
  date_fields <- split_fields(lines[date_line])
  sex_fields <- split_fields(lines[sex_line])
  if (length(date_fields) != length(sex_fields)) {
    .stop_contract(contract, "date and sex header widths differ")
  }
  parsed_dates <- as.Date(date_fields, format = "%d.%m.%Y")
  total_columns <- which(!is.na(parsed_dates) & sex_fields == "Insgesamt")
  requested_columns <- total_columns[parsed_dates[total_columns] %in% reference_dates]
  if (length(requested_columns) != length(reference_dates)) {
    .stop_contract(contract, "response does not contain every requested reference date")
  }

  data_lines <- lines[grepl("^(?:[0-9]{5}|02|11);", lines)]
  if (!length(data_lines)) {
    .stop_contract(contract, "response contains no district observation rows")
  }
  rows <- lapply(data_lines, split_fields)
  if (any(lengths(rows) != length(date_fields))) {
    .stop_contract(contract, "district rows do not match the table header width")
  }
  ids <- vapply(rows, `[[`, character(1), 1L)
  ids[ids == "02"] <- "02000"
  ids[ids == "11"] <- "11000"
  names <- trimws(vapply(rows, `[[`, character(1), 2L))
  if (any(!grepl("^[0-9]{5}$", ids)) || any(!nzchar(names))) {
    .stop_contract(contract, "district identifiers or names are malformed")
  }
  if (!is.null(regions)) {
    missing <- setdiff(regions, ids)
    if (length(missing)) {
      .stop_contract(
        contract,
        sprintf("requested region(s) absent from response: %s", paste(missing, collapse = ", "))
      )
    }
    keep <- ids %in% regions
    rows <- rows[keep]
    ids <- ids[keep]
    names <- names[keep]
  }
  output <- vector("list", length(requested_columns))
  markers <- character()
  for (i in seq_along(requested_columns)) {
    column <- requested_columns[i]
    values <- vapply(rows, `[[`, character(1), column)
    clean <- trimws(values)
    structural_absence <- clean == "-"
    if (!is.null(regions) && any(structural_absence)) {
      absent <- ids[structural_absence]
      .stop_contract(
        contract,
        sprintf(
          "requested region(s) have no population at %s: %s",
          format(parsed_dates[column]), paste(absent, collapse = ", ")
        )
      )
    }
    if (any(structural_absence)) {
      markers <- union(markers, "-")
    }
    bad <- !structural_absence & !grepl("^[0-9]+(?:[.,][0-9]+)?$", clean)
    if (any(bad)) {
      bad_markers <- sort(unique(clean[bad]))
      .stop_contract(
        contract,
        sprintf("population contains missing, nonnumeric, or quality-marked value(s): %s",
                paste(bad_markers, collapse = ", "))
      )
    }
    keep <- !structural_absence
    numeric_values <- suppressWarnings(
      as.numeric(sub(",", ".", clean[keep], fixed = TRUE))
    )
    if (anyNA(numeric_values) || any(!is.finite(numeric_values)) ||
        any(numeric_values < 0)) {
      .stop_contract(contract, "population values must be finite and non-negative")
    }
    output[[i]] <- data.frame(
      geo_id = ids[keep],
      geo_name = names[keep],
      reference_date = rep(parsed_dates[column], sum(keep)),
      population = numeric_values,
      stringsAsFactors = FALSE
    )
  }
  result <- do.call(rbind, output)
  rownames(result) <- NULL
  key <- paste(result$geo_id, format(result$reference_date), sep = "\r")
  if (anyDuplicated(key)) {
    .stop_contract(contract, "response contains duplicate district/reference-date observations")
  }
  attr(result, "quality_markers") <- markers
  attr(result, "data_status") <- data_status
  attr(result, "source_notes") <- source_notes
  result
}
