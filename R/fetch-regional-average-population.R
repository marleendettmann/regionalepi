.regional_average_population_table <- "12411-05-01-4"
.regional_average_population_statistic <- "12411"
.regional_average_population_measure <- "BEV028"
.regional_average_population_transport <- NULL
.regional_average_population_provenance_id <-
  "regional_average_population_12411-05-01-4_bev028"

#' Fetch official district annual-average population
#'
#' Retrieves total annual-average population from Regionaldatenbank table
#' `12411-05-01-4`, measure `BEV028`, for requested reporting years from 2022
#' onward. Years not yet supplied by the source are omitted. The source defines
#' this measure, from 2012 onward, as the simple
#' arithmetic mean of population at the beginning and end of the reporting
#' year. This function is distinct from [fetch_regional_population()], which
#' retrieves population at 31 December.
#'
#' Authentication is read internally from `REGIONALSTATISTIK_USER` and
#' `REGIONALSTATISTIK_PASSWORD`. Credentials are never returned or stored.
#' Structural `-` observations are omitted and are never interpreted as zero.
#'
#' @section Data source and license:
#' Data are obtained from Regionaldatenbank Deutschland, provided by the
#' Statistische Ämter des Bundes und der Länder, under Datenlizenz Deutschland
#' – Namensnennung – Version 2.0 (`dl-de/by-2-0`). regionalepi selects and
#' validates the reviewed source table and measure. Returned provenance retains
#' the source table and measure, retrieval time, source data status, and
#' available source notes. Selection and normalization do not relicense the
#' source data; GPL-3 applies to regionalepi package code.
#' @param years A non-empty vector of unique whole reporting years in the
#'   reviewed range beginning in 2022.
#' @param regions `NULL` or a non-empty character vector of unique
#'   five-character district identifiers.
#' @return An ordinary list with validated `data`, `diagnostics`, and
#'   dataset-level `provenance`.
#' @references [Regionaldatenbank Deutschland](https://www.regionalstatistik.de/),
#'   [Datenlizenz Deutschland – Namensnennung – Version 2.0](https://www.govdata.de/dl-de/by-2-0)
#' @export
fetch_regional_average_population <- function(years, regions = NULL) {
  request <- .validate_regional_average_population_request(years, regions)
  credentials <- .regional_credentials()
  response <- .perform_regional_average_population_request(
    request$years, request$regions, credentials
  )
  .regional_average_population_result(
    response, request$years, request$regions,
    as.POSIXct(Sys.time(), tz = "UTC"), allow_missing_years = TRUE
  )
}

.validate_regional_average_population_request <- function(years, regions) {
  contract <- "Regionaldatenbank annual-average population request"
  if (!is.numeric(years) || !length(years) || anyNA(years) ||
      any(!is.finite(years)) || any(years != floor(years))) {
    .stop_contract(contract, "`years` must be non-empty complete whole numbers")
  }
  years <- as.integer(years)
  if (anyDuplicated(years)) {
    .stop_contract(contract, "`years` must not contain duplicates")
  }
  if (any(years < 2022L)) {
    .stop_contract(contract, "`years` must be 2022 or later")
  }
  if (!is.null(regions) &&
      (!is.character(regions) || !length(regions) || anyNA(regions) ||
       any(!nzchar(regions)) || any(!grepl("^[0-9]{5}$", regions)) ||
       anyDuplicated(regions))) {
    .stop_contract(
      contract,
      "`regions` must be NULL or unique complete five-character district identifiers"
    )
  }
  list(years = sort(years), regions = regions)
}

.regional_average_population_http_transport <- function(years, regions,
                                                         credentials) {
  fields <- list(
    name = .regional_average_population_table,
    area = "all", compress = "false", transpose = "false",
    startyear = as.character(min(years)), endyear = as.character(max(years)),
    regionalvariable = "KREISE"
  )
  if (!is.null(regions)) fields$regionalkey <- paste(regions, collapse = ",")
  request <- httr2::request(paste0(.regional_base_url, "data/table"))
  request <- httr2::req_headers(
    request, username = credentials$username, password = credentials$password
  )
  request <- do.call(
    httr2::req_body_form,
    c(list(.req = request), fields, list(.multipart = FALSE))
  )
  request <- httr2::req_timeout(request, seconds = 120)
  httr2::resp_body_string(httr2::req_perform(request))
}

.perform_regional_average_population_request <- function(years, regions,
                                                          credentials) {
  transport <- .regional_average_population_transport
  if (is.null(transport)) transport <- .regional_average_population_http_transport
  result <- tryCatch(
    transport(years = years, regions = regions, credentials = credentials),
    error = function(error) error
  )
  if (inherits(result, "error")) {
    .stop_contract(
      "Regionaldatenbank annual-average population response",
      "the official service request failed; no credentials are included"
    )
  }
  result
}

.regional_year_rows <- function(content, table, years, regions, contract,
                                allow_missing_years = FALSE) {
  metadata <- .regional_content_metadata(content, contract)
  if (!identical(metadata$lines[[1L]], paste("Tabelle:", table))) {
    .stop_contract(contract, "table content has an unexpected identifier")
  }
  lines <- metadata$lines
  header <- grep(
    "^;;;Insgesamt;[^;]+;[^;]+$",
    lines, value = TRUE
  )
  if (length(header) != 1L) {
    .stop_contract(
      contract,
      "table must select year, Kreis geography, and sex Insgesamt in the expected columns"
    )
  }
  data_lines <- lines[grepl("^[0-9]{4};", lines)]
  rows <- lapply(data_lines, function(line) {
    strsplit(line, ";", fixed = TRUE)[[1L]]
  })
  if (!length(rows) || any(lengths(rows) < 6L)) {
    .stop_contract(contract, "table contains no valid district observations")
  }
  observed_years <- suppressWarnings(as.integer(vapply(rows, `[[`, character(1L), 1L)))
  ids <- vapply(rows, `[[`, character(1L), 2L)
  names <- trimws(vapply(rows, `[[`, character(1L), 3L))
  values <- trimws(vapply(rows, `[[`, character(1L), 4L))
  if (anyNA(observed_years)) {
    .stop_contract(contract, "table contains malformed reporting years")
  }
  district <- grepl("^[0-9]{5}$", ids) | ids %in% c("02", "11")
  if (any(!district & !grepl("^(DG|[0-9]{2,8})$", ids))) {
    .stop_contract(contract, "table contains an unexpected regional key")
  }
  observed_years <- observed_years[district]
  ids <- ids[district]
  names <- names[district]
  values <- values[district]
  ids[ids == "02"] <- "02000"
  ids[ids == "11"] <- "11000"
  if (!allow_missing_years && !all(years %in% observed_years)) {
    .stop_contract(contract, "one or more requested years are absent")
  }
  structural <- values == "-"
  markers <- if (any(structural)) "-" else character()
  if (!is.null(regions) && any(structural & ids %in% regions &
      observed_years %in% years)) {
    absent <- unique(paste(
      ids[structural & ids %in% regions & observed_years %in% years],
      observed_years[structural & ids %in% regions & observed_years %in% years],
      sep = "@"
    ))
    .stop_contract(
      contract,
      sprintf("requested region/year has no annual-average population: %s",
              paste(absent, collapse = ", "))
    )
  }
  keep <- !structural & observed_years %in% years
  if (!is.null(regions)) keep <- keep & ids %in% regions
  if (!any(keep)) {
    .stop_contract(contract, "table contains no valid requested district observations")
  }
  result <- data.frame(
    geo_id = ids[keep], geo_name = names[keep], year = observed_years[keep],
    population = .regional_decimal(
      values[keep], "annual-average population", contract
    ),
    stringsAsFactors = FALSE
  )
  if (!allow_missing_years && !setequal(unique(result$year), years)) {
    .stop_contract(contract, "one or more requested years are absent")
  }
  if (any(!grepl("^[0-9]{5}$", result$geo_id)) ||
      any(!nzchar(result$geo_name))) {
    .stop_contract(contract, "district identifiers or names are malformed")
  }
  if (!is.null(regions)) {
    observed <- unique(paste(result$geo_id, result$year, sep = "\r"))
    expected <- as.vector(outer(regions, years, paste, sep = "\r"))
    if (!setequal(observed, expected)) {
      .stop_contract(contract, "requested region/year coverage is incomplete")
    }
  }
  key <- paste(result$geo_id, result$year, sep = "\r")
  if (anyDuplicated(key)) {
    .stop_contract(contract, "response contains duplicate district/year observations")
  }
  list(
    data = result, data_status = metadata$data_status,
    source_notes = metadata$source_notes, source_quality_markers = markers
  )
}

.validate_regional_average_population_structure <- function(structure,
                                                             contract) {
  get_code <- function(x) {
    if (!is.list(x) || !is.character(x$Code) || length(x$Code) != 1L) {
      return(NA_character_)
    }
    x$Code
  }
  head_measure <- if (is.list(structure$Head) &&
      is.list(structure$Head$Structure) &&
      length(structure$Head$Structure) == 1L) {
    get_code(structure$Head$Structure[[1L]])
  } else NA_character_
  actual <- c(
    head = get_code(structure$Head),
    measure = head_measure,
    columns = if (is.list(structure$Columns) && length(structure$Columns) == 1L)
      get_code(structure$Columns[[1L]]) else NA_character_,
    rows = if (is.list(structure$Rows) && length(structure$Rows) == 1L)
      get_code(structure$Rows[[1L]]) else NA_character_,
    subheading = get_code(structure$Subheading)
  )
  expected <- c(
    head = "12411", measure = "BEV028", columns = "GES",
    rows = "KREISE", subheading = "JAHR"
  )
  if (!identical(actual, expected)) {
    .stop_contract(
      contract,
      "structure must place 12411/BEV028/GES/KREISE/JAHR in the reviewed roles"
    )
  }
  invisible(structure)
}

.annual_average_population_basis <- function(years, source_notes, contract) {
  evidence <- paste(source_notes, collapse = "\n")
  has_basis <- grepl("Mai 2022", evidence, fixed = TRUE) &&
    grepl("Zensus vom 15. Mai 2022", evidence, fixed = TRUE)
  if (!has_basis) {
    .stop_contract(
      contract,
      "source notes do not establish the reviewed Census 2022 population basis"
    )
  }
  rep("census_2022", length(years))
}

.regional_average_population_result <- function(response, years, regions,
                                                  retrieved_at,
                                                  allow_missing_years = FALSE) {
  contract <- "Regionaldatenbank annual-average population response"
  object <- .regional_table_object(
    response, .regional_average_population_table,
    c(
      .regional_average_population_statistic,
      .regional_average_population_measure, "GES", "KREISE", "JAHR"
    ),
    contract
  )
  .validate_regional_average_population_structure(
    object$Object$Structure, contract
  )
  parsed <- .regional_year_rows(
    object$Object$Content, .regional_average_population_table,
    years, regions, contract, allow_missing_years
  )
  basis <- .annual_average_population_basis(
    parsed$data$year, parsed$source_notes, contract
  )
  data <- data.frame(
    geo_id = parsed$data$geo_id,
    geo_name = parsed$data$geo_name,
    geo_level = "district",
    geo_vintage = as.Date(NA),
    year = as.integer(parsed$data$year),
    population = parsed$data$population,
    population_measure = "annual_average_population",
    population_reference = "reporting_year_annual_average",
    population_basis = basis,
    source = .regional_source,
    source_table = .regional_average_population_table,
    retrieved_at = retrieved_at,
    data_status = parsed$data_status,
    provenance_id = .regional_average_population_provenance_id,
    stringsAsFactors = FALSE
  )
  validate_annual_average_population(data)
  list(
    data = data,
    diagnostics = list(
      source = .regional_source,
      source_table = .regional_average_population_table,
      statistic = .regional_average_population_statistic,
      source_measure = .regional_average_population_measure,
      requested_years = years,
      requested_region_count = if (is.null(regions)) NA_integer_ else length(regions),
      returned_region_count = length(unique(data$geo_id)),
      returned_observation_count = nrow(data),
      observations_by_year = table(data$year),
      source_quality_markers = parsed$source_quality_markers,
      geo_vintage_unresolved = all(is.na(data$geo_vintage))
    ),
    provenance = stats::setNames(list(list(
      source = .regional_source,
      statistic = .regional_average_population_statistic,
      source_table = .regional_average_population_table,
      source_measure = .regional_average_population_measure,
      measure = "annual_average_population",
      unit = "persons",
      selection = list(sex = "Insgesamt"),
      requested_years = years,
      population_reference = "reporting_year_annual_average",
      definition = paste(
        "From 2012, the simple arithmetic mean of population at the beginning",
        "and end of the reporting year."
      ),
      population_basis = "Census 2022 basis explicitly established by source notes.",
      data_status = parsed$data_status,
      retrieved_at = retrieved_at,
      source_notes = parsed$source_notes,
      copyright = object$Copyright,
      geo_vintage = "Not independently established; returned as NA_Date_."
    )), .regional_average_population_provenance_id)
  )
}

.fetch_regional_average_population_available <- function(years) {
  request <- .validate_regional_average_population_request(years, NULL)
  credentials <- .regional_credentials()
  response <- .perform_regional_average_population_request(
    request$years, NULL, credentials
  )
  .regional_average_population_result(
    response, request$years, NULL, as.POSIXct(Sys.time(), tz = "UTC"),
    allow_missing_years = TRUE
  )
}
