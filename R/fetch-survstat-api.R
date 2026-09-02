.survstat_api_endpoint <-
  "https://tools.rki.de/SurvStat/SurvStatWebService.svc"
.survstat_api_namespace <- "http://tools.rki.de/SurvStat/"
.survstat_contract_namespace <- paste0(
  "http://schemas.datacontract.org/2004/07/",
  "Rki.SurvStat.WebService.Contracts.Mdx"
)
.survstat_api_transport <- NULL
.survstat_cube <- "SurvStat"
.survstat_disease_dimension <- "[PathogenOut].[KategorieNz]"
.survstat_disease_hierarchy <-
  "[PathogenOut].[KategorieNz].[Krankheit DE]"
.survstat_place_dimension <- "[DeutschlandNodes].[Kreise71Web]"
.survstat_county_hierarchy <-
  "[DeutschlandNodes].[Kreise71Web].[CountyKey71]"
.survstat_state_hierarchy <-
  "[DeutschlandNodes].[Kreise71Web].[FedStateKey71]"
.survstat_year_dimension <- "[ReportingDate]"
.survstat_year_hierarchy <- "[ReportingDate].[WeekYear]"
.survstat_week_hierarchy <- "[ReportingDate].[YearWeek]"
.survstat_reference_dimension <- "[ReferenzDefinition]"
.survstat_reference_hierarchy <- "[ReferenzDefinition].[ID]"
.survstat_reporting_path <- "\u00dcber Gesundheitsamt und Landesstelle"
.survstat_incidence_measure <- list(
  request_value = "Incidence", source_id = "[Measures].[Inzidenz_71_Web]",
  source_label = "Inzidenz 7.1s", measure = "incidence",
  value_semantics = "non_additive"
)
.survstat_cases_measure <- list(
  request_value = "Count", source_id = "[Measures].[FallCount_71_Web]",
  source_label = "Anzahl.71s", measure = "cases",
  value_semantics = "additive"
)

#' Fetch source-provided incidence from the official SurvStat service
#'
#' Retrieves weekly source-provided incidence through the official RKI SOAP
#' service. The adapter is deliberately limited to explicit reporting years and
#' either Kreis output or one explicitly filtered Bundesland. It performs no
#' incidence calculation, geographic resolution, or Berlin-specific canonical
#' mapping.
#'
#' The returned observations use the same `surveillance_incidence` contract as
#' [read_survstat_incidence()]. Missing source cells remain `NA_real_`; numeric
#' zero remains zero. Live SurvStat data may be revised, so the result records
#' retrieval time, cube data status, endpoint, query settings, and source-member
#' identifiers.
#'
#' @param pathogen One exact reviewed SurvStat disease member label.
#' @param reporting_years Non-empty unique explicit reporting years.
#' @param geography Exactly `"kreis"` or `"bundesland"`.
#' @param geography_filter `NULL` for Kreis output; one exact SurvStat
#'   Bundesland member label for Bundesland output.
#' @param query_id One caller-supplied non-empty provenance identifier.
#' @param reference_definition Must be `TRUE` in v0.1.
#' @param reporting_path Must equal the reviewed reporting-path semantics of
#'   the SurvStat cube used by this adapter.
#' @return An ordinary list with canonical `data`, `diagnostics`, and
#'   query-level `provenance`.
#' @export
fetch_survstat_incidence <- function(
    pathogen, reporting_years, geography = c("kreis", "bundesland"),
    geography_filter = NULL, query_id,
    reference_definition = TRUE,
    reporting_path = .survstat_reporting_path) {
  .fetch_survstat_measure(
    pathogen, reporting_years, geography, geography_filter, query_id,
    reference_definition, reporting_path, .survstat_incidence_measure
  )
}

#' Fetch reported case counts from the official SurvStat service
#'
#' Retrieves the reviewed additive weekly count measure `Anzahl.71s`. It uses
#' the same narrow pathogen, year, reference-definition, reporting-path and
#' geography scope as [fetch_survstat_incidence()], but never calculates or
#' replaces incidence. Source null cells remain `NA_real_` and numeric zero
#' remains zero.
#'
#' @inheritParams fetch_survstat_incidence
#' @return An ordinary list with validated additive surveillance `data`,
#'   `diagnostics`, and query-level `provenance`.
#' @export
fetch_survstat_cases <- function(
    pathogen, reporting_years, geography = c("kreis", "bundesland"),
    geography_filter = NULL, query_id,
    reference_definition = TRUE,
    reporting_path = .survstat_reporting_path) {
  .fetch_survstat_measure(
    pathogen, reporting_years, geography, geography_filter, query_id,
    reference_definition, reporting_path, .survstat_cases_measure
  )
}

.fetch_survstat_measure <- function(
    pathogen, reporting_years, geography, geography_filter, query_id,
    reference_definition, reporting_path, measure_spec) {
  request <- .validate_survstat_api_request(
    pathogen, reporting_years, geography, geography_filter, query_id,
    reference_definition, reporting_path
  )
  cube <- .survstat_api_cube_info()
  disease <- .survstat_api_match_member(
    .survstat_disease_hierarchy, request$pathogen, "pathogen"
  )
  place_hierarchy <- if (request$geography == "kreis") {
    .survstat_county_hierarchy
  } else {
    .survstat_state_hierarchy
  }
  place <- if (request$geography == "bundesland") {
    .survstat_api_match_member(
      .survstat_state_hierarchy, request$geography_filter,
      "Bundesland geography filter"
    )
  } else {
    NULL
  }
  retrieved_at <- as.POSIXct(Sys.time(), tz = "UTC")
  query <- .survstat_api_query_provenance(
    request, cube$data_status, retrieved_at, disease, place, measure_spec
  )
  pages <- lapply(request$reporting_years, function(year) {
    filters <- list(
      list(
        dimension = .survstat_disease_dimension,
        hierarchy = .survstat_disease_hierarchy, members = disease$id
      ),
      list(
        dimension = .survstat_reference_dimension,
        hierarchy = .survstat_reference_hierarchy,
        members = "[ReferenzDefinition].[ID].&[1]"
      ),
      list(
        dimension = .survstat_year_dimension,
        hierarchy = .survstat_year_hierarchy,
        members = sprintf("[ReportingDate].[WeekYear].&[%d]", year)
      )
    )
    if (!is.null(place)) {
      filters[[length(filters) + 1L]] <- list(
        dimension = .survstat_place_dimension,
        hierarchy = .survstat_state_hierarchy, members = place$id
      )
    }
    envelope <- .survstat_olap_envelope(
      place_hierarchy, filters, measure_spec$request_value
    )
    document <- .survstat_api_call("GetOlapData", envelope)
    .parse_survstat_olap(document, year, request$geography, query, measure_spec)
  })
  data <- do.call(rbind, pages)
  rownames(data) <- NULL
  if (!nrow(data)) {
    .stop_contract("SurvStat API response", "the reviewed query returned no observations")
  }
  key <- paste(data$geo_name, data$reporting_year, data$reporting_week, sep = "\r")
  if (anyDuplicated(key)) {
    .stop_contract("SurvStat API response", "duplicate source geography/week observations")
  }
  data <- data[order(
    data$reporting_year, data$geo_name, data$reporting_week,
    method = "radix"
  ), , drop = FALSE]
  rownames(data) <- NULL
  if (identical(measure_spec$measure, "incidence")) {
    validate_surveillance_incidence(data)
  } else validate_surveillance(data)
  coverage <- lapply(request$reporting_years, function(year) {
    sort(unique(data$reporting_week[data$reporting_year == year]))
  })
  names(coverage) <- as.character(request$reporting_years)
  query$reporting_week_coverage <- coverage
  list(
    data = data,
    diagnostics = list(
      source = "SurvStat@RKI", source_version = "SurvStat@RKI 2.0",
      adapter = "official_soap_api_v0.1", endpoint = .survstat_api_endpoint,
      operation = "GetOlapData", cube = .survstat_cube,
      requested_years = request$reporting_years,
      year_chunk_count = length(request$reporting_years),
      source_geography_count = length(unique(data$geo_name)),
      observation_count = nrow(data),
      measure = measure_spec$measure,
      source_measure_id = measure_spec$source_id,
      source_measure_label = measure_spec$source_label,
      value_semantics = measure_spec$value_semantics,
      blank_value_cells = sum(is.na(data[[measure_spec$measure]])),
      numeric_zero_value_cells = sum(data[[measure_spec$measure]] == 0, na.rm = TRUE),
      blank_incidence_cells = if (identical(measure_spec$measure, "incidence"))
        sum(is.na(data$incidence)) else NULL,
      numeric_zero_incidence_cells = if (identical(measure_spec$measure, "incidence"))
        sum(data$incidence == 0, na.rm = TRUE) else NULL,
      data_status = cube$data_status, geo_vintage_unresolved = TRUE,
      incidence_calculation_performed = FALSE,
      rate_summation_performed = FALSE, rate_averaging_performed = FALSE
    ),
    provenance = query
  )
}

.validate_survstat_api_request <- function(
    pathogen, reporting_years, geography, geography_filter, query_id,
    reference_definition, reporting_path) {
  contract <- "SurvStat API request"
  .check_scalar_nonempty_character(pathogen, "pathogen", contract)
  .check_scalar_nonempty_character(query_id, "query_id", contract)
  geography <- match.arg(geography, c("kreis", "bundesland"))
  if (!is.numeric(reporting_years) || !length(reporting_years) ||
      anyNA(reporting_years) || any(!is.finite(reporting_years)) ||
      any(reporting_years != floor(reporting_years)) ||
      any(reporting_years < 2001L | reporting_years > 9998L) ||
      anyDuplicated(reporting_years)) {
    .stop_contract(
      contract,
      "`reporting_years` must contain unique explicit whole years from 2001 through 9998"
    )
  }
  if (!is.logical(reference_definition) || length(reference_definition) != 1L ||
      is.na(reference_definition) || !reference_definition) {
    .stop_contract(contract, "v0.1 supports only `reference_definition = TRUE`")
  }
  .check_scalar_nonempty_character(reporting_path, "reporting_path", contract)
  if (!identical(reporting_path, .survstat_reporting_path)) {
    .stop_contract(contract, "unsupported reporting path for the reviewed SurvStat cube")
  }
  if (geography == "kreis" && !is.null(geography_filter)) {
    .stop_contract(contract, "`geography_filter` must be NULL for Kreis output")
  }
  if (geography == "bundesland") {
    .check_scalar_nonempty_character(
      geography_filter, "geography_filter", contract
    )
  }
  list(
    pathogen = pathogen, reporting_years = sort(as.integer(reporting_years)),
    geography = geography, geography_filter = geography_filter,
    query_id = query_id, reference_definition = reference_definition,
    reporting_path = reporting_path
  )
}

.survstat_api_query_provenance <- function(
    request, data_status, retrieved_at, disease, place,
    measure_spec = .survstat_incidence_measure) {
  source_geography <- if (request$geography == "kreis") NULL else list(
    geo_id = NA_character_, geo_name = place$caption,
    geo_level = "survstat_bundesland"
  )
  query <- list(
    query_id = request$query_id,
    query_role = paste0(if (request$geography == "kreis") "kreis_" else
      "bundesland_", measure_spec$measure),
    source = "SurvStat@RKI", source_version = "SurvStat@RKI 2.0",
    measure = measure_spec$measure,
    source_measure_id = measure_spec$source_id,
    source_measure_label = measure_spec$source_label,
    api_measure_request_value = measure_spec$request_value,
    value_semantics = measure_spec$value_semantics,
    pathogen = request$pathogen,
    reporting_years = request$reporting_years,
    time_unit = "week",
    source_geography_dimension = if (request$geography == "kreis") {
      "survstat_kreis"
    } else {
      "survstat_bundesland"
    },
    source_geography_filter = source_geography,
    row_dimension = "Meldewoche",
    column_dimension = if (request$geography == "kreis") "Kreis" else
      "Bundesland",
    reference_definition = request$reference_definition,
    reporting_path = request$reporting_path,
    epidemiological_filters = list(),
    empty_rows_and_columns = TRUE, totals = FALSE,
    retrieved_at = retrieved_at, data_status = data_status,
    geo_vintage = as.Date(NA)
  )
  query$transport <- "official_soap_api"
  query$api_endpoint <- .survstat_api_endpoint
  query$api_binding <- "SOAP 1.2 document/literal with WS-Addressing"
  query$api_namespace <- .survstat_api_namespace
  query$api_operation <- "GetOlapData"
  query$cube <- .survstat_cube
  query$pathogen_member_id <- disease$id
  query$geography_member_id <- if (is.null(place)) NA_character_ else place$id
  query$geography_hierarchy <- if (request$geography == "kreis") {
    .survstat_county_hierarchy
  } else {
    .survstat_state_hierarchy
  }
  query
}

.survstat_api_cube_info <- function() {
  body <- paste0(
    "<sur:GetCubeInfo><sur:request><rki:Cube>", .survstat_cube,
    "</rki:Cube><rki:Language>German</rki:Language>",
    "</sur:request></sur:GetCubeInfo>"
  )
  document <- .survstat_api_call(
    "GetCubeInfo", .survstat_soap_envelope("GetCubeInfo", body)
  )
  node <- xml2::xml_find_first(
    document, "//*[local-name()='LastDataUpdate']"
  )
  if (inherits(node, "xml_missing")) {
    .stop_contract("SurvStat API response", "cube metadata lacks LastDataUpdate")
  }
  value <- xml2::xml_text(node)
  normalized <- sub(
    "([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", value, perl = TRUE
  )
  parsed <- as.POSIXct(strptime(
    normalized, "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"
  ))
  if (length(parsed) != 1L || is.na(parsed)) {
    .stop_contract("SurvStat API response", "cube data status is malformed")
  }
  list(data_status = parsed, source_value = value)
}

.survstat_api_match_member <- function(hierarchy, label, field) {
  body <- paste0(
    "<sur:GetAllHierarchyMembers><sur:request><rki:Cube>",
    .survstat_cube, "</rki:Cube><rki:HierarchyId>",
    .survstat_xml_escape(hierarchy),
    "</rki:HierarchyId><rki:Language>German</rki:Language>",
    "</sur:request></sur:GetAllHierarchyMembers>"
  )
  document <- .survstat_api_call(
    "GetAllHierarchyMembers",
    .survstat_soap_envelope("GetAllHierarchyMembers", body)
  )
  nodes <- xml2::xml_find_all(document, "//*[local-name()='HierarchyMember']")
  if (!length(nodes)) {
    .stop_contract("SurvStat API response", "hierarchy member metadata is empty")
  }
  captions <- vapply(nodes, .survstat_child_text, character(1L), "Caption")
  ids <- vapply(nodes, .survstat_child_text, character(1L), "Id")
  matches <- which(captions == label)
  if (length(matches) != 1L || !nzchar(ids[[matches]])) {
    .stop_contract(
      "SurvStat API request",
      sprintf("unknown or ambiguous exact %s member", field)
    )
  }
  list(caption = captions[[matches]], id = ids[[matches]], hierarchy = hierarchy)
}

.survstat_olap_envelope <- function(column_hierarchy, filters,
                                     measure = "Incidence") {
  if (!measure %in% c("Incidence", "Count")) {
    .stop_contract("SurvStat API request", "unsupported API measure in v0.1")
  }
  filter_xml <- paste(vapply(filters, function(filter) {
    members <- paste0(
      "<rki:string>", .survstat_xml_escape(filter$members), "</rki:string>",
      collapse = ""
    )
    paste0(
      "<rki:KeyValueOfFilterCollectionKeyFilterMemberCollectionb2rWaiIW>",
      "<rki:Key><rki:DimensionId>",
      .survstat_xml_escape(filter$dimension),
      "</rki:DimensionId><rki:HierarchyId>",
      .survstat_xml_escape(filter$hierarchy),
      "</rki:HierarchyId></rki:Key><rki:Value>", members,
      "</rki:Value></rki:KeyValueOfFilterCollectionKeyFilterMemberCollectionb2rWaiIW>"
    )
  }, character(1L)), collapse = "")
  body <- paste0(
    "<sur:GetOlapData><sur:request><rki:ColumnHierarchy>",
    .survstat_xml_escape(column_hierarchy), "</rki:ColumnHierarchy>",
    "<rki:Cube>", .survstat_cube, "</rki:Cube>",
    "<rki:HierarchyFilters>", filter_xml, "</rki:HierarchyFilters>",
    "<rki:IncludeNullColumns>true</rki:IncludeNullColumns>",
    "<rki:IncludeNullRows>true</rki:IncludeNullRows>",
    "<rki:IncludeTotalColumn>false</rki:IncludeTotalColumn>",
    "<rki:IncludeTotalRow>false</rki:IncludeTotalRow>",
    "<rki:Language>German</rki:Language>",
    "<rki:Measures>", measure, "</rki:Measures><rki:RowHierarchy>",
    .survstat_week_hierarchy,
    "</rki:RowHierarchy></sur:request></sur:GetOlapData>"
  )
  .survstat_soap_envelope("GetOlapData", body)
}

.survstat_soap_envelope <- function(operation, body) {
  paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<soap:Envelope xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\" ",
    "xmlns:sur=\"", .survstat_api_namespace, "\" xmlns:rki=\"",
    .survstat_contract_namespace,
    "\" xmlns:wsa=\"http://www.w3.org/2005/08/addressing\">",
    "<soap:Header><wsa:To>", .survstat_api_endpoint, "</wsa:To>",
    "<wsa:Action>", .survstat_api_namespace,
    "SurvStatWebService/", operation, "</wsa:Action></soap:Header>",
    "<soap:Body>", body, "</soap:Body></soap:Envelope>"
  )
}

.survstat_xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

.survstat_api_call <- function(operation, envelope) {
  transport <- .survstat_api_transport
  if (is.null(transport)) transport <- .survstat_api_http_transport
  response <- tryCatch(
    transport(operation = operation, envelope = envelope),
    error = function(error) error
  )
  if (inherits(response, "error")) {
    .stop_contract("SurvStat API transport", "the official service request failed")
  }
  if (!is.list(response) || !identical(sort(names(response)),
      sort(c("status_code", "body"))) ||
      !is.numeric(response$status_code) || length(response$status_code) != 1L ||
      !is.character(response$body) || length(response$body) != 1L) {
    .stop_contract("SurvStat API transport", "transport returned a malformed response")
  }
  document <- tryCatch(
    xml2::read_xml(response$body, options = c("NONET")),
    error = function(error) error
  )
  if (inherits(document, "error")) {
    .stop_contract("SurvStat API response", "response is not well-formed SOAP XML")
  }
  fault <- xml2::xml_find_first(document, "//*[local-name()='Fault']")
  if (!inherits(fault, "xml_missing")) {
    reason <- xml2::xml_find_first(fault, ".//*[local-name()='Text']")
    message <- if (inherits(reason, "xml_missing")) "unspecified SOAP Fault" else
      trimws(xml2::xml_text(reason))
    if (!nzchar(message)) message <- "unspecified SOAP Fault"
    .stop_contract(
      "SurvStat API response",
      paste0("SOAP Fault: ", substr(message, 1L, 240L))
    )
  }
  if (response$status_code < 200L || response$status_code >= 300L) {
    .stop_contract(
      "SurvStat API transport",
      sprintf("official service returned HTTP status %d", response$status_code)
    )
  }
  result <- xml2::xml_find_first(
    document, sprintf("//*[local-name()='%sResult']", operation)
  )
  if (inherits(result, "xml_missing")) {
    .stop_contract("SurvStat API response", "SOAP response lacks the expected result member")
  }
  document
}

.survstat_api_http_transport <- function(operation, envelope) {
  request <- httr2::request(.survstat_api_endpoint)
  request <- httr2::req_headers(
    request, Accept = "application/soap+xml, text/xml"
  )
  request <- httr2::req_body_raw(
    request, charToRaw(envelope), type = "application/soap+xml;charset=utf-8"
  )
  request <- httr2::req_timeout(request, seconds = 120)
  request <- httr2::req_error(request, is_error = function(response) FALSE)
  response <- httr2::req_perform(request)
  list(
    status_code = as.integer(httr2::resp_status(response)),
    body = httr2::resp_body_string(response)
  )
}

.survstat_child_text <- function(node, child) {
  value <- xml2::xml_find_first(
    node, sprintf("./*[local-name()='%s']", child)
  )
  if (inherits(value, "xml_missing")) "" else xml2::xml_text(value)
}

.parse_survstat_olap <- function(document, requested_year, geography, query,
                                 measure_spec = .survstat_incidence_measure) {
  contract <- "SurvStat API response"
  columns <- xml2::xml_find_all(document, "//*[local-name()='QueryResultColumn']")
  rows <- xml2::xml_find_all(document, "//*[local-name()='QueryResultRow']")
  if (!length(columns) || !length(rows)) {
    .stop_contract(contract, "OLAP result lacks columns or rows")
  }
  column_names <- vapply(columns, .survstat_child_text, character(1L), "Caption")
  column_ids <- vapply(columns, .survstat_child_text, character(1L), "ColumnName")
  keep_columns <- !column_names %in% c("Gesamt", "Unbekannt") & nzchar(column_names)
  column_names <- column_names[keep_columns]
  column_ids <- column_ids[keep_columns]
  if (!length(column_names) || anyDuplicated(column_names) || any(!nzchar(column_ids))) {
    .stop_contract(contract, "OLAP geography columns are empty, duplicate, or malformed")
  }
  observations <- vector("list", length(rows))
  retained <- 0L
  for (row in rows) {
    caption <- .survstat_child_text(row, "Caption")
    if (caption == "Gesamt") next
    matched <- regexec("^([0-9]{4})-KW([0-9]{2})$", caption)
    parts <- regmatches(caption, matched)[[1L]]
    if (length(parts) != 3L) {
      .stop_contract(contract, "OLAP row has an invalid reporting year/week caption")
    }
    year <- as.integer(parts[[2L]])
    week <- as.integer(parts[[3L]])
    if (year != requested_year) {
      .stop_contract(contract, "OLAP response contains an unexpected reporting year")
    }
    date <- .iso_week_monday(year, week, contract)
    values_node <- xml2::xml_find_first(row, "./*[local-name()='Values']")
    if (inherits(values_node, "xml_missing")) {
      .stop_contract(contract, "OLAP row lacks its expected Values member")
    }
    values <- xml2::xml_children(values_node)
    if (length(values) != length(keep_columns)) {
      .stop_contract(contract, "OLAP row value count does not match geography columns")
    }
    values <- values[keep_columns]
    text <- xml2::xml_text(values)
    nil <- vapply(values, function(value) {
      attribute <- xml2::xml_attr(value, "nil")
      !is.na(attribute) && identical(tolower(attribute), "true")
    }, logical(1L))
    text[nil] <- NA_character_
    values_parsed <- if (identical(measure_spec$measure, "incidence")) {
      .parse_survstat_incidence(text, "API incidence values", contract)
    } else {
      .parse_survstat_api_counts(text, "API count values", contract)
    }
    retained <- retained + 1L
    common <- list(
      rep(NA_character_, length(column_names)), column_names,
      rep(if (geography == "kreis") "survstat_kreis" else
        "survstat_bundesland", length(column_names)),
      as.Date(NA), rep(date, length(column_names)), query,
      rep(year, length(column_names)), rep(week, length(column_names)),
      values_parsed
    )
    observations[[retained]] <- if (identical(measure_spec$measure, "incidence")) {
      do.call(.new_survstat_incidence_data, common)
    } else do.call(.new_survstat_cases_data, common)
  }
  if (!retained) {
    .stop_contract(contract, "OLAP result contains no weekly observations")
  }
  do.call(rbind, observations[seq_len(retained)])
}

.parse_survstat_api_counts <- function(x, field, contract) {
  values <- as.character(x)
  missing <- is.na(values) | values == ""
  if (any(grepl("^[[:space:]]*[<>=~*]", values[!missing]))) {
    .stop_contract(contract, paste0(field, " contain a quality-marked value"))
  }
  parsed <- suppressWarnings(as.numeric(values))
  if (any(!missing & is.na(parsed)) || any(!is.finite(parsed[!missing]))) {
    .stop_contract(contract, paste0(field, " contain a malformed non-numeric value"))
  }
  if (any(parsed[!missing] < 0) || any(parsed[!missing] != floor(parsed[!missing]))) {
    .stop_contract(contract, paste0(field, " must be non-negative whole-valued counts"))
  }
  parsed
}

.new_survstat_cases_data <- function(
    geo_id, geo_name, geo_level, geo_vintage, date, query,
    reporting_year, reporting_week, cases) {
  data.frame(
    geo_id = geo_id, geo_name = geo_name, geo_level = geo_level,
    geo_vintage = rep(geo_vintage, length(date)), date = date,
    time_unit = "week", pathogen = query$pathogen, cases = cases,
    source = query$source, source_version = query$source_version,
    reporting_year = reporting_year, reporting_week = reporting_week,
    query_id = query$query_id, stringsAsFactors = FALSE
  )
}
