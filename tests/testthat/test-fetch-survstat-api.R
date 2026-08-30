test_that("API incidence maps to the existing canonical contract", {
  pathogen <- "Influenza, saisonal"
  years <- 2020
  result <- with_survstat_api_transport(
    survstat_api_transport(),
    fetch_survstat_incidence(pathogen, years, "kreis", query_id = "api-flu")
  )
  expect_named(result, c("data", "diagnostics", "provenance"),
               ignore.order = FALSE)
  expect_identical(validate_surveillance_incidence(result$data), result$data)
  expect_identical(result$data$geo_name, c("LK Alpha", "LK Alpha"))
  expect_identical(result$data$incidence, c(1.25, 0))
  expect_identical(result$data$reporting_week, c(1L, 53L))
  expect_true(all(is.na(result$data$geo_id)))
  expect_true(all(is.na(result$data$geo_vintage)))
  expect_identical(result$provenance$query_role, "kreis_incidence")
  expect_identical(result$provenance$transport, "official_soap_api")
  expect_identical(pathogen, "Influenza, saisonal")
  expect_identical(years, 2020)
})

test_that("API preserves missing incidence separately from numeric zero", {
  response <- survstat_olap_response(
    geography = "LK Alpha", weeks = c("2020-KW01", "2020-KW02"),
    values = list(NA_character_, "0,00")
  )
  result <- with_survstat_api_transport(
    survstat_api_transport(olap = response),
    fetch_survstat_incidence("COVID-19", 2020, query_id = "api-covid")
  )
  expect_true(is.na(result$data$incidence[[1L]]))
  expect_identical(result$data$incidence[[2L]], 0)
  expect_identical(result$diagnostics$blank_incidence_cells, 1L)
  expect_identical(result$diagnostics$numeric_zero_incidence_cells, 1L)
})

test_that("Bundesland query retains reviewed source-filter provenance", {
  response <- survstat_olap_response(
    geography = "Berlin", weeks = "2020-KW01", values = list("2,50")
  )
  result <- with_survstat_api_transport(
    survstat_api_transport(olap = response),
    fetch_survstat_incidence(
      "Influenza, saisonal", 2020, "bundesland", "Berlin", "api-berlin"
    )
  )
  expect_identical(result$data$geo_name, "Berlin")
  expect_identical(result$data$geo_level, "survstat_bundesland")
  expect_identical(result$provenance$query_role, "bundesland_incidence")
  expect_identical(
    result$provenance$source_geography_filter$geo_name, "Berlin"
  )
  expect_match(result$provenance$geography_member_id, "Berlin", fixed = TRUE)
})

test_that("API request scope is strict", {
  expect_error(fetch_survstat_incidence(
    "Disease", numeric(), query_id = "q"
  ), "explicit whole years")
  expect_error(fetch_survstat_incidence(
    "Disease", 2020, "kreis", "Berlin", "q"
  ), "must be NULL")
  expect_error(fetch_survstat_incidence(
    "Disease", 2020, "bundesland", NULL, "q"
  ), "geography_filter")
  expect_error(fetch_survstat_incidence(
    "Disease", 2020, "unknown", query_id = "q"
  ), "arg")
  expect_error(fetch_survstat_incidence(
    "Disease", 2020, query_id = "q", reference_definition = FALSE
  ), "reference_definition")
  expect_error(fetch_survstat_incidence(
    "Disease", 2020, query_id = "q", reporting_path = "other"
  ), "reporting path")
  expect_error(regionalepi:::.survstat_olap_envelope(
    "hierarchy", list(), measure = "Count"
  ), "unsupported API measure")
})

test_that("unknown exact pathogen and geography members fail", {
  transport <- survstat_api_transport(pathogens = "Known", states = "Known state")
  expect_error(with_survstat_api_transport(
    transport, fetch_survstat_incidence("Unknown", 2020, query_id = "q")
  ), "unknown or ambiguous exact pathogen")
  expect_error(with_survstat_api_transport(
    transport, fetch_survstat_incidence(
      "Known", 2020, "bundesland", "Unknown", "q"
    )
  ), "unknown or ambiguous exact Bundesland")
})

test_that("HTTP, malformed XML, SOAP Fault, and missing result are concise", {
  call <- function(transport) with_survstat_api_transport(
    transport,
    regionalepi:::.survstat_api_call("GetCubeInfo", "synthetic-envelope")
  )
  expect_error(call(function(...) list(status_code = 503L, body =
    soap_response("GetCubeInfo", "<b:LastDataUpdate>x</b:LastDataUpdate>"))),
    "HTTP status 503")
  expect_error(call(function(...) list(status_code = 200L, body = "<bad")),
               "well-formed SOAP XML")
  fault <- paste0(
    "<s:Envelope xmlns:s='http://www.w3.org/2003/05/soap-envelope'>",
    "<s:Body><s:Fault><s:Reason><s:Text>Reviewed failure</s:Text>",
    "</s:Reason></s:Fault></s:Body></s:Envelope>"
  )
  expect_error(call(function(...) list(status_code = 500L, body = fault)),
               "SOAP Fault: Reviewed failure")
  missing <- "<s:Envelope xmlns:s='http://www.w3.org/2003/05/soap-envelope'><s:Body/></s:Envelope>"
  expect_error(call(function(...) list(status_code = 200L, body = missing)),
               "expected result member")
})

test_that("empty, duplicate, invalid week, and malformed values fail", {
  run <- function(response) with_survstat_api_transport(
    survstat_api_transport(olap = response),
    fetch_survstat_incidence("COVID-19", 2020, query_id = "q")
  )
  empty <- soap_response("GetOlapData", "<b:Columns/><b:QueryResults/>")
  expect_error(run(empty), "lacks columns or rows")
  expect_error(run(survstat_olap_response(duplicate_week = TRUE)), "duplicate")
  expect_error(run(survstat_olap_response(
    geography = "LK Alpha", weeks = "2020-KW54", values = list("1,00")
  )), "invalid ISO week")
  expect_error(run(survstat_olap_response(
    geography = "LK Alpha", weeks = "2020-KW01", values = list("1.25")
  )), "decimal-comma")
  missing_values <- sub(
    "<b:Values>.*</b:Values>", "", survstat_olap_response(), perl = TRUE
  )
  expect_error(run(missing_values), "expected Values member")
})

test_that("malformed cube status and transport shapes fail", {
  expect_error(with_survstat_api_transport(
    survstat_api_transport(data_status = "not-a-date"),
    fetch_survstat_incidence("COVID-19", 2020, query_id = "q")
  ), "data status is malformed")
  expect_error(with_survstat_api_transport(
    function(...) "not structured",
    fetch_survstat_incidence("COVID-19", 2020, query_id = "q")
  ), "malformed response")
})
