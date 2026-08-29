regional_population_json <- function(
    table = "12411-01-01-4", statistic = "12411", status = 0L,
    codes = c("12411", "KREISE", "STAG", "GES", "BEVSTD"),
    rows = c("01001;      Flensburg;95000;47000;48000",
             "16063;      Wartburgkreis;154957;76953;78004"),
    date = "31.12.2024") {
  structure <- lapply(codes, function(code) list(Code = code))
  content <- paste(c(
    paste("Tabelle:", table),
    "Bevölkerung nach Geschlecht - Stichtag 31.12. - regionale;;;;",
    "Tiefe: Kreise und krfr. Städte;;;;",
    "Fortschreibung des Bevölkerungsstandes;;;;",
    "Bevölkerungsstand (Anzahl);;;;",
    paste0(";;Stichtag;Stichtag;Stichtag"),
    paste0(";;", date, ";", date, ";", date),
    ";;Insgesamt;männlich;weiblich",
    rows,
    "__________",
    "Synthetic methodological note.",
    "© Synthetic source",
    "Stand: 01.01.2025 / 12:00:00"
  ), collapse = "\n")
  jsonlite::toJSON(list(
    Ident = list(Service = "data", Method = "table"),
    Status = list(Code = status, Content = if (status == 0L) "erfolgreich" else "Fehler",
                  Type = "Information"),
    Parameter = list(name = table),
    Object = list(Content = content, Structure = structure),
    Copyright = "Synthetic attribution"
  ), auto_unbox = TRUE, null = "null")
}

with_regional_population_transport <- function(transport, code) {
  namespace <- asNamespace("regionalepi")
  old <- get(".regional_population_transport", envir = namespace)
  unlockBinding(".regional_population_transport", namespace)
  assign(".regional_population_transport", transport, envir = namespace)
  lockBinding(".regional_population_transport", namespace)
  on.exit({
    unlockBinding(".regional_population_transport", namespace)
    assign(".regional_population_transport", old, envir = namespace)
    lockBinding(".regional_population_transport", namespace)
  }, add = TRUE)
  withr_env <- c(
    REGIONALSTATISTIK_USER = Sys.getenv("REGIONALSTATISTIK_USER", unset = NA_character_),
    REGIONALSTATISTIK_PASSWORD = Sys.getenv("REGIONALSTATISTIK_PASSWORD", unset = NA_character_)
  )
  on.exit({
    for (name in names(withr_env)) {
      if (is.na(withr_env[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, setNames(list(withr_env[[name]]), name))
    }
  }, add = TRUE)
  Sys.setenv(REGIONALSTATISTIK_USER = "synthetic-user",
             REGIONALSTATISTIK_PASSWORD = "synthetic-password")
  force(code)
}

test_that("fetch_regional_population returns validated data and safe metadata", {
  transport <- function(reference_dates, regions, credentials) {
    expect_identical(reference_dates, as.Date("2024-12-31"))
    expect_null(regions)
    expect_named(credentials, c("username", "password"))
    regional_population_json()
  }
  result <- with_regional_population_transport(
    transport, fetch_regional_population(as.Date("2024-12-31"))
  )
  expect_named(result, c("data", "diagnostics", "provenance"), ignore.order = FALSE)
  expect_identical(validate_population_denominator(result$data), result$data)
  expect_identical(result$data$geo_id, c("01001", "16063"))
  expect_identical(result$data$geo_name, c("Flensburg", "Wartburgkreis"))
  expect_identical(result$data$population, c(95000, 154957))
  expect_true(all(result$data$population_basis == "census_2022"))
  expect_true(all(result$data$data_status == "01.01.2025 / 12:00:00"))
  expect_true(all(is.na(result$data$geo_vintage)))
  expect_identical(result$diagnostics$returned_region_count, 2L)
  expect_identical(result$diagnostics$returned_observation_count, 2L)
  expect_true(result$diagnostics$geo_vintage_unresolved)
  expect_identical(result$provenance$source_notes, "Synthetic methodological note.")
  printed <- paste(capture.output(str(result)), collapse = "\n")
  expect_false(grepl("synthetic-user|synthetic-password", printed))
})

test_that("population expands Regionaldatenbank city-state keys", {
  transport <- function(...) regional_population_json(rows = c(
    "02; Hamburg;1900000;1;1",
    "11; Berlin;3700000;1;1"
  ))
  result <- with_regional_population_transport(
    transport, fetch_regional_population(as.Date("2024-12-31"))
  )
  expect_identical(result$data$geo_id, c("02000", "11000"))
  expect_identical(result$data$geo_name, c("Hamburg", "Berlin"))
})

test_that("request dates and explicit regions are validated", {
  check <- regionalepi:::.validate_regional_population_request
  expect_no_error(check(as.Date("2019-12-31"), NULL))
  expect_no_error(check(as.Date("2017-12-31"), NULL))
  expect_no_error(check(as.Date(c("2019-12-31", "2025-12-31")), c("01001", "16063")))
  expect_error(check("2024-12-31", NULL), "Date")
  expect_error(check(as.Date("2024-12-30"), NULL), "31 December")
  expect_error(check(as.Date(c("2024-12-31", "2024-12-31")), NULL), "duplicates")
  expect_no_error(check(as.Date("2018-12-31"), NULL))
  expect_error(check(as.Date("2016-12-31"), NULL), "2017-2025")
  expect_error(check(as.Date("2024-12-31"), 16063), "character")
  expect_error(check(as.Date("2024-12-31"), c("16063", "16063")), "duplicates")
  expect_error(check(as.Date("2024-12-31"), "1063"), "five-character")
})

test_that("credentials must exist before the transport boundary", {
  old_user <- Sys.getenv("REGIONALSTATISTIK_USER", unset = NA_character_)
  old_password <- Sys.getenv("REGIONALSTATISTIK_PASSWORD", unset = NA_character_)
  on.exit({
    if (is.na(old_user)) Sys.unsetenv("REGIONALSTATISTIK_USER") else Sys.setenv(REGIONALSTATISTIK_USER = old_user)
    if (is.na(old_password)) Sys.unsetenv("REGIONALSTATISTIK_PASSWORD") else Sys.setenv(REGIONALSTATISTIK_PASSWORD = old_password)
  })
  Sys.unsetenv(c("REGIONALSTATISTIK_USER", "REGIONALSTATISTIK_PASSWORD"))
  expect_error(fetch_regional_population(as.Date("2024-12-31")),
               "environment variables are missing or empty")
  Sys.setenv(REGIONALSTATISTIK_USER = "", REGIONALSTATISTIK_PASSWORD = "")
  expect_error(fetch_regional_population(as.Date("2024-12-31")),
               "environment variables are missing or empty")
})

test_that("API envelope and Structure metadata are enforced", {
  parse <- regionalepi:::.regional_population_result
  args <- list(reference_dates = as.Date("2024-12-31"), regions = NULL,
               retrieved_at = as.POSIXct("2025-01-01", tz = "UTC"))
  call <- function(response) do.call(parse, c(list(response = response), args))
  expect_error(call("not json"), "not valid JSON")
  expect_error(call(regional_population_json(status = 5L)), "status is not successful")
  expect_error(call(regional_population_json(table = "wrong")), "expected source table")
  expect_error(call(regional_population_json(statistic = "wrong", codes = c("wrong", "KREISE", "STAG", "GES", "BEVSTD"))), "missing expected code.*12411")
  expect_error(call(regional_population_json(codes = c("12411", "KREISE", "STAG", "GES"))), "BEVSTD")
})

test_that("content parser rejects incomplete or unsafe observations", {
  parse <- regionalepi:::.regional_population_result
  call <- function(response, regions = NULL) parse(
    response, as.Date("2024-12-31"), regions,
    as.POSIXct("2025-01-01", tz = "UTC")
  )
  duplicate <- c("16063; Wartburgkreis;154957;1;1",
                 "16063; Wartburgkreis;154957;1;1")
  expect_error(call(regional_population_json(rows = duplicate)), "duplicate")
  expect_error(call(regional_population_json(rows = "16063; Wartburgkreis;.;1;1")),
               "quality-marked")
  expect_error(call(regional_population_json(rows = "16063; Wartburgkreis;;1;1")),
               "quality-marked")
  expect_error(call(regional_population_json(rows = "16063; Wartburgkreis;not-a-number;1;1")),
               "quality-marked")
  expect_error(call(regional_population_json(), regions = "99999"), "absent from response")

  structural <- regional_population_json(rows = c(
    "01001; Flensburg;-;1;1",
    "16063; Wartburgkreis;154957;1;1"
  ))
  result <- call(structural)
  expect_identical(result$data$geo_id, "16063")
  expect_identical(result$diagnostics$source_quality_markers, "-")
  expect_error(call(structural, regions = "01001"), "no population")
})

test_that("multiple dates and population basis codes are reproducible", {
  content_rows <- c(
    "01001; Flensburg;90000;1;1;95000;1;1",
    "16063; Wartburgkreis;150000;1;1;154957;1;1"
  )
  response <- regional_population_json(rows = content_rows)
  object <- jsonlite::fromJSON(response, simplifyVector = FALSE)
  object$Object$Content <- sub(
    ";;31.12.2024;31.12.2024;31.12.2024",
    ";;31.12.2021;31.12.2021;31.12.2021;31.12.2024;31.12.2024;31.12.2024",
    object$Object$Content, fixed = TRUE
  )
  object$Object$Content <- sub(
    ";;Insgesamt;männlich;weiblich",
    ";;Insgesamt;männlich;weiblich;Insgesamt;männlich;weiblich",
    object$Object$Content, fixed = TRUE
  )
  response <- jsonlite::toJSON(object, auto_unbox = TRUE, null = "null")
  transport <- function(...) response
  result <- with_regional_population_transport(
    transport,
    fetch_regional_population(as.Date(c("2021-12-31", "2024-12-31")))
  )
  expect_equal(nrow(result$data), 4L)
  expect_identical(sort(unique(result$data$population_basis)),
                   c("census_2011", "census_2022"))
  expect_identical(result$data$population, c(90000, 150000, 95000, 154957))
})

test_that("transport errors and returned metadata never expose credentials", {
  transport <- function(...) stop("synthetic-user synthetic-password")
  message <- tryCatch(
    with_regional_population_transport(
      transport, fetch_regional_population(as.Date("2024-12-31"))
    ),
    error = conditionMessage
  )
  expect_false(grepl("synthetic-user|synthetic-password", message))
})
