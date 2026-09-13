annual_average_population_json <- function(
    rows_2022 = c(
      "02;Hamburg;1900000;1;1",
      "11;Berlin;3700000;1;1",
      "07315;      Mainz, kreisfreie Stadt;220028;106901;113128",
      "16069;Hildburghausen;61318;1;1",
      "16056;Eisenach, kreisfreie Stadt;-;-;-"
    ),
    rows_2025 = c(
      "02;Hamburg;1910000;1;1",
      "11;Berlin;3750000;1;1",
      "07315;      Mainz, kreisfreie Stadt;224286;1;1",
      "16069;Hildburghausen;59637;1;1",
      "16056;Eisenach, kreisfreie Stadt;-;-;-"
    ),
    codes = c("12411", "BEV028", "GES", "KREISE", "JAHR"),
    table = "12411-05-01-4", source_notes = c(
      "Bei den Bevölkerungsdaten handelt es sich um Fortschreibungszahlen, die ab dem Berichtsmonat Mai 2022 auf",
      "den Ergebnissen des Zensus vom 15. Mai 2022 basieren."
    )) {
  content <- paste(c(
    paste("Tabelle:", table),
    "Durchschnittliche Jahresbevölkerung nach Geschlecht - Jahresdurchschnitt",
    "Fortschreibung des Bevölkerungsstandes",
    "Durchschnittliche Jahresbevölkerung (Anzahl)",
    ";;;Insgesamt;männlich;weiblich",
    paste0("2025;", rows_2025), paste0("2022;", rows_2022),
    "______________", source_notes, "© Synthetic source",
    "Stand: 13.09.2026 / 20:57:21"
  ), collapse = "\n")
  jsonlite::toJSON(list(
    Ident = list(Service = "data", Method = "table"),
    Status = list(Code = 0L, Content = "erfolgreich"),
    Parameter = list(name = table),
    Object = list(
      Content = content,
      Structure = list(
        Head = list(
          Code = codes[[1L]], Content = "Fortschreibung des Bevölkerungsstandes",
          Structure = list(list(
            Code = if (length(codes) >= 2L) codes[[2L]] else NA_character_,
            Content = "Durchschnittliche Jahresbevölkerung"
          ))
        ),
        Columns = list(list(
          Code = if (length(codes) >= 3L) codes[[3L]] else NA_character_,
          Content = "Geschlecht"
        )),
        Rows = list(list(
          Code = if (length(codes) >= 4L) codes[[4L]] else NA_character_,
          Content = "Kreise und kreisfreie Städte"
        )),
        Subtitel = NULL,
        Subheading = list(
          Code = if (length(codes) >= 5L) codes[[5L]] else NA_character_,
          Content = "Jahr"
        )
      )
    ),
    Copyright = "Synthetic attribution"
  ), auto_unbox = TRUE, null = "null")
}

with_regional_average_population_transport <- function(transport, code) {
  namespace <- asNamespace("regionalepi")
  old <- get(".regional_average_population_transport", envir = namespace)
  unlockBinding(".regional_average_population_transport", namespace)
  assign(".regional_average_population_transport", transport, envir = namespace)
  lockBinding(".regional_average_population_transport", namespace)
  on.exit({
    unlockBinding(".regional_average_population_transport", namespace)
    assign(".regional_average_population_transport", old, envir = namespace)
    lockBinding(".regional_average_population_transport", namespace)
  }, add = TRUE)
  old_user <- Sys.getenv("REGIONALSTATISTIK_USER", unset = NA_character_)
  old_password <- Sys.getenv("REGIONALSTATISTIK_PASSWORD", unset = NA_character_)
  on.exit({
    if (is.na(old_user)) Sys.unsetenv("REGIONALSTATISTIK_USER") else
      Sys.setenv(REGIONALSTATISTIK_USER = old_user)
    if (is.na(old_password)) Sys.unsetenv("REGIONALSTATISTIK_PASSWORD") else
      Sys.setenv(REGIONALSTATISTIK_PASSWORD = old_password)
  }, add = TRUE)
  Sys.setenv(
    REGIONALSTATISTIK_USER = "synthetic-user",
    REGIONALSTATISTIK_PASSWORD = "synthetic-password"
  )
  force(code)
}

annual_average_population_example <- function() {
  data.frame(
    geo_id = c("07315", "16069"),
    geo_name = c("Mainz", "Hildburghausen"),
    geo_level = "district", geo_vintage = as.Date(NA),
    year = c(2022L, 2022L), population = c(220028, 61318),
    population_measure = "annual_average_population",
    population_reference = "reporting_year_annual_average",
    population_basis = "census_2022",
    source = "Regionaldatenbank Deutschland",
    source_table = "12411-05-01-4",
    retrieved_at = as.POSIXct("2026-09-13 20:57:21", tz = "UTC"),
    data_status = "13.09.2026 / 20:57:21",
    provenance_id = "regional_average_population_12411-05-01-4_bev028",
    stringsAsFactors = FALSE
  )
}

test_that("annual-average population fetch selects the reviewed table and measure", {
  transport <- function(years, regions, credentials) {
    expect_identical(years, c(2022L, 2025L))
    expect_null(regions)
    expect_named(credentials, c("username", "password"))
    annual_average_population_json()
  }
  result <- with_regional_average_population_transport(
    transport, fetch_regional_average_population(c(2025, 2022))
  )
  expect_named(result, c("data", "diagnostics", "provenance"), ignore.order = FALSE)
  expect_identical(validate_annual_average_population(result$data), result$data)
  expect_identical(sort(unique(result$data$year)), c(2022L, 2025L))
  expect_identical(sort(unique(result$data$geo_id)),
                   c("02000", "07315", "11000", "16069"))
  expect_false("16056" %in% result$data$geo_id)
  expect_true(all(result$data$population_measure == "annual_average_population"))
  expect_true(all(result$data$population_reference == "reporting_year_annual_average"))
  expect_true(all(result$data$population_basis == "census_2022"))
  expect_true(all(is.na(result$data$geo_vintage)))
  expect_identical(result$diagnostics$source_measure, "BEV028")
  expect_identical(result$diagnostics$source_quality_markers, "-")
  provenance <- result$provenance[[result$data$provenance_id[[1L]]]]
  expect_identical(provenance$selection$sex, "Insgesamt")
  expect_identical(provenance$requested_years, c(2022L, 2025L))
  expect_identical(provenance$data_status, "13.09.2026 / 20:57:21")
  printed <- paste(capture.output(str(result)), collapse = "\n")
  expect_false(grepl("synthetic-user|synthetic-password", printed))
})

test_that("annual-average year parser handles structural absence and exact coverage", {
  parse <- regionalepi:::.regional_average_population_result
  call <- function(response, years = c(2022L, 2025L), regions = NULL) {
    parse(response, years, regions, as.POSIXct("2026-09-14", tz = "UTC"))
  }
  result <- call(annual_average_population_json())
  expect_identical(nrow(result$data), 8L)
  expect_identical(result$data$population[result$data$geo_id == "07315" &
      result$data$year == 2022L], 220028)
  expect_identical(result$data$geo_name[result$data$geo_id == "07315" &
      result$data$year == 2022L], "Mainz, kreisfreie Stadt")
  expect_error(call(annual_average_population_json(), regions = "16056"),
               "no annual-average population")
  expect_error(call(annual_average_population_json(), years = 2023L),
               "requested years are absent")
})

test_that("annual-average source structure, total-sex column, and basis evidence are strict", {
  parse <- regionalepi:::.regional_average_population_result
  call <- function(response) parse(
    response, c(2022L, 2025L), NULL,
    as.POSIXct("2026-09-14", tz = "UTC")
  )
  expect_error(call(annual_average_population_json(
    codes = c("12411", "GES", "KREISE", "JAHR"))), "BEV028")
  expect_error(call(annual_average_population_json(
    codes = c("12411", "BEV028", "GES", "KREISE"))), "JAHR")
  expect_error(call(annual_average_population_json(
    codes = c("12411", "BEV028", "KREISE", "GES", "JAHR"))),
    "reviewed roles")
  wrong_header <- annual_average_population_json()
  object <- jsonlite::fromJSON(wrong_header, simplifyVector = FALSE)
  object$Object$Content <- sub(";Insgesamt;", ";männlich;", object$Object$Content,
                               fixed = TRUE)
  expect_error(call(jsonlite::toJSON(object, auto_unbox = TRUE, null = "null")),
               "sex Insgesamt")
  expect_error(call(annual_average_population_json(
    source_notes = "No reviewed population-basis statement.")), "do not establish")
})

test_that("annual-average population rejects duplicate and invalid values", {
  parse <- regionalepi:::.regional_average_population_result
  call <- function(response) parse(
    response, c(2022L, 2025L), NULL,
    as.POSIXct("2026-09-14", tz = "UTC")
  )
  duplicate <- c("07315;Mainz;220028;1;1", "07315;Mainz;220028;1;1")
  expect_error(call(annual_average_population_json(rows_2022 = duplicate)), "duplicate")
  expect_error(call(annual_average_population_json(
    rows_2022 = "07315;Mainz;.;1;1")), "quality-marked")
  expect_error(call(annual_average_population_json(
    rows_2022 = "07315;Mainz;0;1;1")), "strictly positive")
})

test_that("annual-average request and contract reject unsafe inputs", {
  check <- regionalepi:::.validate_regional_average_population_request
  expect_identical(check(c(2025, 2022), NULL)$years, c(2022L, 2025L))
  expect_error(check(character(), NULL), "whole numbers")
  expect_error(check(2022.5, NULL), "whole numbers")
  expect_error(check(c(2022, 2022), NULL), "duplicates")
  expect_error(check(2021, NULL), "2022-2025")
  expect_error(check(2022, 7315), "five-character")
  expect_error(check(2022, "7315"), "five-character")

  x <- annual_average_population_example()
  expect_identical(validate_annual_average_population(x), x)
  expect_error(validate_annual_average_population(transform(x, year = 2022.5)),
               "whole-valued")
  expect_error(validate_annual_average_population(transform(x, population = c(0, 1))),
               "strictly positive")
  expect_error(validate_annual_average_population(rbind(x, x[1L, ])), "unique")
  expect_error(validate_annual_average_population(transform(
    x, population_measure = "year_end_population")), "annual_average_population")
})

test_that("annual-average transport errors never expose credentials", {
  transport <- function(...) stop("synthetic-user synthetic-password")
  message <- tryCatch(
    with_regional_average_population_transport(
      transport, fetch_regional_average_population(2022)
    ),
    error = conditionMessage
  )
  expect_false(grepl("synthetic-user|synthetic-password", message))
})

test_that("year-end population path remains scientifically distinct", {
  expect_identical(names(formals(fetch_regional_population)),
                   c("reference_dates", "regions"))
  expect_identical(names(formals(fetch_regional_average_population)),
                   c("years", "regions"))
  expect_false(identical(
    body(fetch_regional_population), body(fetch_regional_average_population)
  ))
})
