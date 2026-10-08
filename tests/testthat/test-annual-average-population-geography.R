reviewed_average_population_fixture <- function() {
  ids <- c("01001", "16056", "16063", "01001", "16063")
  years <- c(2017L, 2017L, 2017L, 2021L, 2021L)
  data <- data.frame(
    geo_id = ids,
    geo_name = c("Flensburg", "Eisenach", "Wartburgkreis",
                 "Flensburg", "Wartburgkreis"),
    geo_level = "district", geo_vintage = as.Date(NA), year = years,
    population = c(90000, 42649, 124247, 91000, 159419),
    population_measure = "annual_average_population",
    population_reference = "reporting_year_annual_average",
    population_basis = rep("census_2011", length(years)),
    source = "Regionaldatenbank Deutschland",
    source_table = "12411-05-01-4",
    retrieved_at = as.POSIXct("2026-10-08 12:00:00", tz = "UTC"),
    data_status = "08.10.2026 / 12:00:00",
    provenance_id = "regional_average_population_12411-05-01-4_bev028",
    stringsAsFactors = FALSE
  )
  list(
    data = data,
    diagnostics = list(source_quality_markers = "-"),
    provenance = list(source = list(source_table = "12411-05-01-4"))
  )
}

reviewed_average_population_relations <- function() {
  data.frame(
    year = 2017L, from_geo_id = "16056", to_geo_id = "16063",
    relation_type = "historical_merge",
    source = "Regionaldatenbank Deutschland", note = "reviewed test relation",
    stringsAsFactors = FALSE
  )
}

test_that("reviewed denominator harmonization is additive and year-specific", {
  input <- reviewed_average_population_fixture()
  target <- data.frame(
    geo_id = c("01001", "16063"),
    geo_name = c("Flensburg", "Wartburgkreis"),
    stringsAsFactors = FALSE
  )
  before <- tapply(input$data$population, input$data$year, sum)
  result <- regionalepi:::.harmonize_reviewed_annual_average_population(
    input, reviewed_average_population_relations(), target
  )
  expect_identical(sort(unique(result$data$geo_id)), sort(target$geo_id))
  expect_identical(as.integer(table(result$data$year)), c(2L, 2L))
  expect_false("16056" %in% result$data$geo_id)
  expect_equal(result$data$population[
    result$data$year == 2017L & result$data$geo_id == "16063"
  ], 42649 + 124247)
  expect_equal(result$data$population[
    result$data$year == 2021L & result$data$geo_id == "16063"
  ], 159419)
  expect_identical(tapply(result$data$population, result$data$year, sum), before)
  expect_true(result$diagnostics$geography_harmonization$additive_mass_preserved)
  expect_true(result$provenance$source$geography_harmonization$applied_before_incidence)
})

test_that("reviewed denominator harmonization rejects unexpected geography", {
  input <- reviewed_average_population_fixture()
  input$data$geo_id[input$data$geo_id == "16056"] <- "99999"
  target <- data.frame(
    geo_id = c("01001", "16063"),
    geo_name = c("Flensburg", "Wartburgkreis"),
    stringsAsFactors = FALSE
  )
  expect_error(
    regionalepi:::.harmonize_reviewed_annual_average_population(
      input, reviewed_average_population_relations(), target
    ),
    "does not match"
  )
})
