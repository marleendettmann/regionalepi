population_density_inputs <- function() {
  retrieved <- as.POSIXct("2025-01-01", tz = "UTC")
  population <- data.frame(
    geo_id = c("01001", "02000"), geo_name = c("Flensburg", "Hamburg"),
    geo_level = "district", geo_vintage = as.Date(NA),
    reference_date = as.Date("2020-12-31"), population = c(100, 200),
    population_basis = "census_2011", source = "Regionaldatenbank Deutschland",
    source_table = "12411-01-01-4", retrieved_at = retrieved,
    data_status = "synthetic", stringsAsFactors = FALSE
  )
  area <- regional_area_example()
  area$area_km2 <- c(2, 4)
  list(
    population = list(data = population, diagnostics = list(),
                      provenance = list(source_table = "12411-01-01-4")),
    area = list(data = area, diagnostics = list(),
                provenance = list(
                  "area-source" = list(source_table = "11111-01-01-4")
                ))
  )
}

test_that("population density uses exact identifiers and performs no rounding", {
  input <- population_density_inputs()
  result <- derive_population_density(input$population, input$area)
  expect_identical(validate_demographic_indicator(result$data), result$data)
  expect_identical(result$data$indicator_value, c(50, 50))
  expect_true(all(result$data$value_origin == "derived"))
  expect_true(all(result$data$indicator_unit == "persons_per_km2"))
  expect_false(result$diagnostics$rounding_performed)
  expect_true(result$diagnostics$geo_vintage_unresolved)
  provenance <- result$provenance[[result$data$provenance_id[[1L]]]]
  expect_identical(provenance$population_source$source_table, "12411-01-01-4")
  expect_identical(provenance$area_source$source_table, "11111-01-01-4")
})

test_that("density rejects key, name, vintage, and area incompatibility", {
  input <- population_density_inputs()
  input$area$data$geo_id[[1L]] <- "99999"
  expect_error(derive_population_density(input$population, input$area), "keys")
  input <- population_density_inputs(); input$area$data$geo_name[[1L]] <- "Other"
  expect_error(derive_population_density(input$population, input$area), "names")
  input <- population_density_inputs()
  input$area$data$geo_vintage <- as.Date("2020-12-31")
  expect_error(derive_population_density(input$population, input$area), "vintages")
  input <- population_density_inputs(); input$area$data$area_km2[[1L]] <- 0
  expect_error(derive_population_density(input$population, input$area),
               "strictly positive")
})

age_validation_inputs <- function() {
  source <- demographic_example(ids = "16063", years = 2020,
                                indicator_id = "youth_dependency_ratio",
                                unit = "persons_under_20_per_100_persons_20_64")
  source$indicator_value <- 44.4
  groups <- data.frame(
    geo_id = "16063", geo_name = "Name 16063", geo_level = "district",
    geo_vintage = as.Date(NA), reference_date = as.Date("2020-12-31"),
    age_from = seq(0L, 95L, 5L), age_to = c(seq(4L, 94L, 5L), NA_integer_),
    population = 100, population_basis = "census_2011", source = "synthetic",
    sex = "total", stringsAsFactors = FALSE
  )
  list(
    source = list(data = source, diagnostics = list(), provenance = list()),
    ages = list(data = groups, diagnostics = list(), provenance = list())
  )
}

test_that("age groups reproduce but never replace authoritative youth quotient", {
  input <- age_validation_inputs()
  original <- input$source$data
  result <- regionalepi:::.validate_youth_dependency_reproduction(
    input$source, input$ages
  )
  expect_true(result$all_matched)
  expect_identical(result$matched_observations, 1L)
  expect_equal(result$comparison$derived_value, 400 / 900 * 100)
  expect_identical(input$source$data, original)
  input$source$data$indicator_value <- 44.5
  expect_error(
    regionalepi:::.validate_youth_dependency_reproduction(input$source, input$ages),
    "not reproduced"
  )
  input <- age_validation_inputs()
  input$ages$data$geo_name <- "Different name"
  expect_error(
    regionalepi:::.validate_youth_dependency_reproduction(input$source, input$ages),
    "geography must agree"
  )
})

test_that("age validation rejects gaps and overlaps in 0-64 coverage", {
  input <- age_validation_inputs()
  input$ages$data <- input$ages$data[input$ages$data$age_from != 20L, ]
  expect_error(
    regionalepi:::.validate_youth_dependency_reproduction(input$source, input$ages),
    "without gaps"
  )
  input <- age_validation_inputs()
  input$ages$data$age_from[input$ages$data$age_from == 20L] <- 15L
  expect_error(
    regionalepi:::.validate_youth_dependency_reproduction(input$source, input$ages),
    "without gaps"
  )
})

three_indicators <- function(ids = c("01001", "16063"), years = 2017:2020) {
  mean_age <- demographic_example(ids, years, "mean_age", "years")
  youth <- demographic_example(
    ids, years, "youth_dependency_ratio",
    "persons_under_20_per_100_persons_20_64"
  )
  density <- demographic_example(
    ids, years, "population_density", "persons_per_km2", "derived"
  )
  regionalepi:::.combine_demographic_indicators(mean_age, youth, density)
}

test_that("period summary uses explicit unweighted complete annual observations", {
  annual <- three_indicators()
  typology <- dissertation_typology_spec()
  result <- summarize_indicator_period(
    annual, typology$reference_years, typology$temporal_aggregation
  )
  expect_identical(nrow(annual), 24L)
  expect_identical(nrow(result$data), 6L)
  expect_true(all(result$data$annual_observation_count == 4L))
  expect_true(all(result$data$aggregation_method == "arithmetic_mean"))
  expect_false(result$diagnostics$missing_values_removed)
  expect_false(result$diagnostics$weighting_performed)
  one <- result$data[result$data$geo_id == "01001" &
                       result$data$indicator_id == "mean_age", ]
  expect_identical(one$indicator_value, mean(1:4))
  expect_identical(
    result$provenance$arithmetic_mean_mean_age_dissertation_v1$input_provenance_ids,
    "p-mean_age"
  )
})

test_that("period provenance does not assume a dissertation version string", {
  annual <- three_indicators()
  annual$definition_version <- "reviewed_v2"
  result <- summarize_indicator_period(annual, 2017:2020)
  expect_identical(
    result$provenance$arithmetic_mean_mean_age_reviewed_v2$input_provenance_ids,
    "p-mean_age"
  )
})

test_that("period summary fails on missing years, NA, or geography mismatch", {
  annual <- three_indicators()
  expect_error(
    summarize_indicator_period(
      annual[annual$reference_date != as.Date("2018-12-31"), ], 2017:2020
    ),
    "reference years"
  )
  annual <- three_indicators(); annual$indicator_value[[1L]] <- NA_real_
  expect_error(summarize_indicator_period(annual, 2017:2020), "must not contain NA")
  annual <- three_indicators(); annual$geo_id[[1L]] <- "99999"
  expect_error(summarize_indicator_period(annual, 2017:2020),
               "identifier sets|one compatible row")
  expect_error(summarize_indicator_period(three_indicators(), 2017:2020, "median"),
               "only arithmetic_mean")
})

test_that("generic contracts and period summary do not enforce 401 districts", {
  annual <- three_indicators(ids = c("01001", "02000", "16056"))
  expect_no_error(validate_demographic_indicator(annual))
  result <- summarize_indicator_period(annual, 2017:2020)
  expect_identical(nrow(result$data), 9L)
})
