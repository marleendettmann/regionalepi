test_that("dissertation indicator preserves its sole authoritative definition", {
  x <- dissertation_youth_dependency_ratio_spec()
  expect_no_error(validate_indicator_spec(x))
  expect_identical(x$indicator_id, "youth_dependency_ratio")
  expect_identical(x$indicator_type, "ratio")
  expect_identical(x$definition_version, "dissertation_v1")
  expect_identical(x$parameters$numerator$variable, "population")
  expect_identical(x$parameters$numerator$age_from, 0)
  expect_identical(x$parameters$numerator$age_to, 19)
  expect_identical(x$parameters$denominator$variable, "population")
  expect_identical(x$parameters$denominator$age_from, 20)
  expect_identical(x$parameters$denominator$age_to, 64)
  expect_identical(x$parameters$multiplier, 100)
  expect_identical(
    x$parameters$unit, "persons_under_20_per_100_persons_20_64"
  )
  expect_identical(x$parameters$interpretation, "quotient")
})

test_that("unknown indicator types are rejected", {
  x <- list(
    indicator_id = "example", indicator_type = "custom",
    definition_version = "v1", parameters = list(custom_parameter = 1)
  )
  expect_error(validate_indicator_spec(x), "source_provided, ratio, or density")
})

test_that("authoritative demographic source and density specs are exact", {
  mean_age <- dissertation_mean_age_spec()
  expect_identical(mean_age$indicator_type, "source_provided")
  expect_identical(mean_age$definition_version, "dissertation_v1")
  expect_identical(mean_age$parameters$table, "12411-07-01-4")
  expect_identical(mean_age$parameters$measure, "BEV519")
  expect_identical(mean_age$parameters$selection, list(sex = "Insgesamt"))
  expect_identical(mean_age$parameters$unit, "years")

  density <- dissertation_population_density_spec()
  expect_identical(density$indicator_type, "density")
  expect_identical(density$definition_version, "dissertation_v1")
  expect_identical(density$parameters$numerator,
                   list(variable = "population", unit = "persons"))
  expect_identical(density$parameters$denominator,
                   list(variable = "area", unit = "km2"))
  expect_identical(density$parameters$multiplier, 1)
  expect_identical(density$parameters$unit, "persons_per_km2")
})

test_that("indicator specs reject missing elements and malformed ratios", {
  x <- dissertation_youth_dependency_ratio_spec()
  x$parameters <- NULL
  expect_error(validate_indicator_spec(x), "missing required")
  x <- dissertation_youth_dependency_ratio_spec()
  x$parameters$unit <- NULL
  expect_error(validate_indicator_spec(x), "missing required")
  x <- dissertation_youth_dependency_ratio_spec()
  x$parameters$numerator$age_to <- -1
  expect_error(validate_indicator_spec(x), "non-negative|invalid")
  x <- dissertation_youth_dependency_ratio_spec()
  x$parameters$denominator$age_from <- 20.5
  expect_error(validate_indicator_spec(x), "whole-valued")
  x <- dissertation_youth_dependency_ratio_spec()
  x$parameters$denominator$age_from <- 65
  expect_error(validate_indicator_spec(x), "invalid age interval")
  x <- dissertation_youth_dependency_ratio_spec()
  x$indicator_id <- c("one", "two")
  expect_error(validate_indicator_spec(x), "one non-empty")
})

test_that("dissertation typology preserves every reference parameter", {
  x <- dissertation_typology_spec()
  expect_no_error(validate_typology_spec(x))
  expect_identical(x$typology_id, "dissertation_v1")
  expect_identical(x$definition_version, "dissertation_v1")
  expect_identical(x$reference_years, 2017:2020)
  expect_identical(x$temporal_aggregation, "arithmetic_mean")
  expect_identical(x$standardization, "z_score")
  expect_identical(x$method, "k_means")
  expect_identical(x$method_parameters$centers, 3L)
  expect_identical(x$method_parameters$nstart, 25L)
  expect_identical(x$method_parameters$algorithm, "Lloyd")
  expect_identical(x$method_parameters$iter.max, 50L)
  expect_identical(x$method_parameters$seed, 123L)
  expect_true(x$method_parameters$center)
  expect_true(x$method_parameters$scale)
  expect_identical(
    x$method_parameters$scale_semantics, "base_r_sample_sd_n_minus_1"
  )
  expect_identical(
    x$method_parameters$row_order, "ascending_five_character_geo_id"
  )
  expect_identical(
    x$method_parameters$label_mapping_scope,
    "historical_fitted_solution_only"
  )
  expect_identical(
    x$method_parameters$historical_label_mapping,
    list(
      list(raw_cluster = 1L, cluster_code = "ClD",
           cluster_label = "dichte Regionen"),
      list(raw_cluster = 3L, cluster_code = "ClJ",
           cluster_label = "familiengeprägte Regionen"),
      list(raw_cluster = 2L, cluster_code = "ClA",
           cluster_label = "ältere, ländliche Regionen")
    )
  )

  expect_identical(
    x$indicators[[1]],
    list(indicator_id = "population_density",
         definition_version = "dissertation_v1")
  )
  expect_identical(
    x$indicators[[2]],
    list(indicator_id = "mean_age", definition_version = "dissertation_v1")
  )
  youth <- x$indicators[[3]]
  expect_identical(youth$indicator_id, "youth_dependency_ratio")
  expect_identical(
    youth$definition_version,
    dissertation_youth_dependency_ratio_spec()$definition_version
  )
  expect_false(any(grepl("age", names(youth))))
})

test_that("typology specs require core elements and valid references", {
  x <- dissertation_typology_spec()
  x$method <- NULL
  expect_error(validate_typology_spec(x), "missing required")
  x <- dissertation_typology_spec()
  x$indicators <- list()
  expect_error(validate_typology_spec(x), "non-empty list")
  x <- dissertation_typology_spec()
  x$indicators[[1]]$indicator_id <- NULL
  expect_error(validate_typology_spec(x), "missing required")
  x <- dissertation_typology_spec()
  x$reference_years <- c(2017L, 2017L)
  expect_error(validate_typology_spec(x), "unique non-negative integers")
  x <- dissertation_typology_spec()
  x$method_parameters <- "not a list"
  expect_error(validate_typology_spec(x), "must be a list")
})

analysis_example <- function() {
  regionalepi:::new_analysis_spec(
    target_geo_level = "district",
    target_geo_vintage = as.Date("2020-12-31"),
    surveillance_source = "synthetic", surveillance_version = "v1",
    context_source = "synthetic", context_version = "v1",
    indicator_spec_version = "v1", typology_spec_version = "v1",
    package_version = "0.0.0.9000"
  )
}

test_that("analysis specs record complete versioned provenance", {
  x <- analysis_example()
  expect_no_error(validate_analysis_spec(x))
  x$surveillance_version <- NA_character_
  x$context_version <- NA_character_
  expect_no_error(validate_analysis_spec(x))
  x$package_version <- NULL
  expect_error(validate_analysis_spec(x), "missing required")
})

test_that("analysis specs reject malformed required provenance", {
  x <- analysis_example()
  x$target_geo_vintage <- "2020-12-31"
  expect_error(validate_analysis_spec(x), "must be Date")
  x <- analysis_example()
  x$target_geo_level <- NA_character_
  expect_error(validate_analysis_spec(x), "one non-empty")
  x <- analysis_example()
  x$surveillance_version <- ""
  expect_error(validate_analysis_spec(x), "non-empty character value or NA")
  x$surveillance_version <- 1
  expect_error(validate_analysis_spec(x), "non-empty character value or NA")
})
