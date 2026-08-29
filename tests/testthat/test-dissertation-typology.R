typology_summary_example <- function(ids = sprintf("%05d", 1:9)) {
  profiles <- data.frame(
    population_density = c(10, 11, 12, 50, 51, 52, 90, 91, 92),
    mean_age = c(30, 31, 29, 50, 49, 51, 40, 39, 41),
    youth_dependency_ratio = c(20, 21, 19, 25, 24, 26, 50, 49, 51)
  )
  contract <- data.frame(
    indicator_id = c(
      "population_density", "mean_age", "youth_dependency_ratio"
    ),
    indicator_unit = c(
      "persons_per_km2", "years", "persons_under_20_per_100_persons_20_64"
    ), stringsAsFactors = FALSE
  )
  rows <- do.call(rbind, lapply(seq_len(nrow(contract)), function(i) {
    data.frame(
      geo_id = ids, geo_name = paste("District", ids), geo_level = "district",
      indicator_id = contract$indicator_id[[i]],
      indicator_value = profiles[[contract$indicator_id[[i]]]],
      indicator_unit = contract$indicator_unit[[i]],
      definition_version = "dissertation_v1",
      period_start = as.Date("2017-12-31"), period_end = as.Date("2020-12-31"),
      aggregation_method = "arithmetic_mean", annual_observation_count = 4L,
      provenance_id = paste0("summary-", contract$indicator_id[[i]]),
      stringsAsFactors = FALSE
    )
  }))
  list(data = rows, diagnostics = list(), provenance = list())
}

test_that("dissertation matrix construction fixes indicator and AGS order", {
  input <- typology_summary_example()
  input$data <- input$data[rev(seq_len(nrow(input$data))), ]
  fit <- fit_dissertation_typology(input)
  expect_identical(
    fit$matrix$indicator_order,
    c("population_density", "mean_age", "youth_dependency_ratio")
  )
  expect_identical(
    colnames(fit$matrix$data),
    c("Einwohnerdichte", "Durchschnittsalter", "AbhaengigenquoteJunge")
  )
  expect_identical(rownames(fit$matrix$data), sprintf("%05d", 1:9))
  expect_identical(fit$data$geo_id, sprintf("%05d", 1:9))
  expect_identical(fit$matrix$data["00001", "Einwohnerdichte"], 10)
})

test_that("explicit scaling exactly reproduces base scale sample semantics", {
  fit <- fit_dissertation_typology(typology_summary_example())
  expect_equal(fit$matrix$center, colMeans(fit$matrix$data), tolerance = 0)
  expect_equal(fit$matrix$scale, apply(fit$matrix$data, 2, sd), tolerance = 0)
  expected <- scale(fit$matrix$data)
  attributes(expected)[c("scaled:center", "scaled:scale")] <- NULL
  expect_equal(fit$matrix$standardized, expected, tolerance = 1e-15)
  expect_equal(unname(colMeans(fit$matrix$standardized)), rep(0, 3),
               tolerance = 1e-14)
  expect_equal(unname(apply(fit$matrix$standardized, 2, sd)), rep(1, 3),
               tolerance = 1e-14)
})

test_that("zero variance and incomplete indicator sets fail explicitly", {
  input <- typology_summary_example()
  input$data$indicator_value[input$data$indicator_id == "mean_age"] <- 1
  expect_error(fit_dissertation_typology(input), "non-zero sample")
  input <- typology_summary_example()
  input$data <- input$data[input$data$indicator_id != "mean_age", ]
  expect_error(fit_dissertation_typology(input), "three exact")
  input <- typology_summary_example()
  input$data <- rbind(input$data, input$data[1, ])
  expect_error(fit_dissertation_typology(input), "exactly once")
})

test_that("unit, version, and period incompatibilities fail explicitly", {
  input <- typology_summary_example(); input$data$indicator_unit[[1]] <- "other"
  expect_error(fit_dissertation_typology(input), "units or definition")
  input <- typology_summary_example(); input$data$definition_version[[1]] <- "v2"
  expect_error(fit_dissertation_typology(input), "units or definition")
  input <- typology_summary_example(); input$data$period_end[[1]] <- as.Date("2019-12-31")
  expect_error(fit_dissertation_typology(input), "period definition")
  input <- typology_summary_example(); input$data$annual_observation_count[[1]] <- 3L
  expect_error(fit_dissertation_typology(input), "period definition")
  input <- typology_summary_example(); input$data$geo_id[[1]] <- "1"
  expect_error(fit_dissertation_typology(input), "five-character AGS")
})

test_that("fit is deterministic, preserves input and restores RNG state", {
  input <- typology_summary_example()
  original <- input
  set.seed(987); before <- .Random.seed
  one <- fit_dissertation_typology(input)
  after <- .Random.seed
  two <- fit_dissertation_typology(input)
  expect_identical(one$data$raw_cluster, two$data$raw_cluster)
  expect_identical(one$diagnostics$centers, two$diagnostics$centers)
  expect_identical(before, after)
  expect_identical(input, original)
})

test_that("historical fitted-solution labels require explicit request", {
  plain <- fit_dissertation_typology(typology_summary_example())
  historical <- fit_dissertation_typology(
    typology_summary_example(), label_mapping = "historical_reference"
  )
  expect_true(all(is.na(plain$data$cluster_code)))
  expect_true(all(is.na(plain$data$cluster_label)))
  expect_setequal(historical$data$cluster_code, c("ClD", "ClJ", "ClA"))
  expect_identical(
    historical$diagnostics$label_mapping, "historical_reference"
  )
  expect_error(
    fit_dissertation_typology(typology_summary_example(), label_mapping = "automatic"),
    "arg"
  )
})

reference_from_fit <- function(fit, permutation = c("ClJ", "ClA", "ClD")) {
  data.frame(
    geo_id = fit$data$geo_id,
    cluster_code = permutation[fit$data$raw_cluster],
    stringsAsFactors = FALSE
  )
}

test_that("comparison aligns arbitrary permutations with exact agreement", {
  fit <- fit_dissertation_typology(typology_summary_example())
  reference <- reference_from_fit(fit)
  comparison <- compare_typology(fit, reference)
  expect_identical(comparison$exact_agreement, 9L)
  expect_identical(comparison$disagreement, 0L)
  expect_identical(comparison$agreement_proportion, 1)
  expect_identical(comparison$adjusted_rand_index, 1)
  expect_identical(unname(diag(comparison$confusion_matrix)), c(3L, 3L, 3L))
  expect_identical(comparison$changed_geo_ids, character())
})

test_that("comparison reports partial disagreement and ID-set differences", {
  fit <- fit_dissertation_typology(typology_summary_example())
  reference <- reference_from_fit(fit)
  reference$cluster_code[[1]] <- reference$cluster_code[[4]]
  reference <- rbind(
    reference[reference$geo_id != "00009", ],
    data.frame(geo_id = "99999", cluster_code = "ClD")
  )
  comparison <- compare_typology(fit, reference)
  expect_identical(comparison$ids_only_in_candidate, "00009")
  expect_identical(comparison$ids_only_in_reference, "99999")
  expect_identical(comparison$exact_agreement, 7L)
  expect_identical(comparison$disagreement, 1L)
  expect_identical(comparison$changed_geo_ids, "00001")
  expect_equal(sum(comparison$confusion_matrix), 8)
  expect_lt(comparison$adjusted_rand_index, 1)
})

test_that("comparison validates candidate and reference partitions", {
  fit <- fit_dissertation_typology(typology_summary_example())
  reference <- reference_from_fit(fit)
  bad <- fit; bad$data$raw_cluster[bad$data$raw_cluster == 3L] <- 2L
  expect_error(compare_typology(bad, reference), "raw clusters 1, 2, 3")
  reference$geo_id[[2]] <- reference$geo_id[[1]]
  expect_error(compare_typology(fit, reference), "unique")
})
