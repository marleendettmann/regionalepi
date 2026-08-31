test_that("reviewed indicator set has exact ordered definitions", {
  x <- demographic_structure_spec()
  expect_identical(validate_indicator_set_spec(x), x)
  expect_identical(vapply(x$indicators, `[[`, character(1L), "indicator_id"), c(
    "population_density", "mean_age", "youth_dependency_ratio"
  ))
  expect_true(all(vapply(
    x$indicators, `[[`, character(1L), "definition_version"
  ) == "dissertation_v1"))
  expect_false("reference_years" %in% names(x))
})

test_that("indicator-set validation rejects unsafe variants", {
  x <- demographic_structure_spec(); x$indicators <- rev(x$indicators)
  expect_no_error(validate_indicator_set_spec(x))
  expect_false(identical(x, demographic_structure_spec()))
  x <- demographic_structure_spec(); x$indicators[[1L]]$indicator_id <- "unknown"
  expect_error(validate_indicator_set_spec(x), "unknown")
  x <- demographic_structure_spec(); x$indicators[[2L]]$indicator_id <- x$indicators[[1L]]$indicator_id
  expect_error(validate_indicator_set_spec(x), "unique")
  x <- demographic_structure_spec(); x$indicators[[1L]]$definition_version <- "other"
  expect_error(validate_indicator_set_spec(x), "authoritative")
  x <- demographic_structure_spec(); x$standardization$scale <- FALSE
  expect_error(validate_indicator_set_spec(x), "standardization")
})

test_that("dynamic fitting specification is explicit and independent", {
  x <- dynamic_kmeans_spec()
  expect_identical(validate_dynamic_fitting_spec(x), x)
  expect_identical(x$fitting_specification_id, "dynamic_kmeans_v1")
  expect_identical(x$algorithm, "Lloyd")
  expect_identical(x$nstart, 50L)
  expect_identical(x$iter_max, 100L)
  expect_identical(x$seed, 20241231L)
  expect_identical(x$supported_k, 2:5)
  expect_false(identical(x$nstart, dissertation_typology_spec()$method_parameters$nstart))
})

test_that("dynamic fit supports reviewed k values and rejects invalid k", {
  input <- dynamic_summary_example()
  fits <- lapply(2:5, function(k) fit_dynamic_typology(input, k = k))
  expect_identical(vapply(fits, function(x) x$diagnostics$k, integer(1L)), 2:5)
  expect_error(fit_dynamic_typology(input, k = 1), "one of 2")
  expect_error(fit_dynamic_typology(input, k = 6), "one of 2")
  expect_error(fit_dynamic_typology(dynamic_summary_example(3), k = 3), "smaller")
})

test_that("dynamic fit validates complete finite summary input", {
  x <- dynamic_summary_example(); x$data$indicator_value[x$data$indicator_id == "mean_age"] <- 1
  expect_error(fit_dynamic_typology(x), "non-zero sample")
  x <- dynamic_summary_example(); x$data$indicator_value[[1L]] <- NA_real_
  expect_error(fit_dynamic_typology(x), "NA|complete")
  x <- dynamic_summary_example(); x$data <- rbind(x$data, x$data[1L, ])
  expect_error(fit_dynamic_typology(x), "exactly once")
  x <- dynamic_summary_example(); x$data <- x$data[-1L, ]
  expect_error(fit_dynamic_typology(x), "geo_id set")
})

test_that("dynamic fit is deterministic, row-stable, and preserves RNG", {
  input <- dynamic_summary_example()
  shuffled <- input; shuffled$data <- shuffled$data[rev(seq_len(nrow(shuffled$data))), ]
  original <- shuffled
  set.seed(763); before <- .Random.seed
  one <- fit_dynamic_typology(input)
  two <- fit_dynamic_typology(shuffled)
  after <- .Random.seed
  expect_identical(one$assignments, two$assignments)
  expect_identical(one$profiles, two$profiles)
  expect_identical(one$provenance$fit_id, two$provenance$fit_id)
  expect_identical(before, after)
  expect_identical(shuffled, original)
  expect_identical(one$matrix$row_order, sort(unique(input$data$geo_id)))
})

test_that("fit identity responds only to stable analytical inputs", {
  input <- dynamic_summary_example()
  one <- fit_dynamic_typology(input)
  two <- fit_dynamic_typology(input)
  expect_identical(one$provenance$fit_id, two$provenance$fit_id)
  expect_identical(one$assignments$display_cluster_id,
                   two$assignments$display_cluster_id)
  later <- dynamic_summary_example(period = 2021:2024)
  expect_false(identical(
    one$provenance$fit_id, fit_dynamic_typology(later)$provenance$fit_id
  ))
  expect_false(identical(
    one$provenance$fit_id, fit_dynamic_typology(input, k = 4)$provenance$fit_id
  ))
})

test_that("raw and display identities remain separate and neutral", {
  fit <- fit_dynamic_typology(dynamic_summary_example())
  expect_setequal(fit$assignments$raw_cluster, 1:3)
  expect_setequal(fit$assignments$display_cluster_id, c("C01", "C02", "C03"))
  expect_false(any(grepl("ClD|ClJ|ClA|dichte|ältere|jüngere",
                         fit$assignments$display_cluster_id)))
  mapping <- fit$diagnostics$display_mapping
  centers <- fit$matrix$standardized
  observed <- vapply(split(seq_len(nrow(centers)), fit$assignments$raw_cluster),
                     function(rows) colMeans(centers[rows, , drop = FALSE]), numeric(3))
  observed <- t(observed)
  ordered <- do.call(order, c(lapply(seq_len(ncol(observed)), function(j) observed[, j]),
                              list(as.integer(rownames(observed)), method = "radix")))
  expect_identical(mapping$display_cluster_id,
                   sprintf("C%02d", match(mapping$raw_cluster, ordered)))
})

test_that("profiles reproduce means, medians, centers, ranks, and proportions", {
  fit <- fit_dynamic_typology(dynamic_summary_example())
  row <- fit$profiles[1L, ]
  members <- fit$assignments$raw_cluster == row$raw_cluster
  values <- fit$matrix$original[members, row$indicator_id]
  expect_equal(row$original_mean, mean(values), tolerance = 0)
  expect_equal(row$original_median, median(values), tolerance = 0)
  expect_equal(row$standardized_center,
               mean(fit$matrix$standardized[members, row$indicator_id]), tolerance = 1e-15)
  expect_equal(sum(unique(fit$profiles[c("display_cluster_id", "cluster_proportion")])$cluster_proportion), 1)
  for (indicator in unique(fit$profiles$indicator_id)) {
    expect_setequal(
      fit$profiles$indicator_rank[fit$profiles$indicator_id == indicator], 1:3
    )
  }
  for (cluster in unique(fit$profiles$display_cluster_id)) {
    profile <- fit$profiles[fit$profiles$display_cluster_id == cluster, ]
    expect_identical(profile$strongest_indicator_rank,
                     as.integer(rank(-profile$absolute_standardized_center,
                                     ties.method = "min")))
  }
})

test_that("minimum-size rule flags only and never refits", {
  fit <- fit_dynamic_typology(dynamic_summary_example(30), k = 5)
  expect_identical(fit$diagnostics$minimum_size_threshold, 5L)
  expect_identical(fit$cluster_diagnostics$minimum_size_warning,
                   fit$cluster_diagnostics$size < 5L)
  expect_identical(sum(fit$cluster_diagnostics$size), 30L)
})

test_that("dynamic partition comparison is label-neutral for same and different k", {
  input <- dynamic_summary_example()
  k3a <- fit_dynamic_typology(input, k = 3)
  k3b <- fit_dynamic_typology(input, k = 3)
  same <- compare_dynamic_partitions(k3a, k3b)
  expect_identical(same$adjusted_rand_index, 1)
  expect_false(same$semantic_mapping_performed)
  expect_false(same$historical_labels_applied)
  different <- compare_dynamic_partitions(k3a, fit_dynamic_typology(input, k = 4))
  expect_identical(different$x_k, 3L)
  expect_identical(different$y_k, 4L)
  expect_equal(sum(different$contingency_table), 30)
  bad <- dynamic_summary_example(); bad$data$geo_id[bad$data$geo_id == "00030"] <- "99999"
  expect_error(compare_dynamic_partitions(k3a, fit_dynamic_typology(bad, k = 3)),
               "same geo_id")
})
