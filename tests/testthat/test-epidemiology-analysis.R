compatibility_example <- function(typology_only = "00003") {
  list(
    compatibility_id = "compat-v1", typology_id = "typology-v1",
    typology_definition_version = "v1", surveillance_scope_id = "scope-v1",
    expected_typology_only_geo_ids = typology_only,
    expected_surveillance_only_geo_ids = character(), review_status = "reviewed",
    reason = "synthetic reviewed difference"
  )
}

attached_example <- function(values = c(0, 2, NA, 4), ids = sprintf("%05d", 1:4)) {
  data.frame(
    geo_id = ids, geo_name = paste("District", ids), geo_level = "district",
    geo_vintage = as.Date(NA), date = as.Date("2020-01-06"), time_unit = "week",
    pathogen = "Disease", incidence = values, source = "synthetic", query_id = "q1",
    period_set_id = "set-v1", period_id = "wave-v1",
    period_definition_version = "v1", period_review_status = "reviewed",
    typology_id = "typology-v1", typology_definition_version = "v1",
    cluster_id = c("A", "A", "B", "B"), stringsAsFactors = FALSE
  )
}

test_that("compatibility validates expected typology-only identifiers", {
  expect_invisible(validate_typology_surveillance_compatibility(compatibility_example()))
  bad <- compatibility_example("3")
  expect_error(validate_typology_surveillance_compatibility(bad), "five-character")
  bad <- compatibility_example(c("00003", "00003"))
  expect_error(validate_typology_surveillance_compatibility(bad), "unique")
})

test_that("typology attachment is exact by canonical ID and preserves provenance", {
  surveillance <- attached_example()[1:2, setdiff(names(attached_example()), c(
    "typology_id", "typology_definition_version", "cluster_id",
    "period_set_id", "period_id", "period_definition_version", "period_review_status"
  ))]
  typology <- list(
    data = data.frame(
      geo_id = c("00001", "00002", "00003"), cluster_id = c("A", "B", "C"),
      geo_name = c("wrong", "names", "ignored")
    ), provenance = list(fit_id = "fit-1")
  )
  original <- surveillance
  result <- attach_typology(surveillance, typology, compatibility_example())
  expect_identical(result$data$cluster_id, c("A", "B"))
  expect_identical(result$data$query_id, c("q1", "q1"))
  expect_identical(result$provenance$typology$fit_id, "fit-1")
  expect_identical(surveillance, original)
})

test_that("attachment rejects unexpected geography and duplicate typology IDs", {
  surveillance <- attached_example()[1:2, ]
  typology <- data.frame(geo_id = c("00001", "00002"), cluster_id = c("A", "B"))
  expect_error(attach_typology(surveillance, typology, compatibility_example()), "mismatch")
  typology <- rbind(typology, typology[1, ])
  expect_error(attach_typology(surveillance, typology, compatibility_example(character())), "unique")
  typology <- data.frame(geo_id = c("00001", "99999", "00003"), cluster_id = c("A", "B", "C"))
  expect_error(attach_typology(surveillance, typology, compatibility_example()), "mismatch")
})

test_that("reviewed expected geography counts are enforced when specified", {
  surveillance <- attached_example()[1:2, ]
  typology <- data.frame(
    geo_id = c("00001", "00002", "00003"), cluster_id = c("A", "B", "C")
  )
  compatibility <- compatibility_example()
  compatibility$expected_typology_count <- 4L
  compatibility$expected_surveillance_count <- 2L
  expect_error(attach_typology(surveillance, typology, compatibility), "typology geography count")
})

test_that("leading-zero AGS remain character identifiers", {
  surveillance <- attached_example()[1, ]
  typology <- data.frame(geo_id = c("00001", "00003"), cluster_id = c("A", "C"))
  result <- attach_typology(surveillance, typology, compatibility_example())
  expect_identical(result$data$geo_id, "00001")
})

test_that("weekly medians preserve zero and omit NA explicitly", {
  result <- summarize_incidence_by_typology(attached_example(), minimum_group_size = 2)
  a <- result$data[result$data$cluster_id == "A", ]
  b <- result$data[result$data$cluster_id == "B", ]
  expect_identical(a$median_incidence, 1)
  expect_identical(a$q1_incidence, 0.5)
  expect_identical(a$q3_incidence, 1.5)
  expect_identical(a$zero_count, 1L)
  expect_identical(a$observed_non_missing_count, 2L)
  expect_true(a$minimum_group_size_met)
  expect_identical(b$median_incidence, 4)
  expect_identical(b$expected_district_count, 2L)
  expect_identical(b$observed_non_missing_count, 1L)
  expect_identical(b$missing_count, 1L)
  expect_identical(b$completeness_proportion, 0.5)
  expect_false(b$minimum_group_size_met)
})

test_that("district period medians use one unweighted value per district", {
  x <- attached_example(values = c(0, 2, NA, 4))
  later <- x
  later$date <- as.Date("2020-01-13")
  later$incidence <- c(2, 6, 8, NA)
  result <- summarize_period_incidence_by_district(rbind(x, later))
  expect_identical(nrow(result$data), 4L)
  expect_identical(result$data$median_period_incidence[match("00001", result$data$geo_id)], 1)
  expect_identical(result$data$median_period_incidence[match("00002", result$data$geo_id)], 4)
  expect_identical(result$diagnostics$weighting, "none")
})

test_that("median handles even, odd, and all-NA groups without pooling", {
  x <- attached_example(values = c(1, 9, 2, 4))
  x <- rbind(x, transform(x[1, ], geo_id = "00005", incidence = 5))
  result <- summarize_incidence_by_typology(x)
  expect_identical(result$data$median_incidence[result$data$cluster_id == "A"], 5)
  expect_identical(result$data$median_incidence[result$data$cluster_id == "B"], 3)
  x$incidence[x$cluster_id == "B"] <- NA_real_
  result <- summarize_incidence_by_typology(x)
  b <- result$data[result$data$cluster_id == "B", ]
  expect_true(is.na(b$median_incidence))
  expect_false(b$minimum_group_size_met)
  expect_identical(result$diagnostics$estimand, "unweighted_median_of_district_incidence")
})

test_that("unsupported summary semantics fail", {
  expect_error(summarize_incidence_by_typology(attached_example(), "mean"), "only median")
  expect_error(summarize_incidence_by_typology(attached_example(), na_policy = "zero"), "NA omission")
})

test_that("duplicate district observations within a summary group fail", {
  x <- attached_example()
  expect_error(summarize_incidence_by_typology(rbind(x, x[1, ])), "only once")
})

test_that("dissertation compatibility records 401 to 400 without reassignment", {
  x <- dissertation_surveillance_compatibility()
  expect_identical(x$expected_typology_count, 401L)
  expect_identical(x$expected_surveillance_count, 400L)
  expect_identical(x$expected_typology_only_geo_ids, "16056")
  expect_identical(x$reason,
    "Eisenach absent from analytical surveillance; Wartburgkreis retained; both historical ClA; no reassignment")
})
