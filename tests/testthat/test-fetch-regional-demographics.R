demographic_transport <- function(table, reference_dates, regions, credentials) {
  expect_named(credentials, c("username", "password"))
  if (table == "12411-07-01-4") {
    return(regional_long_json(table, c("12411", "KREISE", "BEV519", "GES"), c(
      "31.12.2020;DG; Deutschland;44,6;43,3;45,9",
      "31.12.2020;16; Thueringen;47,5;45,8;49,2",
      "31.12.2020;02000; Hamburg;42,1;40,9;43,3",
      "31.12.2020;16063; Wartburgkreis;48,1;46,6;49,6"
    )))
  }
  if (table == "12411-08-01-4") {
    return(regional_long_json(table,
      c("12411", "KREISE", "BEV216", "BEV215", "GES"), c(
        "31.12.2020;02000; Hamburg;29,6;30,5;28,7;28,8;24,8;32,7",
        "31.12.2020;16063; Wartburgkreis;29,7;29,8;29,5;46,8;40,2;54,1"
      )))
  }
  if (table == "11111-01-01-4") {
    return(regional_long_json(table, c("11111", "KREISE", "FLC006"), c(
      "31.12.2020;02000; Hamburg;755,09",
      "31.12.2020;16063; Wartburgkreis;1266,96"
    )))
  }
  if (table == "12411-09-01-4") {
    labels <- c(
      "unter 5 Jahre", paste(seq(5, 90, 5), "bis unter", seq(10, 95, 5), "Jahre"),
      "95 Jahre und mehr", "Insgesamt"
    )
    rows <- unlist(lapply(c("02000", "16063"), function(id) {
      name <- if (id == "02000") "Hamburg" else "Wartburgkreis"
      vapply(seq_along(labels), function(i) {
        paste("31.12.2020", id, name, labels[[i]],
              if (labels[[i]] == "Insgesamt") 2100 else 100,
              50, 50, sep = ";")
      }, character(1L))
    }))
    return(regional_long_json(table, c("12411", "KREISE", "ALTX05", "GES"), rows))
  }
  stop("unexpected table")
}

test_that("mean-age and youth adapters select authoritative total measures", {
  results <- with_regional_table_transport(demographic_transport, list(
    mean_age = fetch_regional_mean_age(
      as.Date("2020-12-31"), c("02000", "16063")
    ),
    youth = fetch_regional_youth_dependency(
      as.Date("2020-12-31"), c("02000", "16063")
    )
  ))
  expect_identical(results$mean_age$data$geo_id, c("02000", "16063"))
  expect_identical(results$mean_age$data$indicator_value, c(42.1, 48.1))
  expect_true(all(results$mean_age$data$indicator_unit == "years"))
  expect_true(all(results$mean_age$data$value_origin == "source_provided"))
  expect_identical(results$youth$data$indicator_value, c(29.6, 29.7))
  expect_true(all(results$youth$data$indicator_unit ==
                    "persons_under_20_per_100_persons_20_64"))
  expect_true(all(is.na(results$youth$data$geo_vintage)))
})

test_that("city-state keys are expanded and structural absences are omitted", {
  transport <- function(table, ...) {
    regional_long_json(table, c("12411", "KREISE", "BEV519", "GES"), c(
      "31.12.2020;02; Hamburg;42,1;40,9;43,3",
      "31.12.2020;11; Berlin;42,6;41,3;43,8",
      "31.12.2020;03152; Goettingen, Landkreis;-;-;-"
    ))
  }
  result <- with_regional_table_transport(
    transport, fetch_regional_mean_age(as.Date("2020-12-31"))
  )
  expect_identical(result$data$geo_id, c("02000", "11000"))
  expect_identical(result$diagnostics$source_quality_markers, "-")
  expect_error(
    with_regional_table_transport(
      transport,
      fetch_regional_mean_age(as.Date("2020-12-31"), "03152")
    ),
    "no observation"
  )
})

test_that("area adapter preserves positive qkm values and compact provenance", {
  result <- with_regional_table_transport(
    demographic_transport,
    fetch_regional_area(as.Date("2020-12-31"), c("02000", "16063"))
  )
  expect_identical(validate_regional_area(result$data), result$data)
  expect_identical(result$data$area_km2, c(755.09, 1266.96))
  expect_true(all(result$data$source_measure == "FLC006"))
  expect_true(all(result$data$provenance_id == "regional_area_11111-01-01-4"))
  expect_false("source_notes" %in% names(result$data))
})

test_that("internal age adapter maps exact five-year intervals", {
  result <- with_regional_table_transport(
    demographic_transport,
    regionalepi:::.fetch_regional_age_population(
      as.Date("2020-12-31"), c("02000", "16063")
    )
  )
  expect_identical(validate_context_population(result$data), result$data)
  first <- result$data[result$data$geo_id == "02000", ]
  expect_identical(first$age_from[1:13], seq(0L, 60L, 5L))
  expect_identical(first$age_to[1:13], seq(4L, 64L, 5L))
  expect_true(is.na(tail(first$age_to, 1L)))
  expect_true(all(first$sex == "total"))
})

test_that("demographic adapters reject quality markers and unsafe envelopes", {
  bad <- function(table, ...) {
    regional_long_json(table, c("12411", "KREISE", "BEV519", "GES"),
                       "31.12.2020;16063; Wartburgkreis;.")
  }
  expect_error(
    with_regional_table_transport(
      bad, fetch_regional_mean_age(as.Date("2020-12-31"), "16063")
    ),
    "quality-marked"
  )
  wrong_codes <- function(table, ...) {
    regional_long_json(table, c("12411", "KREISE", "GES"),
                       "31.12.2020;16063; Wartburgkreis;48,1")
  }
  expect_error(
    with_regional_table_transport(
      wrong_codes, fetch_regional_mean_age(as.Date("2020-12-31"), "16063")
    ),
    "BEV519"
  )
  unexpected_key <- function(table, ...) {
    regional_long_json(table, c("12411", "KREISE", "BEV519", "GES"), c(
      "31.12.2020;ABC; Unexpected;1,0",
      "31.12.2020;16063; Wartburgkreis;48,1"
    ))
  }
  expect_error(
    with_regional_table_transport(
      unexpected_key, fetch_regional_mean_age(as.Date("2020-12-31"), "16063")
    ),
    "unexpected regional key"
  )
})

test_that("shared transport errors cannot expose credentials", {
  transport <- function(...) stop("synthetic-user synthetic-password")
  message <- tryCatch(
    with_regional_table_transport(
      transport, fetch_regional_area(as.Date("2020-12-31"), "16063")
    ),
    error = conditionMessage
  )
  expect_false(grepl("synthetic-user|synthetic-password", message))
})
