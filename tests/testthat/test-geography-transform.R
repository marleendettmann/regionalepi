relation_rows <- function(from, to, type, from_vintage, to_vintage,
                          weight = NA_real_) {
  data.frame(
    from_geo_id = from,
    from_vintage = rep(as.Date(from_vintage), length(from)),
    to_geo_id = to,
    to_vintage = rep(as.Date(to_vintage), length(from)),
    relation_type = type,
    weight = rep(weight, length.out = length(from)),
    source = rep("synthetic", length(from)),
    note = rep(NA_character_, length(from)),
    stringsAsFactors = FALSE
  )
}

geography_rows <- function(id, name, level = "target",
                           valid_from = "2000-01-01", valid_to = NA) {
  data.frame(
    geo_id = id,
    geo_name = name,
    geo_level = rep(level, length.out = length(id)),
    valid_from = rep(as.Date(valid_from), length.out = length(id)),
    valid_to = rep(as.Date(valid_to), length.out = length(id)),
    source = rep("synthetic", length(id)),
    stringsAsFactors = FALSE
  )
}

berlin_input <- function() {
  data.frame(
    geo_id = rep(c("B01", "B02", "B03"), each = 2),
    geo_name = rep(c("Berlin-Mitte", "Berlin-Pankow", "Berlin-Neukoelln"), each = 2),
    geo_level = "surveillance_unit",
    geo_vintage = as.Date("2020-12-31"),
    week = rep(c("W01", "W02"), 3),
    pathogen = "influenza",
    age_group = "all",
    sex = "all",
    source_version = "synthetic_v1",
    cases = c(10, 15, 20, 25, 30, 35),
    admissions = c(1, 2, 3, 4, 5, 6),
    stringsAsFactors = FALSE
  )
}

berlin_relations <- function() {
  relation_rows(
    c("B01", "B02", "B03"), rep("11000", 3), rep("aggregate", 3),
    "2020-12-31", "2020-12-31"
  )
}

berlin_target <- function() {
  geography_rows("11000", "Berlin", "municipality", "2001-01-01", NA)
}

test_that("Berlin-style aggregation preserves weeks and dimensions", {
  input <- berlin_input()
  original <- input
  result <- aggregate_geography(
    input, berlin_relations(), berlin_target(),
    value_cols = c("cases", "admissions"),
    group_cols = c("week", "pathogen", "age_group", "sex", "source_version")
  )

  expect_identical(input, original)
  expect_identical(result$data$geo_id, c("11000", "11000"))
  expect_identical(result$data$geo_name, c("Berlin", "Berlin"))
  expect_identical(result$data$geo_level, c("municipality", "municipality"))
  expect_identical(result$data$geo_vintage, rep(as.Date("2020-12-31"), 2))
  expect_identical(result$data$week, c("W01", "W02"))
  expect_identical(result$data$pathogen, rep("influenza", 2))
  expect_identical(result$data$age_group, rep("all", 2))
  expect_identical(result$data$sex, rep("all", 2))
  expect_identical(result$data$source_version, rep("synthetic_v1", 2))
  expect_identical(result$data$cases, c(60, 75))
  expect_identical(result$data$admissions, c(9, 12))
  expect_true(all(result$diagnostics$mass_balance$balanced))
  expect_identical(result$diagnostics$mass_balance$groups_checked, c(2L, 2L))
})

test_that("aggregation diagnostics record simple transformation provenance", {
  result <- aggregate_geography(
    berlin_input(), berlin_relations(), berlin_target(),
    value_cols = c("cases", "admissions"),
    group_cols = c("week", "pathogen", "age_group", "sex", "source_version")
  )
  diagnostics <- result$diagnostics
  expect_identical(diagnostics$operation, "aggregate_geography")
  expect_identical(diagnostics$input_rows, 6L)
  expect_identical(diagnostics$input_units, 3L)
  expect_identical(diagnostics$mapped_units, 3L)
  expect_identical(diagnostics$output_rows, 2L)
  expect_identical(diagnostics$output_units, 1L)
  expect_identical(diagnostics$relation_rows_used, 3L)
  expect_identical(diagnostics$relation_types_used, "aggregate")
  expect_identical(diagnostics$unmatched_unit_count, 0L)
  expect_identical(diagnostics$ambiguous_unit_count, 0L)
  expect_identical(diagnostics$value_semantics, "additive")
})

test_that("target metadata comes from the uniquely valid historical register row", {
  target <- rbind(
    geography_rows("11000", "Old Berlin Name", "old_level", "1900-01-01", "1999-12-31"),
    geography_rows("11000", "Canonical Berlin", "canonical_level", "2000-01-01", NA)
  )
  result <- aggregate_geography(
    berlin_input(), berlin_relations(), target, c("cases", "admissions"),
    c("week", "pathogen", "age_group", "sex", "source_version")
  )
  expect_identical(unique(result$data$geo_name), "Canonical Berlin")
  expect_identical(unique(result$data$geo_level), "canonical_level")

  target$valid_to[1] <- as.Date(NA)
  expect_error(
    aggregate_geography(
      berlin_input(), berlin_relations(), target, c("cases", "admissions"),
      c("week", "pathogen", "age_group", "sex", "source_version")
    ),
    "multiple target geography rows"
  )
})

test_that("historical merge preserves additive weekly counts", {
  input <- data.frame(
    geo_id = c("16056", "16063"),
    geo_name = c("Eisenach", "Wartburgkreis"),
    geo_level = "district",
    geo_vintage = as.Date("2020-12-31"),
    week = "W01",
    pathogen = "influenza",
    cases = c(10, 30),
    stringsAsFactors = FALSE
  )
  relations <- rbind(
    relation_rows("16056", "16063", "historical_merge", "2020-12-31", "2022-12-31"),
    relation_rows("16063", "16063", "identity", "2020-12-31", "2022-12-31")
  )
  target <- geography_rows("16063", "Wartburgkreis", "district", "2021-07-01", NA)
  result <- harmonize_vintage(
    input, relations, target,
    source_vintage = as.Date("2020-12-31"),
    target_vintage = as.Date("2022-12-31"),
    value_cols = "cases", group_cols = c("week", "pathogen")
  )
  expect_identical(result$data$geo_id, "16063")
  expect_identical(result$data$geo_name, "Wartburgkreis")
  expect_identical(result$data$geo_level, "district")
  expect_identical(result$data$geo_vintage, as.Date("2022-12-31"))
  expect_identical(result$data$week, "W01")
  expect_identical(result$data$pathogen, "influenza")
  expect_identical(result$data$cases, 40)
  expect_identical(
    result$diagnostics$relation_types_used,
    c("historical_merge", "identity")
  )
  expect_true(result$diagnostics$mass_balance$balanced)
})

test_that("identity relations preserve rows and use target metadata", {
  input <- berlin_input()[1:2, ]
  relations <- relation_rows("B01", "B01", "identity", "2020-12-31", "2020-12-31")
  target <- geography_rows("B01", "Canonical B01", "canonical_unit")
  result <- aggregate_geography(
    input, relations, target, c("cases", "admissions"),
    c("week", "pathogen", "age_group", "sex", "source_version")
  )
  expect_identical(result$data$cases, c(10, 15))
  expect_identical(result$data$geo_name, rep("Canonical B01", 2))
})

test_that("unmatched and ambiguous source units fail explicitly", {
  relations <- berlin_relations()[1:2, ]
  expect_error(
    aggregate_geography(
      berlin_input(), relations, berlin_target(), "cases",
      c("week", "pathogen", "age_group", "sex", "source_version", "admissions")
    ),
    "unmatched.*B03"
  )

  relations <- rbind(
    berlin_relations(),
    relation_rows("B01", "99999", "aggregate", "2020-12-31", "2020-12-31")
  )
  target <- rbind(berlin_target(), geography_rows("99999", "Other target"))
  expect_error(
    aggregate_geography(
      berlin_input(), relations, target, "cases",
      c("week", "pathogen", "age_group", "sex", "source_version", "admissions")
    ),
    "ambiguous.*B01"
  )
})

test_that("unsupported and wrong-operation relation types fail explicitly", {
  for (type in c("historical_split", "boundary_change", "historical_merge")) {
    relations <- berlin_relations()
    relations$relation_type[1] <- type
    expect_error(
      aggregate_geography(
        berlin_input(), relations, berlin_target(), "cases",
        c("week", "pathogen", "age_group", "sex", "source_version", "admissions")
      ),
      "unsupported relation type"
    )
  }

  input <- berlin_input()[1, ]
  relations <- relation_rows("B01", "B01", "aggregate", "2020-12-31", "2021-12-31")
  expect_error(
    harmonize_vintage(
      input, relations, geography_rows("B01", "B01"),
      as.Date("2020-12-31"), as.Date("2021-12-31"), "cases",
      c("week", "pathogen", "age_group", "sex", "source_version", "admissions")
    ),
    "unsupported relation type"
  )
})

test_that("vintage consistency is enforced by each operation", {
  input <- berlin_input()
  input$geo_vintage[1] <- as.Date("2019-12-31")
  expect_error(
    aggregate_geography(
      input, berlin_relations(), berlin_target(), "cases",
      c("week", "pathogen", "age_group", "sex", "source_version", "admissions")
    ),
    "one common.*geo_vintage"
  )

  input <- berlin_input()[1, ]
  expect_error(
    harmonize_vintage(
      input,
      relation_rows("B01", "B01", "identity", "2019-12-31", "2021-12-31"),
      geography_rows("B01", "B01"), as.Date("2019-12-31"),
      as.Date("2021-12-31"), "cases",
      c("week", "pathogen", "age_group", "sex", "source_version", "admissions")
    ),
    "must equal.*source_vintage"
  )

  expect_error(
    harmonize_vintage(
      input,
      relation_rows("B01", "B01", "identity", "2020-12-31", "2022-12-31"),
      geography_rows("B01", "B01"), as.Date("2020-12-31"),
      as.Date("2021-12-31"), "cases",
      c("week", "pathogen", "age_group", "sex", "source_version", "admissions")
    ),
    "unmatched.*source/target vintages"
  )
})

test_that("value, grouping, and semantic arguments are strict", {
  input <- berlin_input()
  groups <- c("week", "pathogen", "age_group", "sex", "source_version", "admissions")
  input$cases <- as.character(input$cases)
  expect_error(
    aggregate_geography(input, berlin_relations(), berlin_target(), "cases", groups),
    "cases.*numeric"
  )
  input <- berlin_input()
  input$cases[1] <- NA_real_
  expect_error(
    aggregate_geography(input, berlin_relations(), berlin_target(), "cases", groups),
    "cases.*must not contain NA"
  )
  input$cases[1] <- Inf
  expect_error(
    aggregate_geography(input, berlin_relations(), berlin_target(), "cases", groups),
    "cases.*finite"
  )
  input <- berlin_input()
  expect_error(
    aggregate_geography(input, berlin_relations(), berlin_target(), "missing", groups),
    "missing requested"
  )
  expect_error(
    aggregate_geography(input, berlin_relations(), berlin_target(), "cases", c(groups, "missing")),
    "missing requested"
  )
  expect_error(
    aggregate_geography(input, berlin_relations(), berlin_target(), "cases", groups[-length(groups)]),
    "unclassified.*admissions"
  )
  expect_error(
    aggregate_geography(input, berlin_relations(), berlin_target(), "cases", c(groups, "cases")),
    "both values and groups"
  )
  expect_error(
    aggregate_geography(
      input, berlin_relations(), berlin_target(), "cases", groups,
      value_semantics = "rate"
    ),
    "exactly.*additive"
  )
})

test_that("fractional relation allocation is not implemented", {
  relations <- berlin_relations()
  relations$weight[1] <- 0.5
  expect_error(
    aggregate_geography(
      berlin_input(), relations, berlin_target(), "cases",
      c("week", "pathogen", "age_group", "sex", "source_version", "admissions")
    ),
    "fractional relation weights.*unsupported"
  )
})

test_that("zero-row inputs return structured successful diagnostics", {
  input <- berlin_input()[0, ]
  result <- aggregate_geography(
    input, berlin_relations(), berlin_target(), c("cases", "admissions"),
    c("week", "pathogen", "age_group", "sex", "source_version")
  )
  expect_identical(result$data, input)
  expect_identical(result$diagnostics$input_rows, 0L)
  expect_true(all(result$diagnostics$mass_balance$balanced))

  result <- harmonize_vintage(
    input, berlin_relations(), berlin_target(), as.Date("2020-12-31"),
    as.Date("2021-12-31"), c("cases", "admissions"),
    c("week", "pathogen", "age_group", "sex", "source_version")
  )
  expect_identical(nrow(result$data), 0L)
  expect_s3_class(result$data$geo_vintage, "Date")
  expect_identical(result$diagnostics$target_vintage, as.Date("2021-12-31"))
})
