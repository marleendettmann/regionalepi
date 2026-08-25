surveillance_example <- function() {
  data.frame(
    geo_id = "01001", geo_name = "Example", geo_level = "district",
    geo_vintage = as.Date("2020-12-31"), date = as.Date("2020-01-01"),
    time_unit = "year", pathogen = "example", cases = 2L,
    source = "synthetic"
  )
}

population_example <- function() {
  data.frame(
    geo_id = "01001", geo_name = "Example", geo_level = "district",
    geo_vintage = as.Date("2020-12-31"),
    reference_date = as.Date("2020-12-31"), age_from = 0L, age_to = 19L,
    population = 100.5, population_basis = "synthetic", source = "synthetic"
  )
}

context_example <- function() {
  data.frame(
    geo_id = "01001", geo_name = "Example", geo_level = "district",
    geo_vintage = as.Date("2020-12-31"),
    reference_date = as.Date("2020-12-31"), variable = "area",
    value = 10.5, unit = "km2", source = "synthetic"
  )
}

geography_example <- function() {
  data.frame(
    geo_id = "01001", geo_name = "Example", geo_level = "district",
    valid_from = as.Date("2020-01-01"), valid_to = as.Date(NA),
    source = "synthetic"
  )
}

relations_example <- function() {
  data.frame(
    from_geo_id = "01001", from_vintage = as.Date("2019-12-31"),
    to_geo_id = "01001", to_vintage = as.Date("2020-12-31"),
    relation_type = "identity", weight = NA_real_, source = "synthetic",
    note = NA_character_
  )
}

