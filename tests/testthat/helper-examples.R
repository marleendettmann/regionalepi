surveillance_example <- function() {
  data.frame(
    geo_id = "01001", geo_name = "Example", geo_level = "district",
    geo_vintage = as.Date("2020-12-31"), date = as.Date("2020-01-01"),
    time_unit = "year", pathogen = "example", cases = 2L,
    source = "synthetic"
  )
}

surveillance_incidence_example <- function() {
  data.frame(
    geo_id = NA_character_, geo_name = "Example",
    geo_level = "survstat_kreis", geo_vintage = as.Date(NA),
    date = as.Date("2020-01-01"), time_unit = "week",
    pathogen = "example", incidence = 1.25, source = "synthetic",
    query_id = "query-1", stringsAsFactors = FALSE
  )
}

write_utf16_survstat_fixture <- function(lines, bom = TRUE) {
  path <- tempfile(fileext = ".csv")
  connection <- file(path, open = "wb")
  on.exit(close(connection))
  if (bom) writeBin(as.raw(c(0xff, 0xfe)), connection)
  bytes <- iconv(
    paste0(lines, collapse = "\r\n"),
    from = "UTF-8", to = "UTF-16LE", toRaw = TRUE
  )[[1L]]
  writeBin(bytes, connection)
  path
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
