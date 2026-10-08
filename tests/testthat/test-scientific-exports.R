export_fixture <- function() {
  map <- regionalepi_map_geometry()
  from_demography <- regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)
  to_demography <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)
  from_fit <- fit_dynamic_typology(from_demography$summary, k = 3L)
  to_fit <- fit_dynamic_typology(to_demography$summary, k = 3L)
  prepared <- list(
    demographic = to_demography, fit = to_fit,
    map_join = regionalepi:::.shiny_map_assignments(map, to_fit),
    demographic_years = 2022:2025,
    configuration = list(k = 3L)
  )
  transition <- compare_dynamic_typology_transitions(
    from_fit, to_fit, map$features$geo_id)
  list(map = map, prepared = prepared, transition = transition)
}

test_that("package and dissertation citations are distinct and consistent", {
  package_citation <- paste0(
    "Dettmann, M. (2026). regionalepi: Regionale Infektionssurveillance und ",
    "demografische Typologien (Version 0.1.0). R-Paket."
  )
  dissertation_citation <- paste0(
    "Dettmann, M. (2026). Einfluss demografischer Faktoren auf die ",
    "Ausbreitung von Infektionskrankheiten am Beispiel von Influenza und ",
    "COVID-19. Freie Universität Berlin. DOI: 10.17169/refubium-51449."
  )
  expect_identical(regionalepi:::.regionalepi_package_citation(),
    package_citation)
  expect_identical(regionalepi:::.regionalepi_dissertation_citation(),
    dissertation_citation)
  sources <- regionalepi:::.export_sources(TRUE)
  expect_identical(sources$attribution[sources$source == "regionalepi R-Paket"],
    package_citation)
  expect_identical(sources$attribution[
    sources$source == "Wissenschaftliche Grundlage"], dissertation_citation)
  expect_false(grepl("refubium|51449", sources$attribution[
    sources$source == "regionalepi R-Paket"], ignore.case = TRUE))
  expect_identical(sum(grepl("10.17169/refubium-51449",
    sources$attribution, fixed = TRUE)), 1L)

  cff_path <- file.path(testthat::test_path("..", ".."), "CITATION.cff")
  if (file.exists(cff_path)) {
    cff <- readLines(cff_path, warn = FALSE)
    expect_true(any(cff == paste0('title: "regionalepi: Regionale ',
      'Infektionssurveillance und demografische Typologien"')))
    expect_true(any(cff == 'version: "0.1.0"'))
    expect_false(any(grepl("refubium-51449", cff, fixed = TRUE)))
  }
  citation_file <- utils::readCitationFile(
    system.file("CITATION", package = "regionalepi"),
    meta = utils::packageDescription("regionalepi"))
  expect_identical(attr(unclass(citation_file)[[1L]], "textVersion"),
    package_citation)
})

test_that("typology export tables equal the prepared displayed state", {
  fixture <- export_fixture()
  tables <- regionalepi:::.typology_export_tables(
    fixture$prepared, fixture$map$provenance,
    as.POSIXct("2026-10-08 12:00:00", tz = "UTC"))
  expect_identical(names(tables), c("Kreiszuordnungen", "Clusterprofile",
    "Metadaten", "Methodik", "Quellen und Lizenz"))
  assignments <- tables$Kreiszuordnungen
  expect_identical(nrow(assignments), 400L)
  expect_true(all(grepl("^[0-9]{5}$", assignments$geo_id)))
  expected <- fixture$prepared$map_join$data$display_cluster_id[
    match(assignments$geo_id, fixture$prepared$map_join$data$geo_id)]
  expect_identical(assignments$dynamic_cluster_id, expected)
  expect_identical(unique(assignments$fit_id),
    fixture$prepared$fit$provenance$fit_id)
  expect_true(all(c("snapshot_id", "fit_id", "source_data_status") %in%
    tables$Metadaten$field))
  expect_identical(tables$`Quellen und Lizenz`$attribution[1:2], c(
    regionalepi:::.regionalepi_package_citation(),
    regionalepi:::.regionalepi_dissertation_citation()))
})

test_that("transition export is the exact cached comparison", {
  fixture <- export_fixture()
  tables <- regionalepi:::.transition_export_tables(fixture$transition,
    fixture$map,
    fixture$prepared$demographic$snapshot_provenance$snapshot_id,
    as.POSIXct("2026-10-08 12:00:00", tz = "UTC"))
  expect_identical(names(tables), c("Clusterwechsler", "Übergangsmatrix",
    "Übergangsprozente", "Indikatorveränderungen", "Metadaten", "Methodik",
    "Quellen und Lizenz"))
  expect_identical(nrow(tables$Clusterwechsler), 400L)
  expect_identical(sum(tables$Clusterwechsler$changed), 31L)
  expect_identical(as.integer(as.matrix(tables$Übergangsmatrix[, -1L])),
    as.integer(fixture$transition$count_matrix))
  expect_identical(unique(tables$Clusterwechsler$comparison_id),
    fixture$transition$provenance$comparison_id)
  expect_identical(nrow(tables$Indikatorveränderungen), 400L)
  expect_identical(tables$`Quellen und Lizenz`$attribution[1:2], c(
    regionalepi:::.regionalepi_package_citation(),
    regionalepi:::.regionalepi_dissertation_citation()))
})

test_that("Excel and CSV writers preserve identifiers, NA, zero and safety", {
  unsafe <- data.frame(geo_id = c("01001", "11000", "16063"),
    label = c("=1+1", "+SUM(A1:A2)", "ordinary"),
    value = c(0, NA_real_, 2.5), stringsAsFactors = FALSE)
  safe <- regionalepi:::.export_table(unsafe)
  expect_identical(safe$geo_id, unsafe$geo_id)
  expect_identical(safe$label[1:2], c("'=1+1", "'+SUM(A1:A2)"))
  expect_identical(safe$value, unsafe$value)
  csv <- tempfile(fileext = ".csv")
  xlsx <- tempfile(fileext = ".xlsx")
  regionalepi:::.write_scientific_csv(unsafe, csv)
  regionalepi:::.write_scientific_workbook(list(Results = unsafe,
    Metadata = data.frame(field = "source", value = "reviewed")), xlsx)
  read_back <- utils::read.csv(csv, colClasses = c(geo_id = "character"),
    na.strings = "NA", check.names = FALSE)
  expect_identical(read_back$geo_id, unsafe$geo_id)
  expect_identical(read_back$label, safe$label)
  expect_identical(read_back$value, unsafe$value)
  expect_gt(file.info(xlsx)$size, 0)
  archive <- utils::unzip(xlsx, list = TRUE)$Name
  expect_true(all(c("xl/workbook.xml", "xl/worksheets/sheet1.xml") %in% archive))
  extracted <- tempfile();dir.create(extracted)
  utils::unzip(xlsx, exdir = extracted)
  xml <- paste(vapply(list.files(file.path(extracted, "xl"), recursive = TRUE,
    full.names = TRUE, pattern = "\\.xml$"), function(path)
      paste(readLines(path, warn = FALSE), collapse = ""), character(1L)),
    collapse = "")
  expect_match(xml, "01001", fixed = TRUE)
  expect_false(grepl("<f>", xml, fixed = TRUE))
})

test_that("epidemiology exports preserve loaded summaries and incidence semantics", {
  weekly <- data.frame(date = as.Date(c("2025-01-06", "2025-01-13")),
    cluster_id = c("C01", "C01"), median_incidence = c(0, NA_real_),
    q1_incidence = c(0, NA_real_), q3_incidence = c(1, NA_real_),
    observed_districts = c(10L, 0L), expected_districts = c(10L, 10L),
    missing_districts = c(0L, 10L), stringsAsFactors = FALSE)
  districts <- data.frame(geo_id = c("01001", "11000"),
    geo_name = c("Flensburg", "Berlin"), cluster_id = c("C01", "C02"),
    median_period_incidence = c(0, NA_real_), cumulative_observed_cases = c(0, NA_real_),
    observed_week_count = c(2L, 0L), expected_week_count = 2L,
    missing_week_count = c(0L, 2L), observed_case_week_count = c(2L, 0L),
    missing_case_week_count = c(0L, 2L), stringsAsFactors = FALSE)
  result <- list(window = list(pathogen = "Influenza, saisonal",
      label = "2025/26"), analysis_range = list(start_date = as.Date("2025-01-06"),
      end_date = as.Date("2025-01-13")),
    loaded_selection = list(demographic_period = "2022-2025", k = 3L),
    fit = list(provenance = list(fit_id = "fit-reviewed")),
    demographic = list(snapshot_provenance = list(snapshot_id = "snapshot-v4")),
    bundle = list(diagnostics = list(status_by_reporting_year = data.frame(
      reporting_year = 2025L, population_year = 2025L,
      incidence_status = "final")), provenance = list(
      incidence = list(q = list(data_status = "reviewed-status")),
      counts = list(q = list(data_status = "reviewed-status")))))
  tables <- regionalepi:::.epidemiology_export_tables(result,
    list(weekly = weekly, district_period = districts))
  expect_identical(tables[["Wöchentliche Clusterinzidenz"]]$median_incidence,
    weekly$median_incidence)
  expect_identical(tables[["Kreisbezogene Ergebnisse"]]$geo_id,
    districts$geo_id)
  expect_identical(tables[["Kreisbezogene Ergebnisse"]]$median_period_incidence,
    districts$median_period_incidence)
  expect_true(all(c("fit_id", "snapshot_id", "incidence_definition",
    "survstat_data_status") %in% tables$Metadaten$field))
  expect_false(any(grepl("PASSWORD|Authorization|SOAP", unlist(tables),
    ignore.case = TRUE)))
  expect_identical(tables$`Quellen und Lizenz`$attribution[1:2], c(
    regionalepi:::.regionalepi_package_citation(),
    regionalepi:::.regionalepi_dissertation_citation()))
})

test_that("scientific PNG plots use supplied result data", {
  skip_if_not_installed("ggplot2")
  fixture <- export_fixture()
  profile_plot <- regionalepi:::.scientific_export_plot("profiles",
    fixture$prepared$fit$profiles)
  transition_plot <- regionalepi:::.scientific_export_plot("transition",
    fixture$transition$count_matrix)
  expect_s3_class(profile_plot, "ggplot")
  expect_s3_class(transition_plot, "ggplot")
  expect_identical(sum(transition_plot$data$n), 400L)
  expect_identical(levels(transition_plot$data$to),
    c("\u00c4lter/l\u00e4ndlich","Familie/Jugend","Dicht"))
  expect_identical(rev(levels(transition_plot$data$from)),
    c("\u00c4lter/l\u00e4ndlich","Familie/Jugend","Dicht"))
  expect_identical(transition_plot$labels$x,"2022\u20132025")
  expect_identical(transition_plot$labels$y,"2017\u20132020")
  path <- tempfile(fileext = ".png")
  regionalepi:::.write_scientific_png(transition_plot, path, 6, 4, 120)
  expect_gt(file.info(path)$size, 1000)
})
