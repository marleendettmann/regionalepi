regional_long_json <- function(table, codes, rows, status = 0L) {
  content <- paste(c(
    paste("Tabelle:", table), "Synthetic table", rows, "__________",
    "Synthetic methodological note.", "© Synthetic source",
    "Stand: 01.01.2025 / 12:00:00"
  ), collapse = "\n")
  jsonlite::toJSON(list(
    Ident = list(Service = "data", Method = "table"),
    Status = list(Code = status, Content = if (status == 0L) "erfolgreich" else "Fehler"),
    Parameter = list(name = table),
    Object = list(Content = content,
                  Structure = lapply(codes, function(code) list(Code = code))),
    Copyright = "Synthetic attribution"
  ), auto_unbox = TRUE, null = "null")
}

with_regional_table_transport <- function(transport, code) {
  namespace <- asNamespace("regionalepi")
  old <- get(".regional_table_transport", envir = namespace)
  unlockBinding(".regional_table_transport", namespace)
  assign(".regional_table_transport", transport, envir = namespace)
  lockBinding(".regional_table_transport", namespace)
  on.exit({
    unlockBinding(".regional_table_transport", namespace)
    assign(".regional_table_transport", old, envir = namespace)
    lockBinding(".regional_table_transport", namespace)
  }, add = TRUE)
  old_user <- Sys.getenv("REGIONALSTATISTIK_USER", unset = NA_character_)
  old_password <- Sys.getenv("REGIONALSTATISTIK_PASSWORD", unset = NA_character_)
  on.exit({
    if (is.na(old_user)) Sys.unsetenv("REGIONALSTATISTIK_USER") else
      Sys.setenv(REGIONALSTATISTIK_USER = old_user)
    if (is.na(old_password)) Sys.unsetenv("REGIONALSTATISTIK_PASSWORD") else
      Sys.setenv(REGIONALSTATISTIK_PASSWORD = old_password)
  }, add = TRUE)
  Sys.setenv(REGIONALSTATISTIK_USER = "synthetic-user",
             REGIONALSTATISTIK_PASSWORD = "synthetic-password")
  force(code)
}

demographic_example <- function(ids = c("01001", "16063"),
                                years = 2017:2020,
                                indicator_id = "mean_age",
                                unit = "years",
                                origin = "source_provided") {
  n <- length(ids) * length(years)
  data.frame(
    geo_id = rep(ids, each = length(years)),
    geo_name = rep(paste0("Name ", ids), each = length(years)),
    geo_level = rep("district", n), geo_vintage = rep(as.Date(NA), n),
    reference_date = rep(as.Date(sprintf("%d-12-31", years)), length(ids)),
    indicator_id = rep(indicator_id, n),
    indicator_value = seq_len(n), indicator_unit = rep(unit, n),
    definition_version = rep("dissertation_v1", n),
    value_origin = rep(origin, n), source = rep("synthetic", n),
    population_basis = rep("census_2011", n),
    provenance_id = rep(paste0("p-", indicator_id), n),
    stringsAsFactors = FALSE
  )
}

regional_area_example <- function() {
  data.frame(
    geo_id = c("01001", "02000"), geo_name = c("Flensburg", "Hamburg"),
    geo_level = "district", geo_vintage = as.Date(NA),
    reference_date = as.Date("2020-12-31"), area_km2 = c(56.73, 755.09),
    source = "Regionaldatenbank Deutschland", source_table = "11111-01-01-4",
    source_measure = "FLC006",
    retrieved_at = as.POSIXct("2025-01-01", tz = "UTC"),
    data_status = "synthetic", provenance_id = "area-source",
    stringsAsFactors = FALSE
  )
}
