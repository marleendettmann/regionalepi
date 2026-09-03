.observation_window_columns <- c(
  "observation_window_id", "pathogen", "label", "start_date", "end_date",
  "start_iso_year", "start_iso_week", "end_iso_year", "end_iso_week",
  "window_type", "definition_version", "note"
)

.iso_week_monday <- function(year, week) {
  jan4 <- as.Date(sprintf("%04d-01-04", as.integer(year)))
  monday <- jan4 - (as.integer(format(jan4, "%u")) - 1L)
  monday + 7L * (as.integer(week) - 1L)
}

.iso_week_label <- function(date) {
  sprintf("%s-KW%02d", format(date, "%G"), as.integer(format(date, "%V")))
}

#' Validate epidemiological observation windows
#'
#' Observation windows are display and analysis frames. They are deliberately
#' separate from reviewed epidemiological periods and do not assert that a
#' wave exists.
#' @param x An observation-window data frame.
#' @return `x`, invisibly.
#' @export
validate_observation_windows <- function(x) {
  contract <- "observation windows"
  .require_data_frame(x, contract)
  .require_columns(x, .observation_window_columns, contract)
  if (!identical(names(x), .observation_window_columns)) {
    .stop_contract(contract, "fields are missing, unexpected, or reordered")
  }
  chars <- c("observation_window_id", "pathogen", "label", "window_type",
             "definition_version", "note")
  for (field in chars) .check_character(x[[field]], field, contract,
                                        allow_na = field == "note")
  for (field in c("start_date", "end_date")) .check_date(x[[field]], field, contract)
  for (field in c("start_iso_year", "start_iso_week", "end_iso_year", "end_iso_week")) {
    .check_whole_number(x[[field]], field, contract, non_negative = TRUE)
  }
  if (!nrow(x) || anyNA(x[setdiff(names(x), "note")]) ||
      any(!nzchar(as.matrix(x[c("observation_window_id", "pathogen", "label",
                                "window_type", "definition_version")]))) ||
      anyDuplicated(x$observation_window_id)) {
    .stop_contract(contract, "required fields must be complete and IDs unique")
  }
  if (any(!x$window_type %in% c("influenza_season_window", "covid_observation_window")) ||
      any(x$start_date > x$end_date) || any(x$start_iso_week > 53L) ||
      any(x$end_iso_week > 53L)) {
    .stop_contract(contract, "window type, interval, or ISO week is invalid")
  }
  expected_start <- .iso_week_monday(x$start_iso_year, x$start_iso_week)
  expected_end <- .iso_week_monday(x$end_iso_year, x$end_iso_week) + 6L
  if (!identical(x$start_date, expected_start) || !identical(x$end_date, expected_end)) {
    .stop_contract(contract, "dates must exactly match the inclusive ISO-week boundaries")
  }
  if (any((x$window_type == "influenza_season_window") !=
          (x$pathogen == "Influenza, saisonal")) ||
      any((x$window_type == "covid_observation_window") !=
          (x$pathogen == "COVID-19"))) {
    .stop_contract(contract, "pathogen and window type are incompatible")
  }
  invisible(x)
}

#' Reviewed regionalepi observation windows
#'
#' Influenza windows run inclusively from ISO week 40 through week 20 of the
#' following year. Ordinary COVID-19 windows run from week 20 through week 20.
#' A neutral historical pandemic frame spans the complete reviewed dissertation
#' period system from 2020 week 10 through 2022 week 21. These are regionalepi
#' observation frames, not official RKI seasons or waves.
#' @return A validated observation-window data frame.
#' @export
regionalepi_observation_windows <- function() {
  make <- function(pathogen, years, start_week, type, prefix) {
    starts <- years[-length(years)]
    ends <- years[-1L]
    data.frame(
      observation_window_id = paste0(prefix, "_", starts, "_", substr(ends, 3L, 4L)),
      pathogen = pathogen,
      label = paste0(starts, "/", substr(ends, 3L, 4L)),
      start_date = .iso_week_monday(starts, start_week),
      end_date = .iso_week_monday(ends, 20L) + 6L,
      start_iso_year = as.integer(starts), start_iso_week = as.integer(start_week),
      end_iso_year = as.integer(ends), end_iso_week = 20L,
      window_type = type, definition_version = "observation_windows_v1",
      note = if (type == "covid_observation_window")
        "regionalepi analytical observation window; not an official RKI COVID-19 season or wave" else
        "ordinary seasonal Influenza observation window; reviewed RKI waves remain separate",
      stringsAsFactors = FALSE
    )
  }
  out <- rbind(
    make("Influenza, saisonal", 2017:2026, 40L, "influenza_season_window", "influenza"),
    data.frame(
      observation_window_id = "covid19_pandemic_2020_22",
      pathogen = "COVID-19", label = "Pandemie-Beobachtungszeitraum 2020\u20132022",
      start_date = .iso_week_monday(2020L, 10L),
      end_date = .iso_week_monday(2022L, 21L) + 6L,
      start_iso_year = 2020L, start_iso_week = 10L,
      end_iso_year = 2022L, end_iso_week = 21L,
      window_type = "covid_observation_window",
      definition_version = "observation_windows_v1",
      note = paste("neutral historical data/display frame covering the reviewed",
        "dissertation pandemic periods; not an official RKI COVID-19 season or wave"),
      stringsAsFactors = FALSE
    ),
    make("COVID-19", 2020:2026, 20L, "covid_observation_window", "covid19")
  )
  validate_observation_windows(out)
  out
}
