.period_columns <- c(
  "period_set_id", "period_id", "pathogen", "season_id", "period_type",
  "label", "start_date", "end_date", "definition_version",
  "variant_context", "historical_context", "evidence_class",
  "source_reference", "review_status", "note"
)

.empty_epidemiological_periods <- function() {
  template <- dissertation_influenza_periods()$periods
  template[0, .period_columns, drop = FALSE]
}

#' Validate epidemiological periods
#'
#' Periods are reviewed external epidemiological metadata. Inclusive `Date`
#' boundaries are authoritative; they are never inferred from surveillance
#' incidence. A season may have zero, one, or multiple period rows.
#'
#' @param x A period data frame.
#' @return `x`, invisibly.
#' @export
validate_epidemiological_periods <- function(x) {
  contract <- "epidemiological periods"
  .require_data_frame(x, contract)
  .require_columns(x, .period_columns, contract)
  for (field in c(
    "period_set_id", "period_id", "pathogen", "period_type", "label",
    "definition_version", "source_reference", "review_status"
  )) .check_character(x[[field]], field, contract)
  .check_character(x$season_id, "season_id", contract, allow_na = TRUE)
  .check_character(x$variant_context, "variant_context", contract, allow_na = TRUE)
  .check_character(x$historical_context, "historical_context", contract, allow_na = TRUE)
  .check_character(x$evidence_class, "evidence_class", contract)
  .check_character(x$note, "note", contract, allow_na = TRUE)
  .check_date(x$start_date, "start_date", contract)
  .check_date(x$end_date, "end_date", contract)
  if (any(x$start_date > x$end_date)) {
    .stop_contract(contract, "`start_date` must not be after `end_date`")
  }
  if (any(!x$review_status %in% c("reviewed", "provisional"))) {
    .stop_contract(contract, "`review_status` must be reviewed or provisional")
  }
  keys <- paste(x$period_set_id, x$period_id, sep = "\r")
  if (anyDuplicated(keys)) {
    .stop_contract(contract, "period IDs must be unique within a period set")
  }
  groups <- split(seq_len(nrow(x)), paste(
    x$period_set_id, x$pathogen, sep = "\r"
  ))
  for (rows in groups) {
    if (length(rows) < 2L) next
    rows <- rows[order(x$start_date[rows], x$end_date[rows])]
    if (any(x$start_date[rows[-1L]] <= x$end_date[rows[-length(rows)]])) {
      .stop_contract(contract, "periods overlap within a mutually exclusive period set")
    }
  }
  invisible(x)
}

.new_period_resource <- function(periods, season_review, provenance) {
  validate_epidemiological_periods(periods)
  .require_data_frame(season_review, "epidemiological season review")
  .require_columns(
    season_review,
    c("season_id", "wave_count", "review_status", "source_reference", "note"),
    "epidemiological season review"
  )
  .check_character(season_review$season_id, "season_id", "epidemiological season review")
  .check_whole_number(
    season_review$wave_count, "wave_count", "epidemiological season review",
    non_negative = TRUE
  )
  .check_character(
    season_review$review_status, "review_status", "epidemiological season review"
  )
  if (anyDuplicated(season_review$season_id)) {
    .stop_contract("epidemiological season review", "season IDs must be unique")
  }
  observed <- table(factor(periods$season_id, levels = season_review$season_id))
  if (!identical(as.integer(observed), as.integer(season_review$wave_count))) {
    .stop_contract("epidemiological season review", "wave counts do not match period rows")
  }
  list(periods = periods, season_review = season_review, provenance = provenance)
}

.period_frame <- function(set, id, pathogen, season, type, label, start, end,
                          version, source, status = "reviewed", note = NA_character_,
                          variant_context = NA_character_,
                          historical_context = NA_character_,
                          evidence_class = "REVIEWED_EXTERNAL_PERIOD") {
  data.frame(
    period_set_id = set, period_id = id, pathogen = pathogen,
    season_id = season, period_type = type, label = label,
    start_date = as.Date(start), end_date = as.Date(end),
    definition_version = version, variant_context = variant_context,
    historical_context = historical_context, evidence_class = evidence_class,
    source_reference = source,
    review_status = status, note = note, stringsAsFactors = FALSE
  )
}

#' Frozen dissertation Influenza waves
#'
#' @return A reviewed period resource with `periods`, `season_review`, and
#'   `provenance`.
#' @export
dissertation_influenza_periods <- function() {
  source <- "RKI/AGI season reports; frozen dissertation reference"
  periods <- rbind(
    .period_frame("dissertation_influenza_v1", "influenza_2017_18", "Influenza, saisonal", "2017/18", "influenza_wave", "2017/18 Grippewelle", "2017-12-25", "2018-04-08", "dissertation_v1", source),
    .period_frame("dissertation_influenza_v1", "influenza_2018_19", "Influenza, saisonal", "2018/19", "influenza_wave", "2018/19 Grippewelle", "2019-01-07", "2019-04-07", "dissertation_v1", source),
    .period_frame("dissertation_influenza_v1", "influenza_2019_20", "Influenza, saisonal", "2019/20", "influenza_wave", "2019/20 Grippewelle", "2020-01-06", "2020-03-22", "dissertation_v1", source)
  )
  review <- data.frame(
    season_id = c("2017/18", "2018/19", "2019/20"), wave_count = 1L,
    review_status = "reviewed", source_reference = source,
    note = "Frozen dissertation analytical interval", stringsAsFactors = FALSE
  )
  .new_period_resource(periods, review, list(
    period_set_id = "dissertation_influenza_v1",
    definition_version = "dissertation_v1"
  ))
}

#' Frozen dissertation principal COVID Welle2 intervals
#'
#' @return A reviewed period resource.
#' @export
dissertation_covid_welle2_periods <- function() {
  bounds <- list(
    c("1", "2020-03-02", "2020-05-17", "1. Welle"),
    c("2", "2020-09-28", "2021-02-28", "2. Welle"),
    c("3", "2021-03-01", "2021-06-13", "3. Welle / Alpha"),
    c("4a", "2021-08-02", "2021-10-03", "4a / Delta Sommer"),
    c("4b", "2021-10-04", "2021-12-26", "4b / Delta Herbst"),
    c("5a", "2021-12-27", "2022-02-27", "5a / Omikron BA.1"),
    c("5b", "2022-02-28", "2022-05-29", "5b / Omikron BA.2")
  )
  periods <- do.call(rbind, lapply(bounds, function(x) .period_frame(
    "dissertation_covid_welle2_v1", paste0("covid_wave_", x[[1L]]),
    "COVID-19", NA_character_, "covid_wave", x[[4L]], x[[2L]], x[[3L]],
    "dissertation_v1", "03_Diss_SurvStat.Rmd; RKI retrospective phase classification",
    variant_context = if (x[[1L]] %in% c("3", "4a", "4b", "5a", "5b"))
      sub("^[^/]+/ ", "", x[[4L]]) else NA_character_,
    historical_context = "Dissertation / RKI-Pandemieperioden",
    evidence_class = "DISSERTATION_RKI_PANDEMIC_PERIOD"
  )))
  list(
    periods = periods,
    provenance = list(
      period_set_id = "dissertation_covid_welle2_v1",
      definition_version = "dissertation_v1",
      semantics = "principal_Welle2_not_broader_Phase"
    )
  )
}

#' Reviewed post-pandemic RKI COVID-19 activity waves
#'
#' These reviewed activity waves are deliberately separate from the numbered
#' pandemic-wave system. RKI phase 8 begins in ISO week 22 of 2022, but has no
#' sufficiently authoritative reviewed closing boundary and is therefore not
#' represented as a selectable closed interval.
#'
#' @return A reviewed period resource.
#' @export
rki_covid_activity_waves <- function() {
  source <- paste(
    "RKI Epidemiologisches Bulletin 35/2025:",
    "Symptomprofile, Erkrankungsraten und Sequenzierung ... GrippeWeb-Plus 2023\u20132025"
  )
  periods <- rbind(
    .period_frame(
      "covid_rki_activity_waves_v1", "covid_activity_2023_24", "COVID-19",
      "2023/24", "covid_activity_wave", "RKI-Aktivit\u00e4tswelle 2023/24",
      "2023-10-02", "2024-01-28", "covid_rki_activity_waves_v1", source,
      historical_context = "RKI-gepr\u00fcfte post-pandemische Aktivit\u00e4tswelle",
      evidence_class = "REVIEWED_RKI_ACTIVITY_WAVE"
    ),
    .period_frame(
      "covid_rki_activity_waves_v1", "covid_activity_2024_25", "COVID-19",
      "2024/25", "covid_activity_wave", "RKI-Aktivit\u00e4tswelle 2024/25",
      "2024-05-27", "2025-01-26", "covid_rki_activity_waves_v1", source,
      historical_context = "RKI-gepr\u00fcfte post-pandemische Aktivit\u00e4tswelle",
      evidence_class = "REVIEWED_RKI_ACTIVITY_WAVE"
    )
  )
  list(periods = periods, provenance = list(
    period_set_id = "covid_rki_activity_waves_v1",
    definition_version = "covid_rki_activity_waves_v1",
    evidence_class = "REVIEWED_RKI_ACTIVITY_WAVE",
    source_reference = source,
    phase_8_context = paste(
      "RKI phase 8 / sixth COVID-19 wave / Omicron BA.5 begins in 2022-KW22;",
      "no reviewed closing boundary is encoded."
    )
  ))
}

.iso_year_week <- function(date) {
  data.frame(
    year = as.integer(format(date, "%G")),
    week = as.integer(format(date, "%V"))
  )
}

#' Format an epidemiological period for display
#'
#' @param period One row from a validated epidemiological period table.
#' @return A named list containing `title` and `subtitle` plus ISO boundaries.
#' @export
format_epidemiological_period <- function(period) {
  validate_epidemiological_periods(period)
  if (nrow(period) != 1L) stop("`period` must contain exactly one row.", call. = FALSE)
  start <- .iso_year_week(period$start_date)
  end <- .iso_year_week(period$end_date)
  pathogen <- if (identical(period$pathogen, "Influenza, saisonal")) "Influenza" else period$pathogen
  iso <- if (start$year == end$year) {
    sprintf("%d-KW %02d\u2013%02d", start$year, start$week, end$week)
  } else {
    sprintf("%d-KW %02d\u2013%d-KW %02d", start$year, start$week, end$year, end$week)
  }
  list(
    title = paste(pathogen, period$label, sep = " \u00b7 "),
    subtitle = paste(
      format(period$start_date, "%d.%m.%Y"),
      format(period$end_date, "%d.%m.%Y"), sep = "\u2013"
    ) |> paste(iso, sep = " \u00b7 "),
    start_iso_year = start$year, start_iso_week = start$week,
    end_iso_year = end$year, end_iso_week = end$week
  )
}

#' Reviewed RKI Influenza waves, 2017/18--2025/26
#'
#' A season is not a wave. The separate season review explicitly represents
#' zero, one, or multiple officially reviewed waves.
#'
#' @return A reviewed period resource.
#' @export
rki_influenza_periods_2017_2026 <- function() {
  set <- "rki_influenza_waves_2017_2026_v1"
  version <- "reviewed_2026-08-29"
  rows <- list(
    c("2017_18_1", "2017/18", "2017-12-25", "2018-04-08", "RKI/AGI Saisonbericht 2017/18: https://influenza.rki.de/saisonberichte/2017.pdf", "Influenza B dominated"),
    c("2018_19_1", "2018/19", "2019-01-07", "2019-04-07", "RKI/AGI Saisonbericht 2018/19: https://influenza.rki.de/Saisonberichte/2018.pdf", "Influenza A(H1N1)pdm09 and A(H3N2)"),
    c("2019_20_1", "2019/20", "2020-01-06", "2020-03-22", "RKI/AGI Wochenbericht KW20/2020: https://influenza.rki.de/Wochenberichte/2019_2020/2020-20.pdf", "Shortened during early COVID-19 measures"),
    c("2021_22_1", "2021/22", "2022-04-25", "2022-05-22", "RKI/AGI ARE-Wochenbericht KW22/2022: https://influenza.rki.de/Wochenberichte/2021_2022/2022-22.pdf", "Unusual late low-activity wave"),
    c("2022_23_1", "2022/23", "2022-10-24", "2023-01-08", "RKI/AGI ARE-Wochenbericht KW18/2023: https://influenza.rki.de/Wochenberichte/2022_2023/2023-18.pdf", "Influenza A(H3N2) dominated"),
    c("2022_23_2", "2022/23", "2023-02-27", "2023-04-09", "RKI/AGI ARE-Wochenbericht KW18/2023: https://influenza.rki.de/Wochenberichte/2022_2023/2023-18.pdf", "Influenza B/Victoria dominated"),
    c("2023_24_1", "2023/24", "2023-12-11", "2024-03-24", "RKI ARE-Wochenbericht KW20/2024: https://influenza.rki.de/Wochenberichte/2023_2024/2024-20.pdf", "Influenza A(H1N1)pdm09 dominated"),
    c("2024_25_1", "2024/25", "2024-12-16", "2025-04-06", "RKI ARE-Wochenbericht KW16/2025: https://influenza.rki.de/Wochenberichte/2024_2025/2025-16.pdf", "Influenza B and A(H1N1)pdm09 similarly frequent"),
    c("2025_26_1", "2025/26", "2025-11-24", "2026-03-08", "RKI ARE-Wochenbericht KW20/2026: https://edoc.rki.de/handle/176904/13745; RKI ARE current situation", "Influenza A(H3N2) subclade K dominated")
  )
  periods <- do.call(rbind, lapply(rows, function(x) .period_frame(
    set, paste0("influenza_", x[[1L]]), "Influenza, saisonal", x[[2L]],
    "influenza_wave", paste(x[[2L]], "Grippewelle"), x[[3L]], x[[4L]],
    version, x[[5L]], note = x[[6L]]
  )))
  seasons <- c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24", "2024/25", "2025/26")
  counts <- c(1L, 1L, 1L, 0L, 1L, 2L, 1L, 1L, 1L)
  notes <- rep("Official RKI wave interval reviewed", length(seasons))
  notes[seasons == "2020/21"] <- "No Influenza wave identified; exceptionally low activity"
  notes[seasons == "2021/22"] <- "Late low-activity wave outside the usual winter period"
  review <- data.frame(
    season_id = seasons, wave_count = counts, review_status = "reviewed",
    source_reference = c(
      "RKI/AGI Saisonbericht 2017/18: https://influenza.rki.de/saisonberichte/2017.pdf",
      "RKI/AGI Saisonbericht 2018/19: https://influenza.rki.de/Saisonberichte/2018.pdf",
      "RKI/AGI Wochenbericht KW20/2020: https://influenza.rki.de/Wochenberichte/2019_2020/2020-20.pdf",
      "RKI/AGI Wochenbericht KW20/2021: https://influenza.rki.de/Wochenberichte/2020_2021/2021-20.pdf",
      "RKI/AGI ARE-Wochenbericht KW22/2022: https://influenza.rki.de/Wochenberichte/2021_2022/2022-22.pdf",
      "RKI/AGI ARE-Wochenbericht KW18/2023: https://influenza.rki.de/Wochenberichte/2022_2023/2023-18.pdf",
      "RKI ARE-Wochenbericht KW20/2024: https://influenza.rki.de/Wochenberichte/2023_2024/2024-20.pdf",
      "RKI ARE-Wochenbericht KW16/2025: https://influenza.rki.de/Wochenberichte/2024_2025/2025-16.pdf",
      "RKI ARE-Wochenbericht KW20/2026: https://edoc.rki.de/handle/176904/13745; RKI ARE current situation"
    ), note = notes, stringsAsFactors = FALSE
  )
  .new_period_resource(periods, review, list(
    period_set_id = set, definition_version = version,
    reviewed_at = as.Date("2026-08-29"), authority = "Robert Koch-Institut / Arbeitsgemeinschaft Influenza",
    boundary_semantics = "inclusive_dates_derived_from_official_ISO_calendar_weeks"
  ))
}

.period_data <- function(x) {
  if (is.data.frame(x)) return(x)
  .require_named_list(x, "periods", "epidemiological period resource")
  x$periods
}

#' Assign reviewed epidemiological periods
#'
#' @param surveillance Canonical surveillance observations with `date` and
#'   `pathogen`.
#' @param period_specification A period data frame or reviewed period resource.
#' @return A list with `data`, `diagnostics`, and `period_provenance`.
#' @export
assign_epidemiological_periods <- function(surveillance, period_specification) {
  contract <- "epidemiological period assignment"
  .require_data_frame(surveillance, contract)
  .require_columns(surveillance, c("date", "pathogen"), contract)
  .check_date(surveillance$date, "date", contract)
  .check_character(surveillance$pathogen, "pathogen", contract)
  periods <- .period_data(period_specification)
  validate_epidemiological_periods(periods)
  if (length(unique(periods$period_set_id)) != 1L) {
    .stop_contract(contract, "assignment requires exactly one period set")
  }
  pathogens <- unique(periods$pathogen)
  if (any(!surveillance$pathogen %in% pathogens)) {
    .stop_contract(contract, "surveillance pathogen is not compatible with the period set")
  }
  matches <- lapply(seq_len(nrow(surveillance)), function(i) which(
    periods$pathogen == surveillance$pathogen[[i]] &
      surveillance$date[[i]] >= periods$start_date &
      surveillance$date[[i]] <= periods$end_date
  ))
  if (any(lengths(matches) > 1L)) {
    .stop_contract(contract, "an observation matches more than one period")
  }
  selected <- vapply(matches, function(x) if (length(x)) x else NA_integer_, integer(1))
  output <- surveillance
  output$period_set_id <- ifelse(
    is.na(selected), unique(periods$period_set_id), periods$period_set_id[selected]
  )
  output$period_id <- periods$period_id[selected]
  output$period_definition_version <- periods$definition_version[selected]
  output$period_review_status <- periods$review_status[selected]
  list(
    data = output,
    diagnostics = list(
      observation_count = nrow(output), assigned_count = sum(!is.na(selected)),
      outside_period_count = sum(is.na(selected)),
      outside_period_dates = sort(unique(output$date[is.na(selected)])),
      assignment_counts = table(factor(output$period_id, levels = periods$period_id), useNA = "ifany")
    ),
    period_provenance = period_specification
  )
}
