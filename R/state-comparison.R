#' Reviewed district-to-state crosswalk
#'
#' Returns the reviewed 2024 administrative relationship between the 400
#' canonical German districts and the 16 Länder. District identities remain
#' unchanged. The crosswalk is derived deterministically from the reviewed
#' canonical AGS and state resources and is not an analytical scope or a basis
#' for fitting demographic typologies.
#'
#' @param version Only `"2024_v1"` is supported.
#' @return A validated district-to-state data frame.
#' @export
regionalepi_district_state_crosswalk <- function(version = "2024_v1") {
  if (!identical(version, "2024_v1")) {
    stop("Unsupported district-to-state crosswalk version.", call. = FALSE)
  }
  districts <- get(
    "regionalepi_geography_resources_2024", envir = asNamespace("regionalepi")
  )$bkg_districts
  states <- get(
    "regionalepi_state_boundaries_2024", envir = asNamespace("regionalepi")
  )$features
  state_id <- substr(districts$geo_id, 1L, 2L)
  position <- match(state_id, states$geo_id)
  x <- data.frame(
    geo_id = districts$geo_id,
    state_id = state_id,
    state_name = states$geo_name[position],
    reference_date = as.Date("2024-12-31"),
    source = paste(
      "Derived from regionalepi_geography_resources_2024 and",
      "regionalepi_state_boundaries_2024; reviewed AGS relationship"
    ),
    definition_version = "district_state_2024_v1",
    stringsAsFactors = FALSE
  )
  validate_district_state_crosswalk(x)
  x
}

#' Validate a district-to-state crosswalk
#'
#' The v0.1 contract describes exactly one reviewed state membership for every
#' canonical 2024 district. It creates no new geographic identity.
#'
#' @param x A district-to-state crosswalk.
#' @return `x`, invisibly.
#' @export
validate_district_state_crosswalk <- function(x) {
  contract <- "district-to-state crosswalk"
  required <- c(
    "geo_id", "state_id", "state_name", "reference_date", "source",
    "definition_version"
  )
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  for (field in setdiff(required, "reference_date")) {
    .check_character(x[[field]], field, contract)
  }
  .check_date(x$reference_date, "reference_date", contract)
  if (anyNA(x[required]) || nrow(x) != 400L || anyDuplicated(x$geo_id)) {
    .stop_contract(contract, "must contain exactly 400 complete unique district rows")
  }
  if (any(!grepl("^[0-9]{5}$", x$geo_id)) ||
      any(!grepl("^[0-9]{2}$", x$state_id))) {
    .stop_contract(contract, "district and state identifiers must retain canonical character formats")
  }
  if (length(unique(x$reference_date)) != 1L ||
      unique(x$reference_date) != as.Date("2024-12-31") ||
      length(unique(x$definition_version)) != 1L) {
    .stop_contract(contract, "reference date and definition version must be uniform")
  }
  districts <- get(
    "regionalepi_geography_resources_2024", envir = asNamespace("regionalepi")
  )$bkg_districts
  states <- get(
    "regionalepi_state_boundaries_2024", envir = asNamespace("regionalepi")
  )$features
  if (!setequal(x$geo_id, districts$geo_id) ||
      !setequal(unique(x$state_id), states$geo_id) ||
      any(x$state_id != substr(x$geo_id, 1L, 2L)) ||
      any(x$state_name != states$geo_name[match(x$state_id, states$geo_id)])) {
    .stop_contract(contract, "is inconsistent with the reviewed 2024 geography resources")
  }
  city_counts <- table(factor(x$state_id, levels = c("11", "02", "04")))
  if (!identical(as.integer(city_counts), c(1L, 1L, 2L))) {
    .stop_contract(contract, "Berlin, Hamburg, and Bremen must contain 1, 1, and 2 districts")
  }
  invisible(x)
}

#' Summarize national cluster composition by state
#'
#' Calculates the proportion of canonical districts in each Land assigned to
#' each nationally fitted cluster. Fractions describe districts, not population,
#' incidence, or cases. Absent clusters are retained as zero-count categories.
#'
#' @param assignments A data frame with unique `geo_id` and `cluster_id`.
#' @param crosswalk A validated district-to-state crosswalk.
#' @return A data frame with one row per state and cluster.
#' @export
summarize_cluster_composition_by_state <- function(
    assignments, crosswalk = regionalepi_district_state_crosswalk()) {
  contract <- "state cluster composition"
  .require_data_frame(assignments, contract)
  .require_columns(assignments, c("geo_id", "cluster_id"), contract)
  .check_character(assignments$geo_id, "geo_id", contract)
  .check_character(assignments$cluster_id, "cluster_id", contract)
  if (anyNA(assignments[c("geo_id", "cluster_id")]) ||
      anyDuplicated(assignments$geo_id)) {
    .stop_contract(contract, "assignments must have complete unique district identifiers")
  }
  validate_district_state_crosswalk(crosswalk)
  if (!setequal(assignments$geo_id, crosswalk$geo_id)) {
    .stop_contract(contract, "assignments must cover exactly the reviewed 400 districts")
  }
  clusters <- unique(assignments$cluster_id)
  joined <- merge(crosswalk[c("geo_id", "state_id", "state_name")], assignments,
                  by = "geo_id", sort = FALSE)
  grid <- expand.grid(
    state_id = unique(crosswalk$state_id), cluster_id = clusters,
    stringsAsFactors = FALSE
  )
  counts <- stats::aggregate(
    joined$geo_id, joined[c("state_id", "cluster_id")], length
  )
  names(counts)[[3L]] <- "n_districts"
  output <- merge(grid, counts, by = c("state_id", "cluster_id"), all.x = TRUE,
                  sort = FALSE)
  output$n_districts[is.na(output$n_districts)] <- 0L
  state_position <- match(output$state_id, crosswalk$state_id)
  output$state_name <- crosswalk$state_name[state_position]
  totals <- table(crosswalk$state_id)
  output$state_total_districts <- as.integer(totals[output$state_id])
  output$district_fraction <- output$n_districts / output$state_total_districts
  output <- output[order(output$state_name, match(output$cluster_id, clusters),
                         method = "radix"), c(
    "state_id", "state_name", "cluster_id", "n_districts",
    "state_total_districts", "district_fraction"
  )]
  rownames(output) <- NULL
  output
}

#' Create district-level state-comparison incidence summaries
#'
#' For every district, computes the median source-provided weekly incidence in
#' the supplied analytical period and retains its national cluster and reviewed
#' state membership. These are descriptive district summaries, not state
#' incidence estimates. Source-provided incidence is never summed, averaged, or
#' population-weighted across districts.
#'
#' @param data District-week surveillance data with national `cluster_id`.
#' @param crosswalk A validated district-to-state crosswalk.
#' @return A data frame with one row per observed district.
#' @export
summarize_state_cluster_incidence <- function(
    data, crosswalk = regionalepi_district_state_crosswalk()) {
  contract <- "state cluster incidence comparison"
  required <- c(
    "geo_id", "geo_name", "cluster_id", "date", "incidence", "cases"
  )
  .require_data_frame(data, contract)
  .require_columns(data, required, contract)
  for (field in c("geo_id", "geo_name", "cluster_id")) {
    .check_character(data[[field]], field, contract)
  }
  .check_date(data$date, "date", contract)
  .check_numeric(data$incidence, "incidence", contract, allow_na = TRUE)
  .check_numeric(data$cases, "cases", contract, allow_na = TRUE)
  if (anyNA(data[c("geo_id", "geo_name", "cluster_id", "date")]) ||
      anyDuplicated(paste(data$geo_id, data$date, sep = "\r"))) {
    .stop_contract(contract, "district/date keys and descriptive fields must be complete and unique")
  }
  validate_district_state_crosswalk(crosswalk)
  if (any(!unique(data$geo_id) %in% crosswalk$geo_id)) {
    .stop_contract(contract, "all districts must occur in the reviewed crosswalk")
  }
  expected_dates <- sort(unique(data$date))
  rows <- split(seq_len(nrow(data)), data$geo_id)
  output <- do.call(rbind, lapply(rows, function(index) {
    x <- data[index, , drop = FALSE]
    if (length(unique(x$geo_name)) != 1L || length(unique(x$cluster_id)) != 1L) {
      .stop_contract(contract, "district name and national cluster must be stable within the period")
    }
    incidence <- x$incidence
    cases <- x$cases
    data.frame(
      geo_id = x$geo_id[[1L]], geo_name = x$geo_name[[1L]],
      cluster_id = x$cluster_id[[1L]],
      period_median_incidence = if (all(is.na(incidence))) NA_real_ else
        stats::median(incidence, na.rm = TRUE),
      observed_weeks = sum(!is.na(incidence)),
      expected_weeks = length(expected_dates),
      missing_weeks = length(expected_dates) - sum(!is.na(incidence)),
      cumulative_reported_cases = if (all(is.na(cases))) NA_real_ else
        sum(cases, na.rm = TRUE), stringsAsFactors = FALSE
    )
  }))
  position <- match(output$geo_id, crosswalk$geo_id)
  output$state_id <- crosswalk$state_id[position]
  output$state_name <- crosswalk$state_name[position]
  output <- output[order(output$state_name, output$cluster_id, output$geo_name,
                         output$geo_id, method = "radix"), c(
    "geo_id", "geo_name", "state_id", "state_name", "cluster_id",
    "period_median_incidence", "observed_weeks", "expected_weeks",
    "missing_weeks", "cumulative_reported_cases"
  )]
  rownames(output) <- NULL
  output
}
