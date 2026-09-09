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

#' Reviewed aggregated-state comparison groups
#'
#' Returns a versioned mapping from all 16 canonical Länder to 12 neutral
#' descriptive comparison groups. The composition corresponds to the regional
#' grouping documented by the RKI Arbeitsgemeinschaft Influenza, but is not a
#' new canonical geography or an epidemiological analysis scope.
#'
#' @return A validated state-to-comparison-group data frame.
#' @export
regionalepi_state_comparison_groups <- function() {
  states <- regionalepi_state_boundaries()$features
  grouped <- c(
    `01` = "SH_HH", `02` = "SH_HH", `03` = "NI_HB", `04` = "NI_HB",
    `05` = "NW", `06` = "HE", `07` = "RP_SL", `08` = "BW",
    `09` = "BY", `10` = "RP_SL", `11` = "BB_BE", `12` = "BB_BE",
    `13` = "MV", `14` = "SN", `15` = "ST", `16` = "TH"
  )
  labels <- c(
    BW = "Baden-W\u00fcrttemberg", BY = "Bayern",
    BB_BE = "Brandenburg/Berlin", HE = "Hessen",
    MV = "Mecklenburg-Vorpommern", NI_HB = "Niedersachsen/Bremen",
    NW = "Nordrhein-Westfalen", RP_SL = "Rheinland-Pfalz/Saarland",
    SN = "Sachsen", ST = "Sachsen-Anhalt",
    SH_HH = "Schleswig-Holstein/Hamburg", TH = "Th\u00fcringen"
  )
  x <- data.frame(
    comparison_group_id = unname(grouped[states$geo_id]),
    comparison_group_name = unname(labels[grouped[states$geo_id]]),
    state_id = states$geo_id,
    definition_version = "aggregated_states_12_v1",
    source = paste(
      "regionalepi reviewed descriptive grouping; composition corresponds to",
      "RKI Arbeitsgemeinschaft Influenza 12-region grouping,",
      "https://influenza.rki.de/Glossar.aspx"
    ), stringsAsFactors = FALSE
  )
  validate_state_comparison_groups(x)
  x
}

#' Validate aggregated-state comparison groups
#' @param x A state-to-comparison-group mapping.
#' @return `x`, invisibly.
#' @export
validate_state_comparison_groups <- function(x) {
  contract <- "aggregated-state comparison groups"
  required <- c("comparison_group_id", "comparison_group_name", "state_id",
                "definition_version", "source")
  .require_data_frame(x, contract); .require_columns(x, required, contract)
  if (!identical(names(x), required))
    .stop_contract(contract, "fields are missing, unexpected, or reordered")
  for (field in required) .check_character(x[[field]], field, contract)
  states <- regionalepi_state_boundaries()$features
  if (nrow(x) != 16L || anyNA(x) || anyDuplicated(x$state_id) ||
      !setequal(x$state_id, states$geo_id) ||
      length(unique(x$comparison_group_id)) != 12L ||
      length(unique(x$definition_version)) != 1L) {
    .stop_contract(contract,
      "must map every one of the 16 canonical states exactly once to 12 groups")
  }
  names_per_id <- tapply(x$comparison_group_name, x$comparison_group_id,
                         function(value) length(unique(value)))
  if (any(names_per_id != 1L))
    .stop_contract(contract, "group IDs and names must have a one-to-one relationship")
  expected <- c(`11`="BB_BE", `12`="BB_BE", `04`="NI_HB", `03`="NI_HB",
                `02`="SH_HH", `01`="SH_HH", `10`="RP_SL", `07`="RP_SL")
  if (!identical(unname(x$comparison_group_id[match(names(expected), x$state_id)]),
                 unname(expected)))
    .stop_contract(contract, "the four reviewed combined groups are incorrect")
  invisible(x)
}

.regional_comparison_groups <- function() {
  states <- regionalepi_state_boundaries()$features
  aggregated <- regionalepi_state_comparison_groups()
  gross_id <- c(`01`="north", `02`="north", `03`="north", `04`="north",
    `05`="middle_west", `06`="middle_west", `07`="middle_west",
    `08`="south", `09`="south", `10`="middle_west", `11`="east",
    `12`="east", `13`="east", `14`="east", `15`="east", `16`="east")
  gross_name <- c(north="Norden (West)", middle_west="Mitte (West)",
                  south="S\u00fcden", east="Osten")
  make <- function(level, id, name, version, source) data.frame(
    comparison_level=level, comparison_group_id=unname(id),
    comparison_group_name=unname(name), state_id=states$geo_id,
    definition_version=version, source=source, stringsAsFactors=FALSE)
  out <- rbind(
    make("grossregion", gross_id[states$geo_id],
      gross_name[gross_id[states$geo_id]], "grossregions_4_v1",
      paste("regionalepi reviewed descriptive grouping corresponding to the",
        "four AGI-Gro\u00dfregionen documented by RKI/AGI; not a canonical",
        "administrative or pathogen-specific geography")),
    make("aggregated_state",
      stats::setNames(aggregated$comparison_group_id, aggregated$state_id)[states$geo_id],
      stats::setNames(aggregated$comparison_group_name, aggregated$state_id)[states$geo_id],
      "aggregated_states_12_v1", unique(aggregated$source)),
    make("state", states$geo_id, states$geo_name, "states_16_2024_v1",
      "Canonical L\u00e4nder from regionalepi_state_boundaries_2024; descriptive comparison membership")
  )
  .validate_regional_comparison_groups(out)
  out
}

.validate_regional_comparison_groups <- function(x) {
  contract <- "regional comparison groups"
  required <- c("comparison_level", "comparison_group_id",
    "comparison_group_name", "state_id", "definition_version", "source")
  .require_data_frame(x, contract); .require_columns(x, required, contract)
  if (!identical(names(x), required))
    .stop_contract(contract, "fields are missing, unexpected, or reordered")
  for (field in required) .check_character(x[[field]], field, contract)
  expected_levels <- c(grossregion=4L, aggregated_state=12L, state=16L)
  if (anyNA(x) || !setequal(unique(x$comparison_level), names(expected_levels)))
    .stop_contract(contract, "must contain exactly the reviewed 4/12/16 levels")
  states <- regionalepi_state_boundaries()$features$geo_id
  for (level in names(expected_levels)) {
    z <- x[x$comparison_level == level, , drop=FALSE]
    if (nrow(z) != 16L || anyDuplicated(z$state_id) ||
        !setequal(z$state_id, states) ||
        length(unique(z$comparison_group_id)) != expected_levels[[level]] ||
        length(unique(z$definition_version)) != 1L)
      .stop_contract(contract, "each level must map all 16 L\u00e4nder exactly once to its reviewed number of groups")
    names_per_id <- tapply(z$comparison_group_name, z$comparison_group_id,
      function(value) length(unique(value)))
    if (any(names_per_id != 1L))
      .stop_contract(contract, "group IDs and names must have a one-to-one relationship within each level")
  }
  invisible(x)
}

.regional_comparison_membership <- function(
    crosswalk, comparison_level = c("grossregion", "aggregated_state", "state"),
    groups = .regional_comparison_groups()) {
  aliases <- c(aggregated_states="aggregated_state", states="state")
  if (length(comparison_level) == 1L && comparison_level %in% names(aliases))
    comparison_level <- unname(aliases[[comparison_level]])
  comparison_level <- match.arg(comparison_level,
    c("grossregion", "aggregated_state", "state"))
  validate_district_state_crosswalk(crosswalk)
  if (!"comparison_level" %in% names(groups)) {
    validate_state_comparison_groups(groups)
    if (!identical(comparison_level, "aggregated_state"))
      groups <- .regional_comparison_groups()
    else groups <- data.frame(comparison_level="aggregated_state", groups,
      stringsAsFactors=FALSE)
  } else .validate_regional_comparison_groups(groups)
  groups <- groups[groups$comparison_level == comparison_level, , drop=FALSE]
  position <- match(crosswalk$state_id, groups$state_id)
  out <- data.frame(
    geo_id=crosswalk$geo_id, state_id=crosswalk$state_id,
    state_name=crosswalk$state_name,
    comparison_id=groups$comparison_group_id[position],
    comparison_name=groups$comparison_group_name[position],
    comparison_level=comparison_level, stringsAsFactors=FALSE)
  if (nrow(out) != 400L || anyDuplicated(out$geo_id) || anyNA(out$comparison_id))
    .stop_contract("regional comparison membership",
      "all 400 canonical districts must resolve exactly once")
  out
}

#' Summarize national cluster composition by regional comparison level
#' @param assignments A data frame with unique `geo_id` and `cluster_id`.
#' @param comparison_level One of `"grossregion"`, `"aggregated_state"`, or
#'   `"state"`. The former plural spellings remain accepted for compatibility.
#' @param crosswalk A validated district-to-state crosswalk.
#' @param comparison_groups A validated state-to-comparison-group mapping.
#' @return One row per comparison unit and national cluster.
#' @export
summarize_cluster_composition_by_region <- function(
    assignments, comparison_level = c("grossregion", "aggregated_state", "state"),
    crosswalk = regionalepi_district_state_crosswalk(),
    comparison_groups = .regional_comparison_groups()) {
  comparison_level <- comparison_level[[1L]]
  membership <- .regional_comparison_membership(
    crosswalk, comparison_level, comparison_groups)
  summarize_cluster_composition_by_state(assignments, crosswalk)
  joined <- merge(assignments, membership, by="geo_id", sort=FALSE)
  clusters <- unique(assignments$cluster_id)
  units <- unique(membership[c("comparison_id", "comparison_name")])
  grid <- merge(units, data.frame(cluster_id=clusters), all=TRUE)
  counts <- stats::aggregate(joined$geo_id,
    joined[c("comparison_id", "comparison_name", "cluster_id")], length)
  names(counts)[[4L]] <- "n_districts"
  out <- merge(grid, counts,
    by=c("comparison_id", "comparison_name", "cluster_id"), all.x=TRUE)
  out$n_districts[is.na(out$n_districts)] <- 0L
  totals <- table(membership$comparison_id)
  out$comparison_total_districts <- as.integer(totals[out$comparison_id])
  out$district_fraction <- out$n_districts / out$comparison_total_districts
  out$comparison_level <- comparison_level
  out <- out[order(out$comparison_name, match(out$cluster_id, clusters)), c(
    "comparison_id", "comparison_name", "comparison_level", "cluster_id",
    "n_districts", "comparison_total_districts", "district_fraction")]
  rownames(out) <- NULL
  out
}

.regional_group_choices <- function(comparison_level, crosswalk,
                                    groups=.regional_comparison_groups()) {
  membership <- .regional_comparison_membership(crosswalk, comparison_level, groups)
  units <- unique(membership[c("comparison_id", "comparison_name")])
  units <- units[order(units$comparison_name, method="radix"), , drop=FALSE]
  stats::setNames(units$comparison_id, units$comparison_name)
}

.summarize_weekly_regional_incidence <- function(
    data, membership, comparison_ids=NULL, by_cluster=FALSE) {
  contract <- "weekly regional district-incidence summary"
  .require_data_frame(data, contract)
  .require_columns(data, c("geo_id", "date", "incidence", "cluster_id"), contract)
  .check_character(data$geo_id, "geo_id", contract)
  .check_character(data$cluster_id, "cluster_id", contract)
  .check_date(data$date, "date", contract)
  .check_numeric(data$incidence, "incidence", contract, allow_na=TRUE)
  if (anyDuplicated(paste(data$geo_id, data$date, sep="\r")))
    .stop_contract(contract, "district/date keys must be unique")
  if (is.null(comparison_ids)) comparison_ids <- unique(membership$comparison_id)
  selected_membership <- membership[membership$comparison_id %in% comparison_ids, , drop=FALSE]
  joined <- merge(data, selected_membership[c("geo_id","comparison_id","comparison_name")],
    by="geo_id", sort=FALSE)
  dates <- sort(unique(data$date)); clusters <- unique(data$cluster_id)
  units <- unique(selected_membership[c("comparison_id","comparison_name")])
  grid <- merge(units, data.frame(date=dates), all=TRUE)
  if (by_cluster) grid <- merge(grid, data.frame(cluster_id=clusters), all=TRUE)
  key_cols <- c("comparison_id","comparison_name","date",
                if (by_cluster) "cluster_id")
  expected <- unique(data[c("geo_id","cluster_id")])
  expected <- merge(expected, selected_membership[c("geo_id","comparison_id","comparison_name")],
    by="geo_id", sort=FALSE)
  expected_n <- stats::aggregate(expected$geo_id, expected[key_cols[key_cols != "date"]], length)
  names(expected_n)[ncol(expected_n)] <- "expected_districts"
  pieces <- split(seq_len(nrow(joined)), interaction(joined[key_cols], drop=TRUE))
  values <- do.call(rbind, lapply(pieces, function(i) {
    z <- joined[i,,drop=FALSE]; observed <- z$incidence[!is.na(z$incidence)]
    row <- z[1L,key_cols,drop=FALSE]
    row$median_incidence <- if(length(observed)) stats::median(observed) else NA_real_
    row$q1_incidence <- if(length(observed)) unname(stats::quantile(observed,.25)) else NA_real_
    row$q3_incidence <- if(length(observed)) unname(stats::quantile(observed,.75)) else NA_real_
    row$observed_districts <- length(observed); row
  }))
  out <- merge(grid, values, by=key_cols, all.x=TRUE, sort=FALSE)
  out <- merge(out, expected_n, by=key_cols[key_cols != "date"], all.x=TRUE, sort=FALSE)
  out$observed_districts[is.na(out$observed_districts)] <- 0L
  out$expected_districts[is.na(out$expected_districts)] <- 0L
  out$missing_districts <- out$expected_districts - out$observed_districts
  out$completeness <- ifelse(out$expected_districts > 0L,
    out$observed_districts / out$expected_districts, NA_real_)
  out <- out[order(out$date, out$comparison_name,
    if(by_cluster) out$cluster_id else seq_len(nrow(out))), , drop=FALSE]
  rownames(out) <- NULL; out
}

.summarize_germany_weekly_incidence <- function(data) {
  membership <- data.frame(geo_id=unique(data$geo_id), comparison_id="DE",
    comparison_name="Deutschland", stringsAsFactors=FALSE)
  .summarize_weekly_regional_incidence(data, membership, "DE", FALSE)
}

.regional_relative_activity <- function(data, membership, comparison_id) {
  cluster <- .summarize_weekly_regional_incidence(
    data, membership, comparison_id, by_cluster = TRUE
  )
  reference <- .summarize_weekly_regional_incidence(
    data, membership, comparison_id, by_cluster = FALSE
  )
  reference <- reference[c(
    "comparison_id", "comparison_name", "date", "median_incidence",
    "observed_districts", "expected_districts", "missing_districts",
    "completeness"
  )]
  names(reference)[4:8] <- c(
    "regional_reference_median", "regional_observed_districts",
    "regional_expected_districts", "regional_missing_districts",
    "regional_completeness"
  )
  out <- merge(
    cluster, reference,
    by = c("comparison_id", "comparison_name", "date"),
    all.x = TRUE, sort = FALSE
  )
  if (nrow(out) != nrow(cluster)) {
    .stop_contract(
      "regional relative activity",
      "cluster and focal-region weekly references are incompatible"
    )
  }
  out$regional_relative_activity <-
    out$median_incidence - out$regional_reference_median
  out <- out[order(out$date, out$cluster_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.regional_comparison_limit <- function(comparison_level) {
  if (length(comparison_level) != 1L || is.na(comparison_level) ||
      !comparison_level %in% c("grossregion","aggregated_state","state"))
    stop("Unsupported regional comparison level.",call.=FALSE)
  if (identical(comparison_level,"grossregion")) 3L else 2L
}

.reconcile_regional_comparisons <- function(comparison_level, focal,
                                            selected, valid_ids) {
  limit <- .regional_comparison_limit(comparison_level)
  valid_ids <- unique(as.character(valid_ids))
  if (length(focal) != 1L || is.na(focal) || !focal %in% valid_ids)
    stop("The focal region is not valid for the selected comparison level.",
      call.=FALSE)
  selected <- as.character(selected)
  selected <- selected[!is.na(selected) & selected != focal &
                         selected %in% valid_ids]
  utils::head(unique(selected), limit)
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
