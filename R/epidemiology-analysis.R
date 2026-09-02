#' Validate typology/surveillance compatibility
#'
#' @param x A reviewed compatibility specification.
#' @return `x`, invisibly.
#' @export
validate_typology_surveillance_compatibility <- function(x) {
  contract <- "typology surveillance compatibility"
  .require_named_list(x, c(
    "compatibility_id", "typology_id", "typology_definition_version",
    "surveillance_scope_id", "expected_typology_only_geo_ids",
    "expected_surveillance_only_geo_ids", "review_status", "reason"
  ), contract)
  for (field in c(
    "compatibility_id", "typology_id", "typology_definition_version",
    "surveillance_scope_id", "review_status", "reason"
  )) .check_scalar_character(x[[field]], field, contract)
  for (field in c("expected_typology_only_geo_ids", "expected_surveillance_only_geo_ids")) {
    .check_character(x[[field]], field, contract)
    if (anyDuplicated(x[[field]])) .stop_contract(contract, paste0("`", field, "` must be unique"))
    if (length(x[[field]]) && any(!grepl("^[0-9]{5}$", x[[field]]))) {
      .stop_contract(contract, paste0("`", field, "` must contain five-character AGS"))
    }
  }
  if (!identical(x$review_status, "reviewed")) {
    .stop_contract(contract, "`review_status` must be reviewed")
  }
  for (field in c("expected_typology_count", "expected_surveillance_count")) {
    if (field %in% names(x)) {
      .check_scalar_number(x[[field]], field, contract, non_negative = TRUE)
      if (x[[field]] != floor(x[[field]])) {
        .stop_contract(contract, paste0("`", field, "` must be whole-valued"))
      }
    }
  }
  invisible(x)
}

#' Dissertation typology/surveillance compatibility
#'
#' @return The reviewed 401-to-400 compatibility specification.
#' @export
dissertation_surveillance_compatibility <- function() {
  x <- list(
    compatibility_id = "dissertation_typology_surveillance_v1",
    typology_id = "dissertation_v1",
    typology_definition_version = "dissertation_v1",
    surveillance_scope_id = "dissertation_survstat_400",
    expected_typology_only_geo_ids = "16056",
    expected_surveillance_only_geo_ids = character(),
    expected_typology_count = 401L, expected_surveillance_count = 400L,
    review_status = "reviewed",
    reason = "Eisenach absent from analytical surveillance; Wartburgkreis retained; both historical ClA; no reassignment"
  )
  validate_typology_surveillance_compatibility(x)
  x
}

.typology_parts <- function(typology) {
  if (is.data.frame(typology)) return(list(data = typology, provenance = list()))
  .require_named_list(typology, c("data", "provenance"), "typology attachment")
  list(data = typology$data, provenance = typology$provenance)
}

#' Attach a reviewed typology to surveillance
#'
#' Joins canonical character identifiers only. No name matching, cluster
#' fitting, reassignment, or epidemiological transformation is performed.
#'
#' @param surveillance Canonical surveillance observations.
#' @param typology Typology assignments or a fit-like list with data/provenance.
#' @param compatibility Reviewed compatibility specification.
#' @return A list with `data`, `diagnostics`, and `provenance`.
#' @export
attach_typology <- function(surveillance, typology, compatibility) {
  contract <- "typology attachment"
  validate_typology_surveillance_compatibility(compatibility)
  .require_data_frame(surveillance, contract)
  .require_columns(surveillance, "geo_id", contract)
  .check_character(surveillance$geo_id, "geo_id", contract)
  parts <- .typology_parts(typology)
  assignments <- parts$data
  .require_data_frame(assignments, contract)
  .require_columns(assignments, "geo_id", contract)
  .check_character(assignments$geo_id, "geo_id", contract)
  if (anyDuplicated(assignments$geo_id)) {
    .stop_contract(contract, "typology assignments must be unique by geo_id")
  }
  if (any(!grepl("^[0-9]{5}$", c(surveillance$geo_id, assignments$geo_id)))) {
    .stop_contract(contract, "geographic identifiers must be five-character AGS")
  }
  cluster_field <- if ("cluster_code" %in% names(assignments) &&
      any(!is.na(assignments$cluster_code))) "cluster_code" else if (
    "cluster_id" %in% names(assignments)) "cluster_id" else if (
    "raw_cluster" %in% names(assignments)) "raw_cluster" else NA_character_
  if (is.na(cluster_field)) .stop_contract(contract, "typology has no cluster identity")
  typology_ids <- unique(assignments$geo_id)
  surveillance_ids <- unique(surveillance$geo_id)
  typology_only <- sort(setdiff(typology_ids, surveillance_ids))
  surveillance_only <- sort(setdiff(surveillance_ids, typology_ids))
  if ("expected_typology_count" %in% names(compatibility) &&
      length(typology_ids) != compatibility$expected_typology_count) {
    .stop_contract(contract, "typology geography count is not the reviewed expectation")
  }
  if ("expected_surveillance_count" %in% names(compatibility) &&
      length(surveillance_ids) != compatibility$expected_surveillance_count) {
    .stop_contract(contract, "surveillance geography count is not the reviewed expectation")
  }
  if (!identical(typology_only, sort(compatibility$expected_typology_only_geo_ids)) ||
      !identical(surveillance_only, sort(compatibility$expected_surveillance_only_geo_ids))) {
    .stop_contract(contract, "observed geography mismatch is not the reviewed compatibility")
  }
  position <- match(surveillance$geo_id, assignments$geo_id)
  if (anyNA(position)) .stop_contract(contract, "surveillance contains an unexpected missing typology assignment")
  output <- surveillance
  output$typology_id <- compatibility$typology_id
  output$typology_definition_version <- compatibility$typology_definition_version
  output$cluster_id <- as.character(assignments[[cluster_field]][position])
  if ("cluster_label" %in% names(assignments)) {
    output$cluster_label <- assignments$cluster_label[position]
  }
  list(
    data = output,
    diagnostics = list(
      surveillance_geo_count = length(surveillance_ids),
      typology_geo_count = length(typology_ids),
      expected_typology_only_geo_ids = typology_only,
      unexpected_missing_geo_ids = character(),
      compatibility_id = compatibility$compatibility_id
    ),
    provenance = list(
      compatibility = compatibility, typology = parts$provenance,
      preserved_query_ids = if ("query_id" %in% names(output)) sort(unique(output$query_id)) else character()
    )
  )
}

#' Summarize weekly source-provided incidence by typology
#'
#' Calculates the median and empirical first and third quartiles of district
#' incidences (R's default `quantile()` type 7). It never calculates a
#' pooled or population-weighted rate. Numeric zero remains zero and missing
#' incidence remains distinct.
#'
#' @param data Period-assigned, typology-attached surveillance incidence.
#' @param statistic Only `"median"` is supported.
#' @param na_policy Only explicit `"omit"` is supported in v0.1.
#' @param minimum_group_size Non-negative whole-valued minimum observed count.
#' @return A list with `data`, `diagnostics`, and `provenance`.
#' @export
summarize_incidence_by_typology <- function(
    data, statistic = "median", na_policy = "omit", minimum_group_size = 1L) {
  contract <- "incidence typology summary"
  if (!identical(statistic, "median")) .stop_contract(contract, "only median is supported")
  if (!identical(na_policy, "omit")) .stop_contract(contract, "only explicit NA omission is supported")
  .check_scalar_number(minimum_group_size, "minimum_group_size", contract, non_negative = TRUE)
  if (minimum_group_size != floor(minimum_group_size)) .stop_contract(contract, "minimum group size must be whole-valued")
  validate_surveillance_incidence(data)
  grouping <- c(
    "pathogen", "period_set_id", "period_id", "date", "typology_id",
    "typology_definition_version", "cluster_id"
  )
  .require_columns(data, grouping, contract)
  for (field in setdiff(grouping, "date")) {
    .check_character(data[[field]], field, contract, allow_na = field == "period_id")
  }
  if (anyNA(data$period_id)) .stop_contract(contract, "unassigned observations must be removed explicitly")
  observation_key <- do.call(paste, c(data[c(grouping, "geo_id")], sep = "\r"))
  if (anyDuplicated(observation_key)) {
    .stop_contract(contract, "each district may occur only once in an output group")
  }
  district_key <- unique(data[, c("typology_id", "typology_definition_version", "cluster_id", "geo_id")])
  expected <- stats::aggregate(
    district_key$geo_id,
    district_key[c("typology_id", "typology_definition_version", "cluster_id")],
    function(x) length(unique(x))
  )
  names(expected)[[4L]] <- "expected_district_count"
  keys <- interaction(data[grouping], drop = TRUE, lex.order = TRUE)
  rows <- split(seq_len(nrow(data)), keys)
  output <- do.call(rbind, lapply(rows, function(index) {
    values <- data$incidence[index]
    first <- data[index[[1L]], grouping, drop = FALSE]
    epos <- match(
      paste(first$typology_id, first$typology_definition_version, first$cluster_id),
      paste(expected$typology_id, expected$typology_definition_version, expected$cluster_id)
    )
    observed <- sum(!is.na(values))
    first$median_incidence <- if (observed) stats::median(values, na.rm = TRUE) else NA_real_
    quartiles <- if (observed) stats::quantile(
      values, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE, type = 7
    ) else c(NA_real_, NA_real_)
    first$q1_incidence <- quartiles[[1L]]
    first$q3_incidence <- quartiles[[2L]]
    first$expected_district_count <- expected$expected_district_count[[epos]]
    first$observed_non_missing_count <- observed
    first$missing_count <- sum(is.na(values))
    first$zero_count <- sum(values == 0, na.rm = TRUE)
    first$completeness_proportion <- observed / first$expected_district_count
    first$minimum_group_size_met <- observed >= minimum_group_size && observed > 0L
    first$query_ids <- paste(sort(unique(data$query_id[index])), collapse = "|")
    first
  }))
  rownames(output) <- NULL
  list(
    data = output,
    diagnostics = list(
      group_count = nrow(output), all_na_group_count = sum(is.na(output$median_incidence)),
      below_minimum_group_count = sum(!output$minimum_group_size_met),
      statistic = statistic, na_policy = na_policy,
      estimand = "median_of_district_source_provided_incidence"
    ),
    provenance = list(
      query_ids = sort(unique(data$query_id)),
      period_set_ids = sort(unique(data$period_set_id)),
      typology_ids = sort(unique(data$typology_id))
    )
  )
}

#' Summarize selected-period incidence for each district
#'
#' Produces exactly one unweighted district-specific median of weekly
#' source-provided incidence. District-week observations are not pooled across
#' districts.
#'
#' @param data Period-assigned, typology-attached surveillance incidence.
#' @param na_policy Only explicit `"omit"` is supported.
#' @return A list with `data`, `diagnostics`, and `provenance`.
#' @export
summarize_period_incidence_by_district <- function(data, na_policy = "omit") {
  contract <- "district period incidence summary"
  if (!identical(na_policy, "omit")) {
    .stop_contract(contract, "only explicit NA omission is supported")
  }
  validate_surveillance_incidence(data)
  grouping <- c(
    "pathogen", "period_set_id", "period_id", "typology_id",
    "typology_definition_version", "cluster_id", "geo_id", "geo_name"
  )
  .require_columns(data, grouping, contract)
  if (anyNA(data$period_id)) {
    .stop_contract(contract, "unassigned observations must be removed explicitly")
  }
  observation_key <- paste(data$period_set_id, data$period_id, data$geo_id,
                           data$date, sep = "\r")
  if (anyDuplicated(observation_key)) {
    .stop_contract(contract, "each district may occur only once per date")
  }
  keys <- interaction(data[grouping], drop = TRUE, lex.order = TRUE)
  rows <- split(seq_len(nrow(data)), keys)
  output <- do.call(rbind, lapply(rows, function(index) {
    first <- data[index[[1L]], grouping, drop = FALSE]
    values <- data$incidence[index]
    observed <- sum(!is.na(values))
    first$median_period_incidence <- if (observed) {
      stats::median(values, na.rm = TRUE)
    } else NA_real_
    first$expected_week_count <- length(unique(data$date[index]))
    first$observed_week_count <- observed
    first$missing_week_count <- sum(is.na(values))
    first$zero_week_count <- sum(values == 0, na.rm = TRUE)
    first$completeness_proportion <- observed / first$expected_week_count
    first
  }))
  rownames(output) <- NULL
  list(
    data = output,
    diagnostics = list(
      district_count = nrow(output),
      all_missing_district_count = sum(is.na(output$median_period_incidence)),
      na_policy = na_policy,
      estimand = "district_median_of_weekly_source_provided_incidence",
      weighting = "none"
    ),
    provenance = list(
      query_ids = sort(unique(data$query_id)),
      period_set_ids = sort(unique(data$period_set_id)),
      typology_ids = sort(unique(data$typology_id))
    )
  )
}
