#' Resolve source geography identities
#'
#' Resolves unique source geographies deterministically against a dated
#' canonical geography register. SurvStat v0.1 parsing supports only the
#' explicitly observed `LK`, `SK`, and special regional forms. Exact identity
#' matching uses both candidate name and caller-declared
#' canonical type. Reviewed aliases are considered only after exact matching
#' fails. No fuzzy or normalized matching is performed.
#'
#' A reviewed `spatial_units` row represents a real source-level geographic
#' identity that must remain distinct until a later explicit spatial relation
#' is applied. The resolver never supplies that later aggregation target.
#'
#' `reference_date` selects applicable register/directive rows; it is not the
#' source territorial vintage. The returned `geo_vintage` is unchanged.
#'
#' @param data Canonical count surveillance or source-provided incidence data.
#' @param geography A canonical register satisfying `validate_geography()`.
#' @param reference_date Complete scalar `Date` used to select register rows.
#' @param source Complete scalar source system; initially `"SurvStat@RKI"`.
#' @param canonical_type_col Complete scalar character naming a complete
#'   character type column in `geography`.
#' @param aliases Optional table satisfying `validate_geography_aliases()`.
#' @param spatial_units Optional reviewed source-unit registry. See Details.
#' @return An ordinary list with `data`, a one-row-per-source-unit `resolution`
#'   audit, and compact `diagnostics`.
#'
#' @details
#' `spatial_units` requires `source`, `source_version`, `source_label`,
#' `source_type`, `source_geo_id`, `source_geo_name`, `source_geo_level`,
#' `valid_from`, `valid_to`, `reason`, and `review_status`. Source identifiers
#' are opaque character values and are never inferred from labels.
#' @export
resolve_geography <- function(
    data, geography, reference_date, source, canonical_type_col,
    aliases = NULL, spatial_units = NULL) {
  contract <- "geography resolution"
  .validate_surveillance_observations(data)
  validate_geography(geography)
  .check_resolution_scalar_date(reference_date, "reference_date", contract)
  .check_resolution_scalar_character(source, "source", contract)
  .check_resolution_scalar_character(
    canonical_type_col, "canonical_type_col", contract
  )
  provenance <- c("source_geo_id", "source_geo_name", "source_geo_level")
  conflicts <- intersect(provenance, names(data))
  if (length(conflicts)) {
    .stop_contract(
      contract,
      sprintf("input already contains reserved provenance column(s): %s",
              paste(conflicts, collapse = ", "))
    )
  }
  if (any(data$source != source)) {
    .stop_contract(contract, "all input `source` values must equal `source`")
  }
  if (!canonical_type_col %in% names(geography)) {
    .stop_contract(contract, "`canonical_type_col` is missing from `geography`")
  }
  .check_character(
    geography[[canonical_type_col]], canonical_type_col, contract
  )
  .check_optional(geography, "vghid", .check_character, contract, allow_na = TRUE)
  if (!is.null(aliases)) {
    validate_geography_aliases(aliases)
  }
  if (!is.null(spatial_units)) {
    .validate_spatial_units(spatial_units)
    .validate_applicable_spatial_uniqueness(spatial_units, reference_date)
  }

  applicable_geography <- geography[
    geography$valid_from <= reference_date &
      (is.na(geography$valid_to) | geography$valid_to >= reference_date),
    , drop = FALSE
  ]
  source_version <- if ("source_version" %in% names(data)) {
    data$source_version
  } else {
    rep(NA_character_, nrow(data))
  }
  .check_character(source_version, "source_version", contract, allow_na = TRUE)
  units <- unique(data.frame(
    source = data$source,
    source_version = source_version,
    source_geo_id = data$geo_id,
    source_geo_name = data$geo_name,
    source_geo_level = data$geo_level,
    stringsAsFactors = FALSE
  ))
  resolution <- .empty_resolution_audit()
  if (nrow(units)) {
    rows <- lapply(seq_len(nrow(units)), function(index) {
      .resolve_one_geography(
        units[index, , drop = FALSE], applicable_geography,
        canonical_type_col, reference_date, source, aliases, spatial_units
      )
    })
    resolution <- do.call(rbind, rows)
    rownames(resolution) <- NULL
    data_keys <- .resolution_unit_keys(
      data$source, source_version, data$geo_id, data$geo_name, data$geo_level
    )
    audit_keys <- .resolution_unit_keys(
      resolution$source, resolution$source_version,
      resolution$source_geo_id, resolution$source_geo_name,
      resolution$source_geo_level
    )
    resolution$input_rows <- tabulate(match(data_keys, audit_keys), nrow(resolution))
  }

  failures <- resolution$status %in% c("unresolved", "ambiguous")
  if (any(failures)) {
    details <- sprintf(
      "%s [%s]", resolution$source_geo_name[failures], resolution$status[failures]
    )
    .stop_contract(
      contract,
      sprintf("unresolved or ambiguous source unit(s): %s",
              paste(details, collapse = "; "))
    )
  }

  output <- data
  output$source_geo_id <- data$geo_id
  output$source_geo_name <- data$geo_name
  output$source_geo_level <- data$geo_level
  if (nrow(data)) {
    keys <- .resolution_unit_keys(
      data$source, source_version, data$geo_id, data$geo_name, data$geo_level
    )
    audit_keys <- .resolution_unit_keys(
      resolution$source, resolution$source_version,
      resolution$source_geo_id, resolution$source_geo_name,
      resolution$source_geo_level
    )
    matched <- match(keys, audit_keys)
    output$geo_id <- resolution$resolved_geo_id[matched]
    output$geo_name <- resolution$resolved_geo_name[matched]
    output$geo_level <- resolution$resolved_geo_level[matched]
  }
  .validate_surveillance_observations(output)

  methods <- sort(unique(resolution$resolution_method))
  methods <- methods[!is.na(methods)]
  targets <- sort(unique(
    resolution$target_geo_id[!is.na(resolution$target_geo_id)]
  ))
  diagnostics <- list(
    operation = "resolve_geography",
    source = source,
    reference_date = reference_date,
    input_rows = nrow(data),
    unique_source_units = nrow(resolution),
    exact_resolved_count = sum(
      resolution$resolution_method == "exact_typed", na.rm = TRUE
    ),
    alias_resolved_count = sum(
      resolution$resolution_method == "reviewed_alias", na.rm = TRUE
    ),
    spatial_relation_required_count = sum(
      resolution$status == "requires_spatial_relation", na.rm = TRUE
    ),
    unresolved_count = sum(resolution$status == "unresolved", na.rm = TRUE),
    ambiguous_count = sum(resolution$status == "ambiguous", na.rm = TRUE),
    unresolved_source_labels = resolution$source_geo_name[
      resolution$status == "unresolved"
    ],
    ambiguous_source_labels = resolution$source_geo_name[
      resolution$status == "ambiguous"
    ],
    canonical_target_ids_used = targets,
    resolution_methods_used = methods,
    source_geo_vintage_unresolved = anyNA(data$geo_vintage),
    output_geo_vintage_unchanged = identical(output$geo_vintage, data$geo_vintage)
  )
  list(data = output, resolution = resolution, diagnostics = diagnostics)
}

.resolve_one_geography <- function(
    unit, geography, type_col, reference_date, source, aliases, spatial_units) {
  parsed <- .parse_source_geography(unit$source_geo_name, source)
  base <- .resolution_audit_row(unit, parsed, reference_date)

  spatial <- .matching_spatial_units(
    spatial_units, unit, parsed$source_type, reference_date
  )
  if (nrow(spatial) > 1L) {
    base$status <- "ambiguous"
    return(base)
  }
  if (nrow(spatial) == 1L) {
    if (is.na(base$source_type)) {
      base$source_type <- spatial$source_type
    }
    base$status <- "requires_spatial_relation"
    base$resolution_method <- "reviewed_spatial_unit"
    base$resolved_geo_id <- spatial$source_geo_id
    base$resolved_geo_name <- spatial$source_geo_name
    base$resolved_geo_level <- spatial$source_geo_level
    base$recognized_geo_id <- spatial$source_geo_id
    base$recognized_geo_name <- spatial$source_geo_name
    base$recognized_geo_level <- spatial$source_geo_level
    base$directive_reason <- spatial$reason
    return(base)
  }

  if (is.na(parsed$source_type)) {
    alias <- .matching_aliases(aliases, unit, parsed$source_type, reference_date)
    if (nrow(alias) > 1L) {
      base$status <- "ambiguous"
      return(base)
    }
    if (!nrow(alias)) {
      base$status <- "unresolved"
      return(base)
    }
    base$source_type <- alias$source_type
    target <- geography[geography$geo_id == alias$target_geo_id, , drop = FALSE]
    if (nrow(target) != 1L) {
      base$status <- if (nrow(target)) "ambiguous" else "unresolved"
      return(base)
    }
    if ("target_vghid" %in% names(alias) && !is.na(alias$target_vghid) &&
        "vghid" %in% names(target) &&
        !identical(alias$target_vghid, target$vghid)) {
      base$status <- "ambiguous"
      return(base)
    }
    return(.fill_identity_resolution(
      base, target, "reviewed_alias", alias$reason
    ))
  }

  exact <- geography[
    geography$geo_name == parsed$candidate_name &
      geography[[type_col]] == parsed$candidate_type,
    , drop = FALSE
  ]
  if (nrow(exact) > 1L) {
    base$status <- "ambiguous"
    return(base)
  }
  if (nrow(exact) == 1L) {
    return(.fill_identity_resolution(base, exact, "exact_typed", NA_character_))
  }

  alias <- .matching_aliases(aliases, unit, parsed$source_type, reference_date)
  if (nrow(alias) > 1L) {
    base$status <- "ambiguous"
    return(base)
  }
  if (!nrow(alias)) {
    base$status <- "unresolved"
    return(base)
  }
  target <- geography[geography$geo_id == alias$target_geo_id, , drop = FALSE]
  if (nrow(target) != 1L) {
    base$status <- if (nrow(target)) "ambiguous" else "unresolved"
    return(base)
  }
  if ("target_vghid" %in% names(alias) && !is.na(alias$target_vghid) &&
      "vghid" %in% names(target) &&
      !identical(alias$target_vghid, target$vghid)) {
    base$status <- "ambiguous"
    return(base)
  }
  .fill_identity_resolution(base, target, "reviewed_alias", alias$reason)
}

.parse_source_geography <- function(label, source) {
  if (!identical(source, "SurvStat@RKI")) {
    return(list(source_type = NA_character_, candidate_name = NA_character_,
                candidate_type = NA_character_))
  }
  if (grepl("^LK .+", label)) {
    return(list(source_type = "LK", candidate_name = sub("^LK ", "", label),
                candidate_type = "Land"))
  }
  if (grepl("^SK .+", label)) {
    return(list(source_type = "SK", candidate_name = sub("^SK ", "", label),
                candidate_type = "Stadt"))
  }
  if (identical(label, "Region Hannover")) {
    return(list(source_type = "Region", candidate_name = label,
                candidate_type = "Regio"))
  }
  if (identical(label, "St\u00e4dteRegion Aachen")) {
    return(list(source_type = "St\u00e4dteRegion", candidate_name = label,
                candidate_type = "Regio"))
  }
  list(source_type = NA_character_, candidate_name = NA_character_,
       candidate_type = NA_character_)
}

.fill_identity_resolution <- function(row, target, method, reason) {
  row$status <- "resolved_identity"
  row$resolution_method <- method
  row$target_geo_id <- target$geo_id
  row$target_geo_name <- target$geo_name
  row$target_geo_level <- target$geo_level
  row$target_vghid <- if ("vghid" %in% names(target)) target$vghid else NA_character_
  row$resolved_geo_id <- target$geo_id
  row$resolved_geo_name <- target$geo_name
  row$resolved_geo_level <- target$geo_level
  row$directive_reason <- reason
  row
}

.matching_aliases <- function(x, unit, source_type, reference_date) {
  if (is.null(x) || !nrow(x)) {
    return(if (is.null(x)) data.frame() else x[0, , drop = FALSE])
  }
  type_match <- if (is.na(source_type)) rep(TRUE, nrow(x)) else x$source_type == source_type
  x[
    x$source == unit$source &
      .na_equal(x$source_version, unit$source_version) &
      x$source_label == unit$source_geo_name &
      type_match &
      x$valid_from <= reference_date &
      (is.na(x$valid_to) | x$valid_to >= reference_date),
    , drop = FALSE
  ]
}

.matching_spatial_units <- function(x, unit, source_type, reference_date) {
  if (is.null(x) || !nrow(x)) {
    return(if (is.null(x)) data.frame() else x[0, , drop = FALSE])
  }
  type_match <- if (is.na(source_type)) rep(TRUE, nrow(x)) else x$source_type == source_type
  x[
    x$source == unit$source &
      .na_equal(x$source_version, unit$source_version) &
      x$source_label == unit$source_geo_name &
      type_match &
      x$valid_from <= reference_date &
      (is.na(x$valid_to) | x$valid_to >= reference_date),
    , drop = FALSE
  ]
}

.na_equal <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

.validate_spatial_units <- function(x) {
  contract <- "spatial units"
  required <- c(
    "source", "source_version", "source_label", "source_type",
    "source_geo_id", "source_geo_name", "source_geo_level", "valid_from",
    "valid_to", "reason", "review_status"
  )
  .require_data_frame(x, contract)
  .require_columns(x, required, contract)
  if (anyDuplicated(names(x))) {
    .stop_contract(contract, "column names must be unique")
  }
  for (field in setdiff(required, c("source_version", "valid_from", "valid_to"))) {
    .check_character(x[[field]], field, contract)
  }
  .check_character(x$source_version, "source_version", contract, allow_na = TRUE)
  .check_date(x$valid_from, "valid_from", contract)
  .check_date(x$valid_to, "valid_to", contract, allow_na = TRUE)
  if (any(!is.na(x$valid_to) & x$valid_to < x$valid_from)) {
    .stop_contract(contract, "`valid_to` must not precede `valid_from`")
  }
  if (any(x$review_status != "reviewed")) {
    .stop_contract(contract, "`review_status` must be exactly \"reviewed\" in v0.1")
  }
  invisible(x)
}

.validate_applicable_spatial_uniqueness <- function(x, reference_date) {
  applicable <- x[
    x$valid_from <= reference_date &
      (is.na(x$valid_to) | x$valid_to >= reference_date),
    , drop = FALSE
  ]
  if (!nrow(applicable)) {
    return(invisible(TRUE))
  }
  version <- ifelse(
    is.na(applicable$source_version), "<NA>",
    paste0(nchar(applicable$source_version), ":", applicable$source_version)
  )
  keys <- paste(
    applicable$source, version, applicable$source_geo_id, sep = "|"
  )
  if (anyDuplicated(keys)) {
    .stop_contract(
      "spatial units",
      "applicable `source_geo_id` values must be unique within source/source-version context"
    )
  }
  invisible(TRUE)
}

.resolution_audit_row <- function(unit, parsed, reference_date) {
  data.frame(
    source = unit$source,
    source_version = unit$source_version,
    source_geo_id = unit$source_geo_id,
    source_geo_name = unit$source_geo_name,
    source_geo_level = unit$source_geo_level,
    source_type = parsed$source_type,
    candidate_name = parsed$candidate_name,
    candidate_type = parsed$candidate_type,
    status = NA_character_,
    resolution_method = NA_character_,
    target_geo_id = NA_character_,
    target_geo_name = NA_character_,
    target_geo_level = NA_character_,
    target_vghid = NA_character_,
    recognized_geo_id = NA_character_,
    recognized_geo_name = NA_character_,
    recognized_geo_level = NA_character_,
    resolved_geo_id = NA_character_,
    resolved_geo_name = NA_character_,
    resolved_geo_level = NA_character_,
    resolution_reference_date = reference_date,
    directive_reason = NA_character_,
    input_rows = 0L,
    stringsAsFactors = FALSE
  )
}

.empty_resolution_audit <- function() {
  template <- data.frame(
    source = character(), source_version = character(),
    source_geo_id = character(), source_geo_name = character(),
    source_geo_level = character(), source_type = character(),
    candidate_name = character(), candidate_type = character(),
    status = character(), resolution_method = character(),
    target_geo_id = character(), target_geo_name = character(),
    target_geo_level = character(), target_vghid = character(),
    recognized_geo_id = character(), recognized_geo_name = character(),
    recognized_geo_level = character(), resolved_geo_id = character(),
    resolved_geo_name = character(), resolved_geo_level = character(),
    resolution_reference_date = as.Date(character()),
    directive_reason = character(), input_rows = integer(),
    stringsAsFactors = FALSE
  )
  template
}

.resolution_unit_keys <- function(source, version, id, name, level) {
  encode <- function(x) ifelse(is.na(x), "<NA>", paste0(nchar(x), ":", x))
  paste(encode(source), encode(version), encode(id), encode(name), encode(level),
        sep = "|")
}

.check_resolution_scalar_date <- function(x, field, contract) {
  if (!inherits(x, "Date") || length(x) != 1L || is.na(x)) {
    .stop_contract(contract, sprintf("`%s` must be one complete Date", field))
  }
}

.check_resolution_scalar_character <- function(x, field, contract) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .stop_contract(contract, sprintf("`%s` must be one non-empty character value", field))
  }
}
