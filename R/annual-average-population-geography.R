.harmonize_reviewed_annual_average_population <- function(
    population, relations, target_geography) {
  contract <- "reviewed annual-average population geography harmonization"
  .require_result_parts(population, contract)
  validate_annual_average_population(population$data)
  needed <- c(
    "year", "from_geo_id", "to_geo_id", "relation_type", "source", "note"
  )
  .require_columns(relations, needed, contract)
  if (!is.data.frame(relations) || !nrow(relations) ||
      anyNA(relations[needed]) ||
      !all(relations$relation_type == "historical_merge") ||
      any(!grepl("^[0-9]{5}$", relations$from_geo_id)) ||
      any(!grepl("^[0-9]{5}$", relations$to_geo_id)) ||
      anyDuplicated(relations[c("year", "from_geo_id")])) {
    .stop_contract(contract, "relations must be unique reviewed historical merges")
  }
  if (!is.numeric(relations$year) || any(relations$year != floor(relations$year))) {
    .stop_contract(contract, "relation years must be whole numbers")
  }
  if (!is.data.frame(target_geography) ||
      !all(c("geo_id", "geo_name") %in% names(target_geography)) ||
      anyNA(target_geography[c("geo_id", "geo_name")]) ||
      anyDuplicated(target_geography$geo_id)) {
    .stop_contract(contract, "target geography must contain unique IDs and names")
  }

  data <- population$data
  source_totals <- stats::setNames(vapply(split(data, data$year), function(x) {
    sum(x$population)
  }, numeric(1L)), names(split(data, data$year)))
  relations <- relations[relations$year %in% unique(data$year), , drop = FALSE]
  relation_years <- sort(unique(as.integer(relations$year)))
  for (year in relation_years) {
    rules <- relations[relations$year == year, , drop = FALSE]
    year_rows <- which(data$year == year)
    observed <- data$geo_id[year_rows]
    expected_extra <- sort(unique(rules$from_geo_id))
    target_ids <- sort(target_geography$geo_id)
    if (!identical(sort(setdiff(observed, target_ids)), expected_extra) ||
        length(setdiff(target_ids, observed))) {
      .stop_contract(contract, paste(
        "source geography does not match the reviewed target plus relation",
        "sources for year", year
      ))
    }
    for (index in seq_len(nrow(rules))) {
      from <- which(data$year == year & data$geo_id == rules$from_geo_id[[index]])
      to <- which(data$year == year & data$geo_id == rules$to_geo_id[[index]])
      if (length(from) != 1L || length(to) != 1L) {
        .stop_contract(contract, "each reviewed merge must have one source and target row")
      }
      data$population[[to]] <- data$population[[to]] + data$population[[from]]
      target <- target_geography[
        target_geography$geo_id == rules$to_geo_id[[index]], , drop = FALSE
      ]
      data$geo_name[[to]] <- target$geo_name[[1L]]
      data <- data[-from, , drop = FALSE]
    }
  }

  target_ids <- sort(target_geography$geo_id)
  by_year <- split(data$geo_id, data$year)
  if (any(vapply(by_year, function(ids) !identical(sort(ids), target_ids), logical(1L)))) {
    .stop_contract(contract, "harmonized annual geography must equal the target geography")
  }
  target_names <- stats::setNames(target_geography$geo_name, target_geography$geo_id)
  data$geo_name <- unname(target_names[data$geo_id])
  rownames(data) <- NULL
  output_totals <- stats::setNames(vapply(split(data, data$year), function(x) {
    sum(x$population)
  }, numeric(1L)), names(split(data, data$year)))
  if (!identical(source_totals, output_totals)) {
    .stop_contract(contract, "additive population mass was not preserved")
  }
  population$data <- data
  population$diagnostics$geography_harmonization <- list(
    method = "reviewed_additive_historical_merge",
    relation_count = nrow(relations),
    relation_years = relation_years,
    target_unit_count = length(target_ids),
    additive_mass_preserved = TRUE
  )
  for (name in names(population$provenance)) {
    population$provenance[[name]]$geography_harmonization <- list(
      method = "reviewed_additive_historical_merge",
      relations = relations,
      applied_before_incidence = TRUE,
      target_geography = "reviewed current 400-district analysis geography",
      additive_mass_preserved = TRUE
    )
  }
  validate_annual_average_population(population$data)
  population
}
