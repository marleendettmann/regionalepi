.shiny_demographic_periods <- function() {
  stats::setNames(
    list(2022:2024, 2017:2020),
    c("2022\u20132024", "2017\u20132020")
  )
}

.shiny_period_resource <- function(pathogen) {
  if (identical(pathogen, "Influenza, saisonal")) {
    rki_influenza_periods_2017_2026()
  } else if (identical(pathogen, "COVID-19")) {
    dissertation_covid_welle2_periods()
  } else {
    stop("Unsupported Shiny PoC pathogen.", call. = FALSE)
  }
}

.shiny_period_choices <- function(pathogen) {
  resource <- .shiny_period_resource(pathogen)
  periods <- resource$periods
  labels <- periods$label
  duplicated_season <- !is.na(periods$season_id) &
    duplicated(periods$season_id) | (!is.na(periods$season_id) &
    duplicated(periods$season_id, fromLast = TRUE))
  if (any(duplicated_season)) {
    wave <- stats::ave(seq_len(nrow(periods)), periods$season_id, FUN = seq_along)
    labels[duplicated_season] <- paste0(labels[duplicated_season], " \u2013 Welle ",
                                        wave[duplicated_season])
  }
  stats::setNames(periods$period_id, labels)
}

.shiny_selected_period <- function(pathogen, period_id) {
  resource <- .shiny_period_resource(pathogen)
  row <- resource$periods[resource$periods$period_id == period_id, , drop = FALSE]
  if (nrow(row) != 1L) stop("Unknown reviewed epidemiological period.", call. = FALSE)
  list(resource = resource, row = row)
}

.shiny_demographic_cache_key <- function(period_label) {
  years <- .shiny_demographic_periods()[[period_label]]
  if (is.null(years)) stop("Unsupported demographic reference period.", call. = FALSE)
  paste0("demography:", paste(years, collapse = "-"))
}

.shiny_surveillance_cache_key <- function(pathogen, reporting_years) {
  paste("survstat-incidence", pathogen,
        paste(sort(unique(reporting_years)), collapse = "-"), sep = ":")
}

.shiny_typology_cache_key <- function(summary, k) {
  data <- summary$data
  paste(
    "typology", format(unique(data$period_start)), format(unique(data$period_end)),
    demographic_structure_spec()$indicator_set_id,
    dynamic_kmeans_spec()$fitting_specification_id, as.integer(k), sep = ":"
  )
}

.shiny_summary_cache_key <- function(surveillance_key, period_id, fit_id) {
  paste("incidence-summary", surveillance_key, period_id, fit_id, sep = ":")
}

.shiny_reference_years <- function(period_row) {
  start <- as.integer(format(period_row$start_date, "%Y"))
  end <- as.integer(format(period_row$end_date, "%Y"))
  seq.int(start, end)
}

.shiny_dynamic_compatibility <- function(fit, surveillance_ids) {
  typology_ids <- sort(unique(fit$assignments$geo_id))
  surveillance_ids <- sort(unique(surveillance_ids))
  list(
    compatibility_id = paste0("shiny_dynamic_", fit$provenance$fit_id),
    typology_id = fit$provenance$fit_id,
    typology_definition_version = fit$indicator_set$definition_version,
    surveillance_scope_id = "reviewed_survstat_400",
    expected_typology_only_geo_ids = sort(setdiff(typology_ids, surveillance_ids)),
    expected_surveillance_only_geo_ids = sort(setdiff(surveillance_ids, typology_ids)),
    expected_typology_count = length(typology_ids),
    expected_surveillance_count = length(surveillance_ids),
    review_status = "reviewed",
    reason = if (identical(setdiff(typology_ids, surveillance_ids), "16056")) {
      "Reviewed historical 401-to-current 400 compatibility; Eisenach remains fit provenance only"
    } else {
      "Exact canonical geography compatibility for the Shiny PoC"
    }
  )
}

.shiny_map_assignments <- function(map, fit) {
  features <- map$features
  assignments <- fit$assignments
  position <- match(features$geo_id, assignments$geo_id)
  missing <- features$geo_id[is.na(position)]
  extra <- setdiff(assignments$geo_id, features$geo_id)
  if (length(missing)) stop("Typology does not cover every reviewed map ID.", call. = FALSE)
  data.frame(
    geo_id = features$geo_id,
    geo_name = features$geo_name,
    display_cluster_id = assignments$display_cluster_id[position],
    stringsAsFactors = FALSE
  ) -> joined
  list(data = joined, map_only_geo_ids = missing, typology_only_geo_ids = extra)
}

.shiny_fetch_demography <- function(years) {
  dates <- as.Date(sprintf("%d-12-31", years))
  fetched <- tryCatch(list(
    population = fetch_regional_population(dates),
    area = fetch_regional_area(dates),
    mean_age = fetch_regional_mean_age(dates),
    youth = fetch_regional_youth_dependency(dates)
  ), error = function(error) {
    stop("Regionaldatenbank retrieval failed: ", conditionMessage(error),
         call. = FALSE)
  })
  population <- fetched$population
  area <- fetched$area
  mean_age <- fetched$mean_age
  youth <- fetched$youth
  density <- derive_population_density(population, area)
  annual <- .combine_demographic_indicators(mean_age$data, youth$data, density$data)
  summary <- summarize_indicator_period(annual, years)
  list(
    annual = annual, summary = summary,
    source = list(population = population, area = area,
                  mean_age = mean_age, youth_dependency = youth)
  )
}

.shiny_resolve_survstat <- function(result, resources, aliases,
                                    spatial_units = NULL) {
  resolved <- resolve_geography(
    result$data, resources$bkg_districts, as.Date("2024-12-31"),
    "SurvStat@RKI", "canonical_type", aliases, spatial_units
  )
  list(data = resolved$data, resolution = resolved$resolution,
       diagnostics = resolved$diagnostics, provenance = result$provenance)
}

.shiny_fetch_surveillance <- function(pathogen, years, resources) {
  token <- .stable_text_hash(c(pathogen, years))
  fetched <- tryCatch(list(
    base = fetch_survstat_incidence(
      pathogen, years, "kreis", query_id = paste0("shiny-kreis-", token)
    ),
    berlin = fetch_survstat_incidence(
      pathogen, years, "bundesland", geography_filter = "Berlin",
      query_id = paste0("shiny-berlin-", token)
    )
  ), error = function(error) {
    stop("SurvStat@RKI retrieval failed: ", conditionMessage(error),
         call. = FALSE)
  })
  base <- fetched$base
  berlin <- fetched$berlin
  base <- .shiny_resolve_survstat(
    base, resources, resources$survstat_aliases,
    resources$survstat_spatial_units
  )
  berlin <- .shiny_resolve_survstat(
    berlin, resources, resources$survstat_incidence_aliases
  )
  assembled <- assemble_surveillance_incidence(
    base$data, berlin$data, base$provenance, berlin$provenance,
    resources$survstat_incidence_assembly_spec
  )
  assembled$resolution <- list(kreis = base$resolution, berlin = berlin$resolution)
  assembled
}

.shiny_analyse_period <- function(surveillance, selected_period, fit) {
  assigned <- assign_epidemiological_periods(
    surveillance$data, selected_period$resource
  )
  selected_rows <- !is.na(assigned$data$period_id) &
    assigned$data$period_id == selected_period$row$period_id
  period_data <- assigned$data[selected_rows, , drop = FALSE]
  compatibility <- .shiny_dynamic_compatibility(fit, period_data$geo_id)
  typology <- list(
    data = data.frame(
      geo_id = fit$assignments$geo_id,
      cluster_id = fit$assignments$display_cluster_id,
      stringsAsFactors = FALSE
    ),
    provenance = fit$provenance
  )
  attached <- attach_typology(period_data, typology, compatibility)
  summary <- summarize_incidence_by_typology(attached$data)
  list(summary = summary, assigned = assigned, attached = attached,
       compatibility = compatibility)
}

.shiny_app_dir <- function() {
  system.file("shiny", "regionalepi", package = "regionalepi")
}

.shiny_missing_dependencies <- function(checker = requireNamespace) {
  packages <- c("shiny", "leaflet")
  packages[!vapply(packages, checker, logical(1L), quietly = TRUE)]
}

#' Run the regionalepi Shiny proof of concept
#'
#' Launches the installed, deliberately narrow interactive demonstration. Live
#' demographic queries require the Regionaldatenbank credential environment
#' variables; SurvStat incidence is fetched from the official live service.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return The value returned by `shiny::runApp()`, invisibly.
#' @export
run_regionalepi_app <- function(...) {
  missing <- .shiny_missing_dependencies()
  if (length(missing)) {
    stop(
      "The regionalepi Shiny PoC requires optional package(s): ",
      paste(missing, collapse = ", "), ". Install them before launching.",
      call. = FALSE
    )
  }
  app_dir <- .shiny_app_dir()
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("The installed regionalepi Shiny application could not be found.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
