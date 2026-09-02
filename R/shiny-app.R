.shiny_demographic_periods <- function() {
  stats::setNames(
    list(2022:2024, 2017:2020),
    c("2022\u20132024", "2017\u20132020")
  )
}

.shiny_period_resources <- function(pathogen) {
  if (identical(pathogen, "Influenza, saisonal")) {
    stats::setNames(list(rki_influenza_periods_2017_2026()), "RKI-gepr\u00fcfte Influenzawellen")
  } else if (identical(pathogen, "COVID-19")) {
    stats::setNames(
      list(dissertation_covid_welle2_periods(), rki_covid_activity_waves()),
      c("Dissertation / RKI-Pandemieperioden", "RKI-gepr\u00fcfte Aktivit\u00e4tswellen")
    )
  } else {
    stop("Unsupported Shiny PoC pathogen.", call. = FALSE)
  }
}

.shiny_period_resource <- function(pathogen) .shiny_period_resources(pathogen)[[1L]]

.shiny_period_choices <- function(pathogen) {
  lapply(.shiny_period_resources(pathogen), function(resource) {
    stats::setNames(resource$periods$period_id, resource$periods$label)
  })
}

.shiny_selected_period <- function(pathogen, period_id) {
  matches <- lapply(.shiny_period_resources(pathogen), function(resource) {
    row <- resource$periods[resource$periods$period_id == period_id, , drop = FALSE]
    if (nrow(row)) list(resource = resource, row = row) else NULL
  })
  matches <- Filter(Negate(is.null), matches)
  if (length(matches) != 1L) stop("Unknown reviewed epidemiological period.", call. = FALSE)
  matches[[1L]]
}

.shiny_demographic_cache_key <- function(period_label, source_mode = "snapshot") {
  years <- .shiny_demographic_periods()[[period_label]]
  if (is.null(years)) stop("Unsupported demographic reference period.", call. = FALSE)
  if (!source_mode %in% c("snapshot", "live")) {
    stop("Unsupported demographic source mode.", call. = FALSE)
  }
  paste0("demography:", source_mode, ":", paste(years, collapse = "-"))
}

.shiny_surveillance_cache_key <- function(pathogen, reporting_years) {
  paste("survstat-incidence", pathogen,
        paste(sort(unique(reporting_years)), collapse = "-"), sep = ":")
}

.shiny_typology_cache_key <- function(summary, k, mode = "dynamic") {
  data <- summary$data
  if (identical(mode, "dissertation")) {
    return("typology:dissertation_v1:historical_reference")
  }
  paste(
    "typology", format(unique(data$period_start)), format(unique(data$period_end)),
    demographic_structure_spec()$indicator_set_id,
    dynamic_kmeans_spec()$fitting_specification_id, as.integer(k), sep = ":"
  )
}

.shiny_summary_cache_key <- function(surveillance_key, period_id, fit_id) {
  paste("incidence-summary", surveillance_key, period_id, fit_id, sep = ":")
}

.shiny_surveillance_cache_match <- function(index, pathogen, reporting_years) {
  if (!length(index)) return(NULL)
  required <- sort(unique(as.integer(reporting_years)))
  compatible <- vapply(index, function(entry) {
    identical(entry$pathogen, pathogen) &&
      all(required %in% entry$reporting_years)
  }, logical(1L))
  if (!any(compatible)) return(NULL)
  candidates <- index[compatible]
  sizes <- vapply(candidates, function(entry) length(entry$reporting_years),
                  integer(1L))
  candidates[[which.min(sizes)]]$key
}

.shiny_app_cluster_colours <- function(ids, mode = "dynamic") {
  if (identical(mode, "dissertation")) {
    historical <- c(ClD = "#bc5e21", ClJ = "#748c61", ClA = "#274f66")
    if (any(!unique(ids) %in% names(historical))) {
      stop("Unknown dissertation cluster identity.", call. = FALSE)
    }
    return(historical[unique(ids)])
  }
  palette <- c("#3B6FB6", "#D17C28", "#3F8F6B", "#8A64A8", "#B64E5A")
  levels <- sort(unique(ids))
  stats::setNames(palette[seq_along(levels)], levels)
}

.shiny_app_map_geojson <- function(map, assignments, mode = "dynamic") {
  colours <- .shiny_app_cluster_colours(assignments$display_cluster_id, mode)
  position <- match(map$features$geo_id, assignments$geo_id)
  features <- lapply(seq_len(nrow(map$features)), function(index) {
    cluster <- assignments$display_cluster_id[position[[index]]]
    list(
      type = "Feature", id = map$features$geo_id[[index]],
      properties = list(
        geo_id = map$features$geo_id[[index]],
        geo_name = map$features$geo_name[[index]], cluster = cluster,
        fillColor = unname(colours[[cluster]])
      ),
      geometry = map$features$geometry[[index]]
    )
  })
  jsonlite::toJSON(
    list(type = "FeatureCollection", features = features),
    auto_unbox = TRUE, null = "null", digits = 10
  )
}

.shiny_app_map_bounds <- function(map) {
  coordinates <- unlist(lapply(map$features$geometry, `[[`, "coordinates"),
                        recursive = TRUE, use.names = FALSE)
  if (!length(coordinates) || length(coordinates) %% 2L != 0L ||
      !is.numeric(coordinates) || any(!is.finite(coordinates))) {
    stop("Map geometry does not provide finite coordinate pairs.", call. = FALSE)
  }
  positions <- matrix(coordinates, ncol = 2L, byrow = TRUE)
  c(lng1 = min(positions[, 1L]), lat1 = min(positions[, 2L]),
    lng2 = max(positions[, 1L]), lat2 = max(positions[, 2L]))
}

.shiny_app_leaflet_geojson <- function(geojson, bounds, assignments = NULL,
                                       mode = "dynamic") {
  widget <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 4))
  widget <- leaflet::addGeoJSON(widget, geojson)
  display <- if (is.null(assignments)) NULL else list(
    clusters = stats::setNames(as.list(assignments$display_cluster_id),
                               assignments$geo_id),
    colours = as.list(.shiny_app_cluster_colours(
      assignments$display_cluster_id, mode))
  )
  widget <- htmlwidgets::onRender(widget, "
    function(el, x, display) {
      var map = this;
      map.eachLayer(function(layer) {
        if (!layer.feature || !layer.feature.properties) return;
        var p = layer.feature.properties;
        if (display && display.clusters) {
          p.cluster = display.clusters[p.geo_id];
          p.fillColor = display.colours[p.cluster];
        }
        layer.setStyle({color:'#ffffff', weight:0.7, fillColor:p.fillColor,
                        fillOpacity:0.82});
        layer.bindTooltip('<strong>' + p.geo_name + '</strong><br>' +
                          p.geo_id + ' \\u00b7 ' + p.cluster);
        layer.on('click', function() {
          Shiny.setInputValue('selected_geo_id', p.geo_id,
                              {priority:'event'});
        });
      });
    }", data = display)
  leaflet::fitBounds(widget, bounds[["lng1"]], bounds[["lat1"]],
                     bounds[["lng2"]], bounds[["lat2"]])
}

.shiny_app_leaflet_map <- function(map, assignments, mode = "dynamic") {
  .shiny_app_leaflet_geojson(
    if (!is.null(map$browser_geojson)) map$browser_geojson else
      .shiny_app_map_geojson(map, assignments, mode),
    .shiny_app_map_bounds(map), assignments, mode
  )
}

.shiny_typology_config <- function(mode) {
  if (identical(mode, "dissertation")) return(list(
    mode = mode, label = "Dissertation-Referenz", years = 2017:2020,
    k = 3L, note = "Historische Referenzreproduktion der Dissertation (2017\u20132020)"
  ))
  if (identical(mode, "dynamic")) return(list(
    mode = mode, label = "Dynamische demographische Typologie",
    years = NULL, k = NULL, note = "Fit-lokale neutrale Clusteridentit\u00e4ten"
  ))
  stop("Unsupported typology mode.", call. = FALSE)
}

.shiny_dissertation_fit <- function(summary) {
  frozen <- fit_dissertation_typology(summary, label_mapping = "historical_reference")
  assignments <- data.frame(
    fit_id = "dissertation_v1_historical_reference",
    geo_id = frozen$data$geo_id, raw_cluster = frozen$data$raw_cluster,
    display_cluster_id = frozen$data$cluster_code,
    cluster_label = frozen$data$cluster_label, stringsAsFactors = FALSE
  )
  joined <- merge(summary$data,
    assignments[c("geo_id", "raw_cluster", "display_cluster_id")],
    by = "geo_id", sort = FALSE
  )
  profile_groups <- split(seq_len(nrow(joined)), paste(
    joined$display_cluster_id, joined$indicator_id, sep = "\r"
  ))
  profiles <- do.call(rbind, lapply(profile_groups, function(i) {
    x <- joined[i, , drop = FALSE]
    indicator_position <- match(x$indicator_id[[1L]], frozen$matrix$indicator_order)
    center <- frozen$diagnostics$centers[
      as.character(x$raw_cluster[[1L]]), indicator_position
    ]
    data.frame(
      fit_id = "dissertation_v1_historical_reference",
      display_cluster_id = x$display_cluster_id[[1L]],
      raw_cluster = x$raw_cluster[[1L]], cluster_size = length(unique(x$geo_id)),
      cluster_proportion = length(unique(x$geo_id)) / nrow(assignments),
      indicator_id = x$indicator_id[[1L]],
      definition_version = x$definition_version[[1L]],
      indicator_unit = x$indicator_unit[[1L]],
      original_mean = mean(x$indicator_value),
      original_median = stats::median(x$indicator_value),
      standardized_center = unname(center), stringsAsFactors = FALSE
    )
  }))
  labels <- unique(assignments[c("display_cluster_id", "cluster_label")])
  diagnostics <- unique(profiles[c(
    "display_cluster_id", "raw_cluster", "cluster_size", "cluster_proportion"
  )])
  diagnostics <- merge(diagnostics, labels, by = "display_cluster_id", sort = FALSE)
  names(diagnostics)[names(diagnostics) == "cluster_size"] <- "size"
  frozen$assignments <- assignments
  frozen$profiles <- profiles
  frozen$cluster_diagnostics <- diagnostics
  frozen$provenance$fit_id <- "dissertation_v1_historical_reference"
  frozen$indicator_set <- list(
    indicator_set_id = "dissertation_v1", definition_version = "dissertation_v1"
  )
  frozen$fitting_specification <- frozen$specification
  frozen$mode <- "dissertation"
  frozen
}

.shiny_fit_typology <- function(summary, mode = "dynamic", k = 3L) {
  if (identical(mode, "dissertation")) return(.shiny_dissertation_fit(summary))
  fit <- fit_dynamic_typology(summary, k = k)
  fit$mode <- "dynamic"
  fit
}

.shiny_app_indicator_display <- function() {
  data.frame(
    indicator_id = c("population_density", "mean_age",
                     "youth_dependency_ratio"),
    label = c("Bev\u00f6lkerungsdichte", "Durchschnittsalter",
              "Jugendquotient"),
    unit = c("Einwohner je km\u00b2", "Jahre",
             "Unter-20-J\u00e4hrige je 100 Personen im Alter 20\u201364"),
    stringsAsFactors = FALSE
  )
}

.shiny_app_source_status <- function(source) {
  statuses <- unlist(lapply(source, function(x) {
    candidates <- c(x$diagnostics$data_status, x$provenance$data_status)
    candidates <- candidates[!vapply(candidates, is.null, logical(1L))]
    vapply(candidates, as.character, character(1L))
  }), use.names = FALSE)
  unique(statuses)
}

.shiny_app_query_status <- function(queries) {
  vapply(queries, function(query) as.character(query$data_status), character(1L))
}

.shiny_app_format_error <- function(error, source) {
  message <- conditionMessage(error)
  if (grepl("Regionaldatenbank", message, fixed = TRUE) &&
      grepl("authentication|required environment variables|missing or empty",
            message, ignore.case = TRUE)) {
    return(paste(
      "F\u00fcr die Live-Abfrage der Regionaldatenbank fehlen die Zugangsdaten.",
      "Bitte konfigurieren Sie die Zugangsdaten und laden Sie die Analyse erneut."
    ))
  }
  if (grepl("Regionaldatenbank", message, fixed = TRUE)) {
    return(paste(
      "Die demographischen Daten konnten nicht geladen werden.",
      "Bitte Zugang und Dienstverf\u00fcgbarkeit pr\u00fcfen und erneut versuchen."
    ))
  }
  if (grepl("SurvStat", message, fixed = TRUE)) {
    return(paste(
      "Die SurvStat-Daten konnten nicht geladen werden.",
      "Bitte Dienstverf\u00fcgbarkeit pr\u00fcfen und erneut versuchen."
    ))
  }
  paste0(source, " konnte nicht geladen werden. Bitte erneut versuchen.")
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
                  mean_age = mean_age, youth_dependency = youth),
    source_mode = "live", snapshot_provenance = NULL
  )
}

.shiny_fetch_snapshot_demography <- function(years) {
  snapshot <- regionalepi_demographic_snapshot()
  .demographic_snapshot_period(snapshot, years)
}

.shiny_demography_status <- function(demographic) {
  if (identical(demographic$source_mode, "snapshot")) {
    return(paste0(
      "Gepr\u00fcfter Snapshot \u2013 Datenstand ",
      demographic$snapshot_provenance$source_status_display_date
    ))
  }
  statuses <- .shiny_app_source_status(demographic$source)
  paste0("Live-Abruf \u2013 Datenstand ", paste(statuses, collapse = ", "))
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
  compatibility <- if (identical(fit$mode, "dissertation")) {
    dissertation_surveillance_compatibility()
  } else .shiny_dynamic_compatibility(fit, period_data$geo_id)
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
  district_period <- summarize_period_incidence_by_district(attached$data)
  list(summary = summary, assigned = assigned, attached = attached,
       district_period = district_period, compatibility = compatibility)
}

.shiny_app_dir <- function() {
  system.file("shiny", "regionalepi", package = "regionalepi")
}

.shiny_missing_dependencies <- function(checker = requireNamespace) {
  packages <- c("shiny", "leaflet", "ggplot2")
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
