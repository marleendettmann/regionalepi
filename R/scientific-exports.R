.regionalepi_package_citation <- function() {
  paste0(
    "Dettmann, M. (2026). regionalepi: Regionale Infektionssurveillance und ",
    "demografische Typologien (Version ",
    as.character(utils::packageVersion("regionalepi")), "). R-Paket."
  )
}

.regionalepi_dissertation_citation <- function() {
  paste0(
    "Dettmann, M. (2026). Einfluss demografischer Faktoren auf die ",
    "Ausbreitung von Infektionskrankheiten am Beispiel von Influenza und ",
    "COVID-19. Freie Universit\u00e4t Berlin. DOI: 10.17169/refubium-51449."
  )
}

.export_safe_character <- function(x) {
  risky <- !is.na(x) & grepl("^[=+@-]", x)
  x[risky] <- paste0("'", x[risky])
  x
}

.export_table <- function(x, contract = "scientific export") {
  if (!is.data.frame(x)) .stop_contract(contract, "result sheet must be a data frame")
  if (!nrow(x) || !ncol(x) || anyDuplicated(names(x))) {
    .stop_contract(contract, "result sheet must be non-empty with unique columns")
  }
  for (name in names(x)) {
    if (is.factor(x[[name]])) x[[name]] <- as.character(x[[name]])
    if (is.character(x[[name]])) x[[name]] <- .export_safe_character(x[[name]])
    if (is.list(x[[name]])) .stop_contract(contract, "list columns are not exportable")
  }
  if ("geo_id" %in% names(x) &&
      (anyNA(x$geo_id) || any(!grepl("^[0-9]{5}$", x$geo_id)))) {
    .stop_contract(contract, "geo_id must remain a complete five-character string")
  }
  x
}

.export_metadata <- function(values) {
  keep <- !vapply(values, is.null, logical(1L))
  values <- values[keep]
  data.frame(
    field = names(values),
    value = vapply(values, function(x) paste(as.character(x), collapse = "; "),
      character(1L)),
    stringsAsFactors = FALSE
  )
}

.export_methodology <- function(lines) data.frame(
  section = names(lines), text = unname(lines), stringsAsFactors = FALSE)

.snapshot_export_observation_provenance <- function(snapshot) {
  segments <- snapshot$component_observation_provenance
  if (is.null(segments)) return(list(statuses = NULL, retrieved_at = NULL))
  flatten <- function(field) unlist(lapply(names(segments), function(component) {
    vapply(segments[[component]], function(segment) paste0(
      component, "[", paste(segment$periods, collapse = ","), "]=",
      as.character(segment[[field]])), character(1L))
  }), use.names = FALSE)
  list(statuses = flatten("data_status"), retrieved_at = flatten("retrieved_at"))
}

.export_sources <- function(include_rki = FALSE) {
  rows <- data.frame(
    source = c("regionalepi R-Paket", "Wissenschaftliche Grundlage",
      "Regionaldatenbank Deutschland",
      "Bundesamt f\u00fcr Kartographie und Geod\u00e4sie (BKG)"),
    attribution = c(
      .regionalepi_package_citation(),
      .regionalepi_dissertation_citation(),
      "Statistische \u00c4mter des Bundes und der L\u00e4nder",
      "Geobasis-DE / BKG"
    ),
    licence = c("GPL-3 package code; source data retain separate licences",
      "Separate scientific publication; repository terms apply",
      "Datenlizenz Deutschland \u2013 Namensnennung \u2013 Version 2.0",
      "CC BY 4.0 for the reviewed source resource"),
    stringsAsFactors = FALSE
  )
  if (isTRUE(include_rki)) rows <- rbind(rows, data.frame(
    source = "SurvStat@RKI", attribution = "Robert Koch-Institut",
    licence = "RKI source and data-use conditions apply",
    stringsAsFactors = FALSE))
  rows
}

.write_scientific_workbook <- function(sheets, path) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Scientific Excel export requires package 'writexl'.", call. = FALSE)
  }
  if (!is.list(sheets) || !length(sheets) || is.null(names(sheets)) ||
      any(!nzchar(names(sheets))) || anyDuplicated(names(sheets))) {
    stop("Scientific workbook requires uniquely named sheets.", call. = FALSE)
  }
  sheets <- lapply(sheets, .export_table)
  writexl::write_xlsx(sheets, path = path)
  invisible(normalizePath(path, mustWork = TRUE))
}

.write_scientific_csv <- function(data, path) {
  data <- .export_table(data)
  utils::write.csv(data, file = path, row.names = FALSE, na = "NA",
    fileEncoding = "UTF-8")
  invisible(normalizePath(path, mustWork = TRUE))
}

.typology_export_tables <- function(prepared, map_provenance,
    exported_at = Sys.time()) {
  fit <- prepared$fit
  assignment <- prepared$map_join$data[, c("geo_id", "geo_name"), drop = FALSE]
  matched <- fit$assignments[match(assignment$geo_id, fit$assignments$geo_id), ]
  profile <- rep(NA_character_, nrow(assignment))
  if (identical(fit$diagnostics$k, 3L)) {
    alignment <- .dynamic_profile_alignment(fit)
    profile <- alignment$profile_class[
      match(matched$display_cluster_id, alignment$dynamic_cluster_id)]
  }
  assignment$demographic_reference_period <- paste(
    range(prepared$demographic_years), collapse = "-")
  assignment$dynamic_cluster_id <- matched$display_cluster_id
  assignment$aligned_demographic_profile <- profile
  assignment$k <- fit$diagnostics$k
  assignment$fit_id <- fit$provenance$fit_id
  snapshot <- prepared$demographic$snapshot_provenance
  observation_provenance <- .snapshot_export_observation_provenance(snapshot)
  reference <- prepared$reference_selection
  if (is.null(reference)) reference <- list(
    selection_type = "preset", configuration_type = "preset",
    solution_review_status = "reviewed_reference", exploratory = FALSE,
    geography_regime = if (max(prepared$demographic_years) <= 2020L)
      "historical_401_districts" else "current_400_districts",
    census_bases = unique(ifelse(prepared$demographic_years <= 2021L,
      "census_2011", "census_2022")))
  configuration_type <- reference$configuration_type %||%
    reference$selection_type
  solution_review_status <- reference$solution_review_status %||%
    if (identical(configuration_type, "preset"))
      "reviewed_reference" else "not_individually_reviewed"
  indicator_specification <- paste(fit$provenance$indicator_set_id,
    fit$provenance$indicator_set_version, sep = "@")
  fitting_specification <- paste(fit$provenance$fitting_specification_id,
    fit$provenance$fitting_specification_version, sep = "@")
  assignment$snapshot_id <- snapshot$snapshot_id
  assignment$reference_selection <- reference$selection_type
  assignment$configuration_type <- configuration_type
  assignment$solution_review_status <- solution_review_status
  assignment$exploratory <- reference$exploratory
  assignment$geography_regime <- reference$geography_regime
  assignment$census_bases <- paste(reference$census_bases, collapse = ";")
  assignment$indicator_specification <- indicator_specification
  assignment$fitting_specification <- fitting_specification
  metadata <- .export_metadata(list(
    package_version = as.character(utils::packageVersion("regionalepi")),
    exported_at = format(exported_at, tz = "UTC", usetz = TRUE),
    snapshot_id = snapshot$snapshot_id,
    demographic_reference_years = prepared$demographic_years,
    reference_selection = reference$selection_type,
    configuration_type = configuration_type,
    solution_review_status = solution_review_status,
    configuration_label = if (identical(configuration_type, "custom"))
      "Benutzerdefinierte Konfiguration" else
        "Gepr\u00fcfte Referenzkonfiguration",
    solution_review_note = if (identical(solution_review_status,
      "not_individually_reviewed")) paste(
        "Die Typologie wird f\u00fcr den gew\u00e4hlten Zeitraum anhand gepr\u00fcfter",
        "demografischer Indikatoren und Berechnungsmethoden neu erstellt.",
        "Die konkrete Clusterl\u00f6sung wurde nicht gesondert wissenschaftlich validiert."
      ) else "Gepr\u00fcfte Referenzkonfiguration.",
    exploratory = reference$exploratory,
    geography_regime = reference$geography_regime,
    census_bases = reference$census_bases,
    selected_indicators = fit$matrix$indicator_order,
    indicator_set = indicator_specification,
    typology_specification = fitting_specification,
    k = fit$diagnostics$k, algorithm = fit$fitting_specification$algorithm,
    nstart = fit$fitting_specification$nstart,
    iter_max = fit$fitting_specification$iter_max,
    seed = fit$fitting_specification$seed,
    fit_id = fit$provenance$fit_id,
    geography_reference = as.character(map_provenance$source_vintage),
    source_data_status = snapshot$source_data_status,
    observation_source_status = observation_provenance$statuses,
    observation_retrieved_at = observation_provenance$retrieved_at
  ))
  list(
    `Kreiszuordnungen` = assignment,
    `Clusterprofile` = fit$profiles,
    Metadaten = metadata,
    Methodik = .export_methodology(c(
      Indikatoren = "Bev\u00f6lkerungsdichte, Durchschnittsalter und Jugendquotient werden nach ihren versionierten Definitionen verwendet.",
      Standardisierung = "Die Indikatoren werden bundesweit \u00fcber alle einbezogenen Kreise z-standardisiert.",
      Clusterverfahren = "Die dynamische Typologie verwendet die dokumentierte deterministische k-Means-Spezifikation.",
      Interpretation = "Dynamische Cluster-IDs sind fit-lokal und explorativ; sie sind keine zeitstabilen Identit\u00e4ten oder kausalen Kategorien."
    )),
    `Quellen und Lizenz` = .export_sources(FALSE)
  )
}

.transition_export_tables <- function(transition, map, snapshot_id,
    exported_at = Sys.time()) {
  assignments <- transition$assignments
  assignments$geo_name <- map$features$geo_name[
    match(assignments$geo_id, map$features$geo_id)]
  assignments$comparison_id <- transition$provenance$comparison_id
  assignments$snapshot_id <- snapshot_id
  assignments$from_reference_years <- paste(
    transition$provenance$from_reference_period, collapse = ";")
  assignments$to_reference_years <- paste(
    transition$provenance$to_reference_period, collapse = ";")
  assignments$reference_selection <- "fixed_reviewed_comparison"
  assignments$configuration_type <- "preset"
  assignments$solution_review_status <- "reviewed_reference"
  assignments$exploratory <- FALSE
  assignments$geography_regime <- "current_400_district_comparison"
  assignments$census_bases <- "census_2011;census_2022"
  assignments$indicator_specification <- paste(
    demographic_structure_spec()$indicator_set_id,
    demographic_structure_spec()$definition_version, sep = "@")
  assignments$fitting_specification <- paste(
    dynamic_kmeans_spec()$fitting_specification_id,
    dynamic_kmeans_spec()$definition_version, sep = "@")
  assignments <- assignments[, c("geo_id", "geo_name",
    "from_dynamic_cluster_id", "to_dynamic_cluster_id",
    "from_profile_class", "to_profile_class", "changed", "from_fit_id",
    "to_fit_id", "comparison_id", "snapshot_id", "from_reference_years",
    "to_reference_years", "reference_selection", "configuration_type",
    "solution_review_status", "exploratory",
    "geography_regime", "census_bases", "indicator_specification",
    "fitting_specification"), drop = FALSE]
  matrix_table <- function(x) data.frame(from_profile = rownames(x),
    as.data.frame.matrix(unclass(x)), check.names = FALSE)
  changes <- stats::reshape(transition$indicator_changes,
    idvar = "geo_id", timevar = "indicator_id", direction = "wide")
  changes$geo_name <- map$features$geo_name[match(changes$geo_id,
    map$features$geo_id)]
  changes <- changes[, c("geo_id", "geo_name",
    setdiff(names(changes), c("geo_id", "geo_name"))), drop = FALSE]
  sheets <- list(
    Clusterwechsler = assignments,
    transition_counts = matrix_table(transition$count_matrix),
    transition_percentages = matrix_table(transition$row_percentage_matrix),
    indicator_changes = changes,
    Metadaten = .export_metadata(list(
      package_version = as.character(utils::packageVersion("regionalepi")),
      exported_at = format(exported_at, tz = "UTC", usetz = TRUE),
      snapshot_id = snapshot_id,
      from_fit_id = transition$provenance$from_fit_id,
      to_fit_id = transition$provenance$to_fit_id,
      from_reference_period = transition$provenance$from_reference_period,
      to_reference_period = transition$provenance$to_reference_period,
      reference_selection = "fixed_reviewed_comparison",
      configuration_type = "preset",
      solution_review_status = "reviewed_reference",
      exploratory = FALSE,
      geography_regime = "current_400_district_comparison",
      census_bases = c("census_2011", "census_2022"),
      indicator_specification = paste(
        demographic_structure_spec()$indicator_set_id,
        demographic_structure_spec()$definition_version, sep = "@"),
      fitting_specification = paste(
        dynamic_kmeans_spec()$fitting_specification_id,
        dynamic_kmeans_spec()$definition_version, sep = "@"),
      k = 3L, alignment = transition$provenance$alignment_version,
      comparison_id = transition$provenance$comparison_id,
      comparison_universe = "reviewed current 400-district geography",
      geography_reference = as.character(map$provenance$source_vintage)
    )),
    Methodik = .export_methodology(c(
      Fits = "Beide dynamischen Typologien wurden unabh\u00e4ngig angepasst.",
      Ausrichtung = "Fit-lokale C01/C02/C03-IDs werden anhand eindeutiger demografischer Profilanker ausgerichtet.",
      Vergleichsraum = "Eisenach 16056 bleibt in der Provenienz des 2017-2020-Fits erhalten, ist aber nicht Teil des aktuellen 400-Kreis-Vergleichs.",
      Interpretation = "Eine neue Clusterzuordnung ist nicht gleichbedeutend mit einer grundlegenden demografischen Ver\u00e4nderung."
    )),
    `Quellen und Lizenz` = .export_sources(FALSE)
  )
  names(sheets)[2:4] <- c("\u00dcbergangsmatrix", "\u00dcbergangsprozente",
    "Indikatorver\u00e4nderungen")
  sheets
}

.epidemiology_export_tables <- function(result, summaries,
    regional = NULL, reference = FALSE, geography_reference = NULL,
    exported_at = Sys.time()) {
  weekly <- summaries$weekly
  weekly$iso_year_week <- .iso_week_label(weekly$date)
  weekly$completeness_percentage <- 100 * weekly$observed_districts /
    weekly$expected_districts
  weekly <- weekly[, c("date", "iso_year_week", "cluster_id",
    "median_incidence", "q1_incidence", "q3_incidence",
    "observed_districts", "expected_districts", "missing_districts",
    "completeness_percentage"), drop = FALSE]
  district <- summaries$district_period
  loaded <- result$loaded_selection
  if (is.null(loaded)) loaded <- list(demographic_period = "2017-2020", k = 3L,
    demographic_source = "not_applicable")
  status <- result$bundle$diagnostics$status_by_reporting_year
  incidence_label <- if (isTRUE(reference))
    "source-provided SurvStat incidence" else "annual_average_population_v1"
  queries <- if ("source_incidence" %in% names(result$bundle$provenance))
    result$bundle$provenance$source_incidence else result$bundle$provenance$incidence
  source_status <- unique(c(.shiny_app_query_status(queries),
    .shiny_app_query_status(result$bundle$provenance$counts)))
  query_retrieved_at <- unique(unlist(lapply(c(queries,
    result$bundle$provenance$counts), function(query) {
      if (is.null(query$retrieved_at)) character() else
        as.character(query$retrieved_at)
    }), use.names = FALSE))
  sheets <- list(weekly = weekly, `Kreisbezogene Ergebnisse` = district)
  names(sheets)[[1L]] <- "W\u00f6chentliche Clusterinzidenz"
  if (!is.null(regional) && is.data.frame(regional) && nrow(regional)) {
    sheets$`Regionale Ergebnisse` <- regional
  }
  fit_provenance <- result$fit$provenance
  fitting <- result$fit$fitting_specification
  indicator_specification <- if (all(c("indicator_set_id",
      "indicator_set_version") %in% names(fit_provenance))) paste(
    fit_provenance$indicator_set_id, fit_provenance$indicator_set_version,
    sep = "@") else NULL
  fitting_specification <- if (all(c("fitting_specification_id",
      "fitting_specification_version") %in% names(fit_provenance))) paste(
    fit_provenance$fitting_specification_id,
    fit_provenance$fitting_specification_version, sep = "@") else NULL
  export_context <- list(
    snapshot_id = if (!isTRUE(reference))
      result$demographic$snapshot_provenance$snapshot_id %||% NA_character_
      else NA_character_,
    demographic_reference_years = paste(
      loaded$demographic_years %||% result$demographic_years %||% integer(),
      collapse = ";"),
    reference_selection = loaded$reference_selection %||% NA_character_,
    configuration_type = loaded$configuration_type %||%
      loaded$reference_selection %||% NA_character_,
    solution_review_status = loaded$solution_review_status %||%
      if (identical(loaded$reference_selection, "preset"))
        "reviewed_reference" else if (identical(
          loaded$reference_selection, "custom"))
            "not_individually_reviewed" else NA_character_,
    exploratory_typology = loaded$exploratory %||% NA,
    demographic_geography_regime = loaded$geography_regime %||% NA_character_,
    demographic_census_bases = paste(loaded$census_bases %||% character(),
      collapse = ";"),
    indicator_specification = indicator_specification %||% NA_character_,
    fitting_specification = fitting_specification %||% NA_character_,
    fit_id = result$fit$provenance$fit_id)
  snapshot_provenance <- if (!isTRUE(reference))
    result$demographic$snapshot_provenance else NULL
  observation_provenance <- if (is.null(snapshot_provenance)) {
    list(statuses = NULL, retrieved_at = NULL)
  } else .snapshot_export_observation_provenance(snapshot_provenance)
  for (sheet_name in names(sheets)) {
    for (field in names(export_context)) {
      sheets[[sheet_name]][[field]] <- rep(export_context[[field]],
        nrow(sheets[[sheet_name]]))
    }
  }
  sheets$Metadaten <- .export_metadata(list(
    analysis_type = if (isTRUE(reference))
      "Historische Referenzanalyse mit aktuellem SurvStat-Datenstand" else
      "Regul\u00e4re Surveillance-Analyse",
    package_version = as.character(utils::packageVersion("regionalepi")),
    exported_at = format(exported_at, tz = "UTC", usetz = TRUE),
    pathogen = result$window$pathogen,
    observation_window = result$window$label,
    effective_analysis_start = result$analysis_range$start_date,
    effective_analysis_end = result$analysis_range$end_date,
    analysis_period = result$analysis_range$label,
    analysis_period_mode = result$analysis_range$mode,
    demographic_reference_period = loaded$demographic_period,
    demographic_reference_years = loaded$demographic_years,
    reference_selection = loaded$reference_selection,
    configuration_type = loaded$configuration_type %||%
      loaded$reference_selection,
    solution_review_status = loaded$solution_review_status %||%
      if (identical(loaded$reference_selection, "preset"))
        "reviewed_reference" else if (identical(
          loaded$reference_selection, "custom"))
            "not_individually_reviewed" else NA_character_,
    exploratory_typology = loaded$exploratory,
    demographic_geography_regime = loaded$geography_regime,
    demographic_census_bases = loaded$census_bases,
    k = loaded$k, fit_id = result$fit$provenance$fit_id,
    indicator_specification = indicator_specification,
    fitting_specification = fitting_specification,
    fitting_algorithm = fitting$algorithm,
    fitting_nstart = fitting$nstart,
    fitting_iter_max = fitting$iter_max,
    fitting_seed = fitting$seed,
    snapshot_id = if (!isTRUE(reference))
      result$demographic$snapshot_provenance$snapshot_id else NULL,
    demographic_observation_source_status = observation_provenance$statuses,
    demographic_observation_retrieved_at = observation_provenance$retrieved_at,
    incidence_definition = incidence_label,
    population_basis = if (!isTRUE(reference))
      result$demographic$snapshot_provenance$population_basis else NULL,
    population_year = if (!is.null(status)) status$population_year else NULL,
    incidence_status = if (!is.null(status)) status$incidence_status else NULL,
    survstat_data_status = source_status,
    source_retrieved_at = query_retrieved_at,
    geography_reference = geography_reference
  ))
  methods <- c(
    Inzidenz = if (isTRUE(reference))
      "Die Referenzanalyse verwendet ausschlie\u00dflich die von SurvStat bereitgestellte historische Inzidenz."
      else "Fallzahlen werden durch die amtliche durchschnittliche Jahresbev\u00f6lkerung des Berichtsjahres geteilt und mit 100000 multipliziert.",
    Zusammenfassung = "Linie und IQR zeigen ungewichteten Median sowie empirisches erstes und drittes Quartil der Kreisinzidenzen.",
    Fehlende_Werte = "Beobachtete Null bleibt Null; fehlende Inzidenz bleibt NA und wird nicht als Null behandelt.",
    completeness = "Beobachtete und erwartete Kreise beziehungsweise Wochen werden getrennt ausgewiesen.",
    population_basis = "Provenienz unterscheidet Census-2011- und Census-2022-basierte Bev\u00f6lkerungswerte sowie gegebenenfalls vorl\u00e4ufige Nenner."
  )
  names(methods)[4:5] <- c("Vollst\u00e4ndigkeit", "Bev\u00f6lkerungsbasis")
  sheets$Methodik <- .export_methodology(methods)
  sheets$`Quellen und Lizenz` <- .export_sources(TRUE)
  sheets
}

.scientific_export_plot <- function(kind, data, metadata = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("PNG export requires package 'ggplot2'.", call. = FALSE)
  }
  if (identical(kind, "weekly")) {
    colours <- stats::setNames(metadata$display_colour,
      metadata$display_cluster_id)
    return(ggplot2::ggplot(data, ggplot2::aes(date, median_incidence,
      colour = cluster_id, group = cluster_id)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = q1_incidence,
        ymax = q3_incidence, fill = cluster_id), alpha = .16,
        colour = NA) + ggplot2::geom_line(linewidth = .8) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::scale_fill_manual(values = colours) +
      ggplot2::labs(x = NULL, y = "Inzidenz je 100.000",
        colour = "Demografischer Regionaltyp",
        fill = "Demografischer Regionaltyp",
        caption = "regionalepi; SurvStat@RKI; Regionaldatenbank Deutschland") +
      ggplot2::theme_minimal())
  }
  if (identical(kind, "district")) {
    colours <- stats::setNames(metadata$display_colour,
      metadata$display_cluster_id)
    return(ggplot2::ggplot(data, ggplot2::aes(cluster_id,
      median_period_incidence, fill = cluster_id)) +
      ggplot2::geom_boxplot(outlier.alpha = .35) +
      ggplot2::scale_fill_manual(values = colours) +
      ggplot2::labs(x = "Demografischer Regionaltyp",
        y = "Median der Wocheninzidenz",
        caption = "regionalepi; SurvStat@RKI; Regionaldatenbank Deutschland") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none"))
  }
  if (identical(kind, "weekly_district")) {
    required <- c("cluster_id", "cluster_display", "incidence", "date")
    if (!all(required %in% names(data)) || length(unique(data$date)) != 1L) {
      stop("Weekly district PNG data are incomplete.", call. = FALSE)
    }
    colours <- stats::setNames(metadata$display_colour,
      metadata$display_cluster_id)
    return(ggplot2::ggplot(data, ggplot2::aes(cluster_display, incidence,
      fill = cluster_id, colour = cluster_id)) +
      ggplot2::geom_violin(alpha = .2, trim = FALSE, na.rm = TRUE,
        show.legend = FALSE) +
      ggplot2::geom_boxplot(width = .15, outlier.shape = NA, alpha = .65,
        na.rm = TRUE, show.legend = FALSE) +
      ggplot2::geom_jitter(width = .09, alpha = .5, size = 1,
        na.rm = TRUE, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::scale_fill_manual(values = colours) +
      ggplot2::labs(x = "Demografischer Regionaltyp",
        y = "Kreisinzidenz je 100.000",
        title = .shiny_iso_week_display(unique(data$date))) +
      ggplot2::theme_minimal())
  }
  if (identical(kind, "profiles")) {
    return(ggplot2::ggplot(data, ggplot2::aes(indicator_id,
      standardized_center, fill = display_cluster_id)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::labs(x = "Indikator", y = "Standardisiertes Zentrum",
        fill = "Cluster",
        caption = "regionalepi; Regionaldatenbank Deutschland") +
      ggplot2::theme_minimal())
  }
  if (identical(kind, "transition")) {
    order <- .shiny_transition_profile_order()
    if (!all(order %in% rownames(data)) || !all(order %in% colnames(data))) {
      stop("Transition matrix does not contain the reviewed profile order.",
        call. = FALSE)
    }
    data <- data[order, order, drop = FALSE]
    labels <- .shiny_transition_profile_labels()
    frame <- as.data.frame(data, stringsAsFactors = FALSE)
    names(frame) <- c("from", "to", "n")
    frame$from <- factor(unname(labels[as.character(frame$from)]),
      levels = unname(labels[rev(order)]))
    frame$to <- factor(unname(labels[as.character(frame$to)]),
      levels = unname(labels[order]))
    return(ggplot2::ggplot(frame, ggplot2::aes(to, from, fill = n)) +
      ggplot2::geom_tile() + ggplot2::geom_text(ggplot2::aes(label = n)) +
      ggplot2::scale_fill_gradient(low = "#F7F7F9", high = "#5F627B") +
      ggplot2::labs(x = "2022\u20132025", y = "2017\u20132020", fill = "Kreise",
        caption = "regionalepi; Regionaldatenbank Deutschland") +
      ggplot2::theme_minimal())
  }
  stop("Unsupported scientific PNG export kind.", call. = FALSE)
}

.write_scientific_png <- function(plot, path, width = 10, height = 6,
    dpi = 300) {
  ggplot2::ggsave(path, plot = plot, width = width, height = height,
    dpi = dpi, units = "in", bg = "white")
  invisible(normalizePath(path, mustWork = TRUE))
}
