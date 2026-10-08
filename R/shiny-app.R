.shiny_demographic_periods <- function() {
  stats::setNames(
    list(2022:2025, 2017:2020),
    c("2022\u20132025", "2017\u20132020")
  )
}

.shiny_loaded_analysis_context <- function(result) {
  ids <- as.character(result$display_metadata$display_cluster_id)
  cluster_text <- paste(ids, collapse = " / ")
  if (identical(result$typology_mode, "dissertation")) {
    return(paste0(
      "Historische Referenztypologie (", cluster_text, ") \u00b7 ",
      "Historische SurvStat-Inzidenz"
    ))
  }
  period <- result$loaded_selection$demographic_period
  if (is.null(period) || length(period) != 1L || is.na(period) || !nzchar(period)) {
    period <- paste(range(result$demographic_years), collapse = "\u2013")
  }
  paste0(
    "Demografische Typologie \u00b7 ", period, " \u00b7 k=", length(ids), " \u00b7 ",
    "Inzidenz auf Basis der durchschnittlichen Jahresbev\u00f6lkerung"
  )
}

.shiny_loaded_analysis_is_provisional <- function(result) {
  status <- result$bundle$diagnostics$status_by_reporting_year
  is.data.frame(status) && nrow(status) > 0L &&
    any(status$incidence_status == "provisional")
}

.shiny_loaded_selection_changed <- function(
    result, typology_mode, demographic_period, k, demographic_source) {
  loaded <- result$loaded_selection
  if (is.null(loaded)) return(FALSE)
  source_mode <- demographic_source
  if (is.null(source_mode) || !source_mode %in% c("snapshot", "live")) {
    source_mode <- "snapshot"
  }
  changed <- !identical(typology_mode, loaded$typology_mode) ||
    !identical(source_mode, loaded$demographic_source)
  if (identical(typology_mode, "dynamic")) {
    changed <- changed ||
      !identical(demographic_period, loaded$demographic_period) ||
      !identical(as.integer(k), loaded$k)
  }
  changed
}

.shiny_regional_credentials_available <- function(getenv = Sys.getenv) {
  username <- getenv("REGIONALSTATISTIK_USER", unset = "")
  password <- getenv("REGIONALSTATISTIK_PASSWORD", unset = "")
  is.character(username) && length(username) == 1L && !is.na(username) &&
    nzchar(trimws(username)) &&
    is.character(password) && length(password) == 1L && !is.na(password) &&
    nzchar(trimws(password))
}

.shiny_pathogen_registry <- function() {
  list(
    "Influenza, saisonal" = list(
      pathogen_id = "influenza", display_label = "Influenza",
      survstat_member = "Influenza, saisonal",
      period_factories = list("AGI-Influenzawellen" = rki_influenza_periods_2017_2026),
      specification_version = "shiny_pathogens_v1",
      temporal_context = "Seasonal observation windows with separately reviewed RKI waves"
    ),
    "COVID-19" = list(
      pathogen_id = "covid19", display_label = "COVID-19",
      survstat_member = "COVID-19",
      period_factories = list(
        "Retrospektive Phaseneinteilung der Pandemie (RKI)" = dissertation_covid_welle2_periods,
        "Definierte COVID-19-Wellen (RKI)" = rki_covid_activity_waves
      ),
      specification_version = "shiny_pathogens_v1",
      temporal_context = "Neutral observation windows with separately reviewed periods"
    ),
    "Norovirus-Gastroenteritis" = list(
      pathogen_id = "norovirus", display_label = "Norovirus",
      survstat_member = "Norovirus-Gastroenteritis", period_factories = list(),
      specification_version = "shiny_pathogens_v1",
      temporal_context = paste(
        "Year-round occurrence with a typical October-to-March peak;",
        "ISO-week 27-to-26 comparison frame; no reviewed national wave catalogue"
      )
    )
  )
}

.shiny_pathogen_config <- function(pathogen) {
  config <- .shiny_pathogen_registry()[[pathogen]]
  if (is.null(config)) stop("Unsupported Shiny application pathogen.", call. = FALSE)
  config
}

.shiny_pathogen_choices <- function() {
  registry <- .shiny_pathogen_registry()
  stats::setNames(names(registry), vapply(registry, `[[`, character(1L), "display_label"))
}

.shiny_period_resources <- function(pathogen) {
  factories <- .shiny_pathogen_config(pathogen)$period_factories
  lapply(factories, function(factory) factory())
}

.shiny_period_resource <- function(pathogen) .shiny_period_resources(pathogen)[[1L]]

.shiny_window_choices <- function(pathogen) {
  x <- regionalepi_observation_windows()
  x <- x[x$pathogen == pathogen, , drop = FALSE]
  stats::setNames(x$observation_window_id, x$label)
}

.shiny_selected_window <- function(pathogen, id) {
  x <- regionalepi_observation_windows()
  x <- x[x$pathogen == pathogen & x$observation_window_id == id, , drop = FALSE]
  if (nrow(x) != 1L) stop("Unknown observation window.", call. = FALSE)
  x
}

.shiny_reconcile_selection <- function(pathogen, window_id = NULL,
                                       range_mode = "window",
                                       reviewed_period_id = NULL) {
  choices <- .shiny_window_choices(pathogen)
  valid_ids <- unname(choices)
  selected <- if (length(window_id) == 1L && !is.na(window_id) &&
                  window_id %in% valid_ids) window_id else utils::tail(valid_ids, 1L)
  window <- .shiny_selected_window(pathogen, selected)
  periods <- .shiny_periods_in_window(window)
  if (length(range_mode) != 1L || is.na(range_mode) ||
      !range_mode %in% c("window", "reviewed", "custom")) range_mode <- "window"
  mode <- if (identical(range_mode, "reviewed") && !nrow(periods)) "window" else range_mode
  period_ids <- periods$period_id
  selected_period <- if (length(period_ids)) {
    if (length(reviewed_period_id) == 1L && !is.na(reviewed_period_id) &&
        reviewed_period_id %in% period_ids) reviewed_period_id else period_ids[[1L]]
  } else NA_character_
  list(pathogen = pathogen, window_id = selected, window = window,
       periods = periods, range_mode = mode,
       reviewed_period_id = selected_period, choices = choices)
}

.shiny_periods_in_window <- function(window) {
  resources <- .shiny_period_resources(window$pathogen)
  if (!length(resources)) return(.empty_epidemiological_periods())
  periods <- do.call(rbind, Map(function(resource,system) {
    x <- resource$periods; x$period_system <- system; x
  },resources,names(resources)))
  periods[periods$start_date >= window$start_date &
            periods$end_date <= window$end_date, , drop = FALSE]
}

.shiny_period_choices_in_window <- function(window) {
  periods <- .shiny_periods_in_window(window)
  if (!nrow(periods)) return(list())
  groups <- split(periods, factor(periods$period_system,
    levels=names(.shiny_period_resources(window$pathogen))))
  groups <- groups[vapply(groups,nrow,integer(1L))>0L]
  lapply(groups,function(x)stats::setNames(x$period_id,x$label))
}

.shiny_period_choices <- function(pathogen) {
  lapply(.shiny_period_resources(pathogen), function(resource) {
    stats::setNames(resource$periods$period_id, resource$periods$label)
  })
}

.shiny_reference_pathogen_choices <- function() {
  c("Influenza" = "Influenza, saisonal", "COVID-19" = "COVID-19")
}

.shiny_reference_selection <- function(pathogen, window_id = NULL,
                                       period_id = NULL) {
  if (!pathogen %in% unname(.shiny_reference_pathogen_choices())) {
    stop("Unsupported reference-analysis pathogen.", call. = FALSE)
  }
  if (identical(pathogen, "Influenza, saisonal")) {
    allowed_windows <- c(
      influenza_2017_18 = "2017/18",
      influenza_2018_19 = "2018/19",
      influenza_2019_20 = "2019/20"
    )
    periods <- dissertation_influenza_periods()
  } else {
    allowed_windows <- c(
      covid19_pandemic_2020_22 = "Pandemie-Beobachtungszeitraum 2020\u20132022"
    )
    periods <- dissertation_covid_welle2_periods()
  }
  window_id <- if (length(window_id) == 1L && !is.na(window_id) &&
      window_id %in% names(allowed_windows)) window_id else names(allowed_windows)[[1L]]
  windows <- regionalepi_observation_windows()
  window <- windows[
    windows$pathogen == pathogen & windows$observation_window_id == window_id,
    , drop = FALSE
  ]
  if (nrow(window) != 1L) stop("Reference-analysis window is unavailable.", call. = FALSE)
  available <- periods$periods[
    periods$periods$start_date >= window$start_date &
      periods$periods$end_date <= window$end_date, , drop = FALSE
  ]
  if (!nrow(available)) stop("Reference-analysis period is unavailable.", call. = FALSE)
  period_id <- if (length(period_id) == 1L && !is.na(period_id) &&
      period_id %in% available$period_id) period_id else available$period_id[[1L]]
  list(
    pathogen = pathogen,
    window = window,
    window_choices = stats::setNames(names(allowed_windows), allowed_windows),
    period_resource = periods,
    periods = available,
    period_id = period_id,
    period_choices = stats::setNames(available$period_id, available$label)
  )
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

.shiny_typology_configuration <- function(
    period_label, k, source_mode = "snapshot",
    configuration_id = "reviewed_demographic_typology_v1",
    indicator_ids = vapply(
      demographic_structure_spec()$indicators, `[[`, character(1L),
      "indicator_id"
    ),
    fitting_specification = dynamic_kmeans_spec()) {
  years <- .shiny_demographic_periods()[[period_label]]
  if (is.null(years)) stop("Unsupported demographic reference period.", call. = FALSE)
  if (!source_mode %in% c("snapshot", "live"))
    stop("Unsupported demographic source mode.", call. = FALSE)
  k <- as.integer(k)
  if (length(k) != 1L || is.na(k) || !k %in% 2:5)
    stop("Unsupported dynamic cluster count.", call. = FALSE)
  indicator_ids <- sort(unique(as.character(indicator_ids)), method = "radix")
  if (!length(indicator_ids) || anyNA(indicator_ids) || any(!nzchar(indicator_ids)))
    stop("Typology configuration requires indicator identities.", call. = FALSE)
  list(
    configuration_id = configuration_id,
    specification_id = demographic_structure_spec()$indicator_set_id,
    indicator_ids = indicator_ids,
    reference_years = as.integer(years),
    source_identity = source_mode,
    k = k,
    fitting_specification_id = fitting_specification$fitting_specification_id,
    fitting_specification_version = fitting_specification$definition_version,
    fit_parameters = fitting_specification[c(
      "method", "algorithm", "nstart", "iter_max", "seed", "row_order"
    )]
  )
}

.shiny_typology_configuration_key <- function(configuration,
                                               data_provenance_id) {
  required <- c(
    "configuration_id", "specification_id", "indicator_ids",
    "reference_years", "source_identity", "k",
    "fitting_specification_id", "fitting_specification_version",
    "fit_parameters"
  )
  if (!is.list(configuration) || !all(required %in% names(configuration)) ||
      length(data_provenance_id) != 1L || is.na(data_provenance_id) ||
      !nzchar(data_provenance_id)) {
    stop("Incomplete typology configuration identity.", call. = FALSE)
  }
  parameter_text <- paste(
    names(configuration$fit_parameters),
    unlist(configuration$fit_parameters, use.names = FALSE), sep = "=",
    collapse = ","
  )
  paste(
    "typology-configuration", configuration$configuration_id,
    configuration$specification_id,
    paste(configuration$indicator_ids, collapse = ","),
    paste(configuration$reference_years, collapse = "-"),
    configuration$source_identity, configuration$k,
    configuration$fitting_specification_id,
    configuration$fitting_specification_version, parameter_text,
    data_provenance_id, sep = ":"
  )
}

.shiny_surveillance_cache_key <- function(pathogen, reporting_years) {
  paste("survstat-incidence", pathogen,
        paste(sort(unique(reporting_years)), collapse = "-"), sep = ":")
}

.shiny_surveillance_bundle_cache_key <- function(
    pathogen, reporting_years,
    incidence_definition = "source_survstat_incidence",
    surveillance_data_status = "unknown",
    population_data_status = "not_applicable",
    population_source_mode = "not_applicable",
    incidence_status = "source",
    provisional_denominator_year = NA_integer_,
    effective_analysis_end = NA_character_) {
  paste(
    "surveillance-analysis", pathogen,
    paste(sort(unique(reporting_years)), collapse = "-"),
    incidence_definition, surveillance_data_status, population_data_status,
    population_source_mode,
    incidence_status,
    paste(provisional_denominator_year, collapse = "-"),
    as.character(effective_analysis_end),
    "reference-definition", .survstat_reporting_path, sep = ":"
  )
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

.shiny_surveillance_cache_match <- function(
    index, pathogen, reporting_years,
    incidence_definition = "source_survstat_incidence",
    population_source_mode = "not_applicable",
    effective_analysis_end = NA_character_) {
  if (!length(index)) return(NULL)
  required <- sort(unique(as.integer(reporting_years)))
  compatible <- vapply(index, function(entry) {
    identical(entry$pathogen, pathogen) &&
      identical(entry$incidence_definition %||% "source_survstat_incidence",
                incidence_definition) &&
      identical(entry$population_source_mode %||% "not_applicable",
                population_source_mode) &&
      identical(as.character(entry$effective_analysis_end %||% NA_character_),
                as.character(effective_analysis_end)) &&
      all(required %in% entry$reporting_years)
  }, logical(1L))
  if (!any(compatible)) return(NULL)
  candidates <- index[compatible]
  sizes <- vapply(candidates, function(entry) length(entry$reporting_years),
                  integer(1L))
  candidates[[which.min(sizes)]]$key
}

.shiny_profile_palette_alignment <- function(profiles, reference_profiles,
                                             tolerance = sqrt(.Machine$double.eps)) {
  required <- c("display_cluster_id", "indicator_id", "standardized_center")
  if (!all(required %in% names(profiles)) ||
      !all(required %in% names(reference_profiles))) {
    stop("Palette alignment requires complete profile centers.", call. = FALSE)
  }
  dynamic_ids <- sort(unique(profiles$display_cluster_id))
  reference_ids <- c("ClD", "ClJ", "ClA")
  indicators <- sort(unique(profiles$indicator_id))
  if (length(dynamic_ids) != 3L ||
      !setequal(unique(reference_profiles$display_cluster_id), reference_ids) ||
      !setequal(indicators, unique(reference_profiles$indicator_id))) {
    stop("Profile-aligned colours require compatible three-cluster profiles.",
         call. = FALSE)
  }
  matrix_for <- function(x, ids) vapply(ids, function(id) {
    rows <- x[x$display_cluster_id == id, , drop = FALSE]
    rows$standardized_center[match(indicators, rows$indicator_id)]
  }, numeric(length(indicators)))
  dynamic <- matrix_for(profiles, dynamic_ids)
  reference <- matrix_for(reference_profiles, reference_ids)
  rownames(dynamic) <- rownames(reference) <- indicators
  if (anyNA(dynamic) || anyNA(reference))
    stop("Profile-aligned colours require complete indicator centers.",call.=FALSE)
  defining_indicator <- c(ClD="population_density",ClJ="youth_dependency_ratio",
                          ClA="mean_age")
  if (!all(defining_indicator %in% rownames(dynamic)))
    stop("Profile-aligned colours require the reviewed defining indicators.",call.=FALSE)
  permutations <- rbind(c(1L,2L,3L),c(1L,3L,2L),c(2L,1L,3L),
                        c(2L,3L,1L),c(3L,1L,2L),c(3L,2L,1L))
  scores <- apply(permutations,1L,function(p)sum(vapply(seq_along(reference_ids),
    function(i)dynamic[defining_indicator[[reference_ids[[i]]]],p[[i]]],numeric(1L))))
  ordering <- order(-scores)
  margin <- scores[ordering[[1L]]] - scores[ordering[[2L]]]
  if (!is.finite(margin) || margin <= tolerance) {
    stop("Profile-aligned dynamic colours are ambiguous.", call. = FALSE)
  }
  mapping <- stats::setNames(rep(NA_character_,length(dynamic_ids)),dynamic_ids)
  mapping[dynamic_ids[permutations[ordering[[1L]],]]] <- reference_ids
  list(method = paste("maximum one-to-one alignment on reviewed defining profile",
                      "features: density, youth dependency, and mean age"),
       mapping = mapping, best_score = scores[ordering[[1L]]],
       second_best_score = scores[ordering[[2L]]], margin = margin,
       ambiguous = FALSE)
}

.shiny_dynamic_colour_policy <- function(fit, reference_fit, anchor_fit = fit) {
  anchor_alignment <- .shiny_profile_palette_alignment(
    anchor_fit$profiles, reference_fit$profiles)
  anchors <- c(ClD="#bc5e21",ClJ="#748c61",ClA="#274f66")
  additions <- c("#7B61A8", "#A94F74")
  target_ids <- sort(unique(fit$assignments$display_cluster_id))
  anchor_ids <- sort(unique(anchor_fit$assignments$display_cluster_id))
  if (length(target_ids) < 2L || length(target_ids) > 5L || length(anchor_ids) != 3L)
    stop("Dynamic colour policy supports k = 2 to 5.", call. = FALSE)
  if (length(target_ids) == 2L) {
    common <- intersect(anchor_fit$assignments$geo_id, fit$assignments$geo_id)
    source <- anchor_fit$assignments$display_cluster_id[
      match(common, anchor_fit$assignments$geo_id)]
    target <- fit$assignments$display_cluster_id[match(common, fit$assignments$geo_id)]
    table_overlap <- table(source, target)
    source_centers <- stats::xtabs(standardized_center~display_cluster_id+indicator_id,
                                    anchor_fit$profiles)
    target_centers <- stats::xtabs(standardized_center~display_cluster_id+indicator_id,
                                    fit$profiles)
    distances <- outer(seq_len(nrow(source_centers)),seq_len(nrow(target_centers)),
      Vectorize(function(i,j)sqrt(sum((source_centers[i,]-target_centers[j,])^2))))
    dimnames(distances) <- list(rownames(source_centers),rownames(target_centers))
    target_fraction <- prop.table(table_overlap, 2L)
    dominant <- apply(target_fraction, 2L, which.max)
    nearest <- apply(distances, 2L, which.min)
    clear <- colnames(table_overlap)[vapply(seq_along(target_ids), function(i)
      max(target_fraction[,i]) >= .8 && dominant[[i]] == nearest[[i]] &&
        distances[nearest[[i]],i] < .75, logical(1L))]
    continuation <- stats::setNames(character(), character())
    if (length(clear)) continuation <- stats::setNames(clear,
      rownames(table_overlap)[dominant[match(clear,colnames(table_overlap))]])
    colours <- stats::setNames(rep(additions[[1L]],length(target_ids)),target_ids)
    for (source_id in names(continuation)) {
      reference_id <- anchor_alignment$mapping[[source_id]]
      colours[[continuation[[source_id]]]] <- anchors[[reference_id]]
    }
    remainder <- names(colours)[!names(colours)%in%unname(continuation)]
    colours[remainder] <- additions[seq_along(remainder)]
    return(list(colours=colours,anchor_alignment=anchor_alignment,
      continuation=continuation,branch_alignment=stats::setNames(
        character(), character()),total_shared=if(length(clear))sum(table_overlap[
        cbind(names(continuation),unname(continuation))]) else 0,
      total_center_distance=if(length(clear))sum(distances[
        cbind(names(continuation),unname(continuation))]) else 0,
      target_anchor_fraction=target_fraction,center_distances=distances,
      method=paste("k=2 clear anchor requires at least 80% target membership,",
        "the same nearest standardized center, and distance below 0.75;",
        "coarse mixtures use the existing additional purple")))
  }
  if (identical(fit$provenance$fit_id, anchor_fit$provenance$fit_id)) {
    continuation <- stats::setNames(anchor_ids, anchor_ids)
    overlap <- nrow(fit$assignments)
    distance <- 0
  } else {
    common <- intersect(anchor_fit$assignments$geo_id, fit$assignments$geo_id)
    source <- anchor_fit$assignments$display_cluster_id[
      match(common, anchor_fit$assignments$geo_id)]
    target <- fit$assignments$display_cluster_id[match(common, fit$assignments$geo_id)]
    table_overlap <- table(source, target)
    source_centers <- stats::xtabs(standardized_center~display_cluster_id+indicator_id,
                            anchor_fit$profiles)
    target_centers <- stats::xtabs(standardized_center~display_cluster_id+indicator_id,
                            fit$profiles)
    distances <- outer(seq_len(nrow(source_centers)), seq_len(nrow(target_centers)),
      Vectorize(function(i, j) sqrt(sum(
        (source_centers[i, ] - target_centers[j, ])^2))))
    dimnames(distances) <- list(rownames(source_centers), rownames(target_centers))
    candidates <- expand.grid(rep(list(target_ids), length(anchor_ids)),
                              stringsAsFactors=FALSE)
    candidates <- candidates[apply(candidates,1L,function(x)length(unique(x))==length(x)),,drop=FALSE]
    overlap_scores <- apply(candidates,1L,function(x)sum(table_overlap[
      cbind(anchor_ids,x)]))
    distance_scores <- apply(candidates, 1L, function(x) sum(
      distances[cbind(anchor_ids, x)]))
    ordering <- order(distance_scores,-overlap_scores,
                      apply(candidates,1L,paste,collapse="\r"),method="radix")
    best <- ordering[[1L]]
    continuation <- stats::setNames(as.character(candidates[best,]),anchor_ids)
    overlap <- overlap_scores[[best]]; distance <- distance_scores[[best]]
  }
  colours <- stats::setNames(rep(NA_character_,length(target_ids)),target_ids)
  for (source_id in names(continuation)) {
    reference_id <- anchor_alignment$mapping[[source_id]]
    colours[[continuation[[source_id]]]] <- anchors[[reference_id]]
  }
  remainder <- names(colours)[is.na(colours)]
  branch_alignment <- stats::setNames(character(), character())
  if (length(remainder)) {
    nearest <- apply(distances[, remainder, drop = FALSE], 2L, function(value) {
      ordered <- order(value, names(value), method = "radix")
      c(anchor = unname(names(value)[ordered[[1L]]]),
        margin = unname(value[ordered[[2L]]] - value[ordered[[1L]]]))
    })
    if (is.null(dim(nearest))) nearest <- matrix(nearest, ncol = 1L,
      dimnames = list(names(nearest), remainder))
    clear_branch <- as.numeric(nearest["margin", ]) >= 0.25
    branch_targets <- remainder[clear_branch]
    mixed_targets <- remainder[!clear_branch]
    if (length(branch_targets)) {
      branch_alignment <- stats::setNames(
        unname(nearest["anchor", branch_targets]), branch_targets)
      colours[branch_targets] <- additions[[2L]]
    }
    if (length(mixed_targets)) colours[mixed_targets] <- additions[[1L]]
  }
  list(colours=colours, anchor_alignment=anchor_alignment,
       continuation=continuation, branch_alignment=branch_alignment,
       total_shared=overlap,
       total_center_distance=distance,
       method=paste("minimum one-to-one standardized-center distance; district",
                    "overlap as tie-breaker; unmatched targets with a nearest-anchor",
                    "distance margin of at least 0.25 use the stable branch colour;",
                    "ambiguous residual targets use the mixed purple"))
}

.shiny_finalize_incidence_legend <- function(widget) {
  selected_seen <- FALSE
  for (i in seq_along(widget$x$data)) {
    trace <- widget$x$data[[i]]
    if (identical(trace$fill, "toself")) {
      widget$x$data[[i]]$showlegend <- FALSE
    } else if (grepl("lines",if(is.null(trace$mode)) "" else trace$mode,
                     fixed=TRUE) &&
               (is.null(trace$name) || !nzchar(trace$name))) {
      widget$x$data[[i]]$name <- "Ausgew\u00e4hlter Kreis"
      widget$x$data[[i]]$showlegend <- !selected_seen
      selected_seen <- TRUE
    } else if (identical(trace$name, "Ausgew\u00e4hlter Kreis")) {
      widget$x$data[[i]]$showlegend <- !selected_seen
      selected_seen <- TRUE
    }
  }
  widget
}

.shiny_clean_named_legend <- function(widget, labels) {
  labels <- unique(as.character(labels))
  labels <- labels[order(nchar(labels),decreasing=TRUE)]
  seen <- character()
  for (i in seq_along(widget$x$data)) {
    name <- widget$x$data[[i]]$name
    if (is.null(name) || !nzchar(name)) next
    hits <- labels[vapply(labels,function(label)
      grepl(label,name,fixed=TRUE),logical(1L))]
    if (!length(hits)) next
    clean <- hits[[1L]]
    widget$x$data[[i]]$name <- clean
    widget$x$data[[i]]$legendgroup <- clean
    widget$x$data[[i]]$showlegend <- !clean %in% seen
    seen <- c(seen,clean)
  }
  widget
}

.shiny_app_cluster_colours <- function(ids, mode = "dynamic",
                                       variant = "neutral", alignment = NULL) {
  if (identical(mode, "dissertation")) {
    historical <- c(ClD = "#bc5e21", ClJ = "#748c61", ClA = "#274f66")
    if (any(!unique(ids) %in% names(historical))) {
      stop("Unknown dissertation cluster identity.", call. = FALSE)
    }
    return(historical[unique(ids)])
  }
  if (identical(variant, "profile_aligned")) {
    if (!is.null(alignment$colours)) {
      levels <- sort(unique(ids))
      if (!all(levels %in% names(alignment$colours)))
        stop("Dynamic colour policy does not cover every cluster.",call.=FALSE)
      return(alignment$colours[levels])
    }
    if (is.null(alignment) || isTRUE(alignment$ambiguous) ||
        !all(unique(ids) %in% names(alignment$mapping))) {
      stop("Profile-aligned colours require an unambiguous reviewed alignment.",
           call. = FALSE)
    }
    historical <- c(ClD = "#bc5e21", ClJ = "#748c61", ClA = "#274f66")
    levels <- sort(unique(ids))
    return(stats::setNames(unname(historical[alignment$mapping[levels]]), levels))
  }
  if (!identical(variant, "neutral")) stop("Unknown dynamic colour variant.", call. = FALSE)
  # Okabe-Ito categorical colours: colour-vision-friendly and non-sequential.
  palette <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00")
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
                                       mode = "dynamic", state_geojson = NULL,
                                       exterior_geojson = NULL,
                                       colour_variant = "neutral", alignment = NULL,
                                       display_metadata = NULL) {
  widget <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 4))
  widget <- leaflet::addGeoJSON(widget, geojson)
  if (!is.null(assignments) && !"state_name" %in% names(assignments))
    assignments$state_name <- NA_character_
  if (!is.null(assignments) && !"profile_description" %in% names(assignments))
    assignments$profile_description <- NA_character_
  colours <- if (is.null(assignments)) NULL else if (is.null(display_metadata))
    .shiny_app_cluster_colours(assignments$display_cluster_id, mode,
                               colour_variant, alignment) else
    stats::setNames(display_metadata$display_colour,
                    display_metadata$display_cluster_id)
  display <- if (is.null(assignments)) NULL else list(
    clusters = stats::setNames(as.list(assignments$display_cluster_id),
                               assignments$geo_id),
    colours = as.list(colours),
    states = stats::setNames(as.list(assignments$state_name), assignments$geo_id),
    profiles = stats::setNames(as.list(assignments$profile_description),
                               assignments$geo_id)
  )
  if (!is.null(state_geojson)) widget <- leaflet::addGeoJSON(widget, state_geojson,
    options = leaflet::pathOptions(color = "#F7F7F3", weight = 1.2, opacity = .98,
      fill = FALSE, interactive = FALSE), group = "Bundesl\u00e4nder")
  if (!is.null(exterior_geojson)) widget <- leaflet::addGeoJSON(
    widget, exterior_geojson,
    options = leaflet::pathOptions(color = "#27313A", weight = 2.1,
      opacity = .95, fill = FALSE, interactive = FALSE),
    group = "Deutschland-Au\u00dfenlinie")
  widget <- htmlwidgets::onRender(widget, "
    function(el, x, display) {
      var map = this;
      map.eachLayer(function(layer) {
        if (!layer.feature || !layer.feature.properties) return;
        var p = layer.feature.properties;
        if (!p.geo_id || p.geo_id.length !== 5) return;
        if (display && display.clusters) {
          p.cluster = display.clusters[p.geo_id];
          p.fillColor = display.colours[p.cluster];
          p.state_name = display.states[p.geo_id];
          p.profile = display.profiles[p.geo_id];
        }
        layer.setStyle({color:'#F4F4F0', weight:0.45, opacity:0.95,
                        fillColor:p.fillColor, fillOpacity:0.88});
        var profile = p.profile ? '<br>' + p.profile : '';
        layer.bindTooltip('<strong>' + p.geo_name + '</strong><br>' +
                          p.state_name + '<br>AGS ' + p.geo_id + ' \\u00b7 ' +
                          p.cluster + profile);
        layer.on('click', function() {
          Shiny.setInputValue('selected_geo_id', p.geo_id,
                              {priority:'event'});
        });
      });
      if (window.Shiny) Shiny.addCustomMessageHandler('regionalepi-select', function(id) {
        map.eachLayer(function(layer) {
          if (!layer.feature || !layer.feature.properties) return;
          var p = layer.feature.properties;
          if (!p.geo_id || p.geo_id.length !== 5) return;
          layer.setStyle({color: p.geo_id === id ? '#111111' : '#F4F4F0',
                          weight: p.geo_id === id ? 3.25 : 0.45,
                          opacity: p.geo_id === id ? 1 : 0.95,
                          fillOpacity: p.geo_id === id ? 0.94 : 0.88});
        });
      });
    }", data = display)
  widget <- leaflet::addEasyButton(widget, leaflet::easyButton(
    icon = "fa-home", title = "Deutschland anzeigen",
    onClick = htmlwidgets::JS(sprintf(
      "function(btn,map){map.fitBounds([[%.8f,%.8f],[%.8f,%.8f]],{padding:[4,4]});}",
      bounds[["lat1"]], bounds[["lng1"]], bounds[["lat2"]], bounds[["lng2"]]))))
  leaflet::fitBounds(widget, bounds[["lng1"]], bounds[["lat1"]],
                     bounds[["lng2"]], bounds[["lat2"]], options = list(padding = c(4, 4)))
}

.shiny_germany_outline_geojson <- function() {
  germany_exterior_outline_2024
}

.shiny_app_leaflet_map <- function(map, assignments, mode = "dynamic") {
  .shiny_app_leaflet_geojson(
    if (!is.null(map$browser_geojson)) map$browser_geojson else
      .shiny_app_map_geojson(map, assignments, mode),
    .shiny_app_map_bounds(map), assignments, mode,
    exterior_geojson = .shiny_germany_outline_geojson()
  )
}

.shiny_typology_config <- function(mode) {
  if (identical(mode, "dissertation")) return(list(
    mode = mode, label = "Historische Referenztypologie (2017\u20132020)", years = 2017:2020,
    k = 3L, note = "Historische Drei-Cluster-Typologie (2017\u20132020)"
  ))
  if (identical(mode, "dynamic")) return(list(
    mode = mode, label = "Aktualisierte demografische Typologie",
    years = NULL, k = NULL, note = "Fit-lokale neutrale Clusteridentit\u00e4ten"
  ))
  stop("Unsupported typology mode.", call. = FALSE)
}

.shiny_historical_semantic_mapping <- function(frozen) {
  indicator_order <- frozen$matrix$indicator_order
  centers <- frozen$diagnostics$centers
  defining_indicators <- c(
    ClD = "population_density",
    ClJ = "youth_dependency_ratio",
    ClA = "mean_age"
  )
  positions <- stats::setNames(
    match(defining_indicators, indicator_order), names(defining_indicators)
  )
  if (anyNA(positions) || nrow(centers) != 3L) {
    stop("Historical Shiny fit does not provide the reviewed profile anchors.",
         call. = FALSE)
  }
  raw_ids <- vapply(positions, function(position) {
    values <- centers[, position]
    winners <- which(values == max(values))
    if (length(winners) != 1L) {
      stop("Historical Shiny profile anchors are not unique.", call. = FALSE)
    }
    as.integer(rownames(centers)[winners])
  }, integer(1L))
  if (anyNA(raw_ids) || anyDuplicated(raw_ids)) {
    stop("Historical Shiny profile anchors do not identify three clusters.",
         call. = FALSE)
  }
  labels <- c(
    ClD = "dichte Regionen",
    ClJ = "familiengepr\u00e4gte Regionen",
    ClA = "\u00e4ltere, l\u00e4ndliche Regionen"
  )
  data.frame(
    raw_cluster = unname(raw_ids),
    cluster_code = names(raw_ids),
    cluster_label = unname(labels[names(raw_ids)]),
    stringsAsFactors = FALSE
  )
}

.shiny_dissertation_fit <- function(summary) {
  frozen <- fit_dissertation_typology(summary, label_mapping = "none")
  applied_mapping <- .shiny_historical_semantic_mapping(frozen)
  position <- match(frozen$data$raw_cluster, applied_mapping$raw_cluster)
  frozen$data$cluster_code <- applied_mapping$cluster_code[position]
  frozen$data$cluster_label <- applied_mapping$cluster_label[position]
  frozen$diagnostics$label_mapping <- "historical_reference"
  frozen$diagnostics$applied_mapping <- applied_mapping
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
  if (grepl("annual-average-population incidence|durchschnittlichen Jahresbev\u00f6lkerung",
            message, ignore.case = TRUE)) {
    return(paste(
      "Die dynamische Inzidenzanalyse ben\u00f6tigt die amtliche durchschnittliche Jahresbev\u00f6lkerung aus der Regionaldatenbank.",
      "Bitte konfigurieren Sie die Zugangsdaten und laden Sie die Analyse erneut."
    ))
  }
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
      "Die demografischen Daten f\u00fcr die Typologie konnten nicht geladen werden.",
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

.shiny_app_failure_category <- function(error, stage) {
  message <- conditionMessage(error)
  if (grepl("SurvStat API transport", message, ignore.case = TRUE)) {
    return("survstat_transport")
  }
  if (grepl("SurvStat API response|SOAP|XML", message, ignore.case = TRUE)) {
    return("survstat_response")
  }
  if (grepl("SurvStat.*retrieval failed", message, ignore.case = TRUE)) {
    return("survstat_retrieval")
  }
  if (grepl("geograph|geo_id|resolution|spatial|aggregate",
            message, ignore.case = TRUE)) {
    return("geography")
  }
  if (grepl("incidence|annual-average|population denominator|case count",
            message, ignore.case = TRUE)) {
    return("incidence_preparation")
  }
  if (grepl("Regionaldatenbank", message, ignore.case = TRUE)) {
    return("regionaldatenbank")
  }
  if (identical(stage, "typology")) return("typology")
  if (identical(stage, "map_and_result")) return("result_assembly")
  "unexpected"
}

.shiny_app_safe_failure_message <- function(category) {
  switch(category,
    survstat_transport = "SurvStat transport or source availability failed.",
    survstat_response = "SurvStat response parsing or validation failed.",
    survstat_retrieval = "SurvStat retrieval failed before analysis preparation.",
    geography = "Geography resolution or aggregation failed.",
    incidence_preparation = "Incidence preparation or denominator validation failed.",
    regionaldatenbank = "Regionaldatenbank retrieval or validation failed.",
    typology = "Typology preparation failed.",
    result_assembly = "Analysis result assembly failed.",
    "Unexpected analysis-load failure."
  )
}

.shiny_app_log_failure <- function(stage, error, logger = message) {
  class <- paste(class(error), collapse = "/")
  category <- .shiny_app_failure_category(error, stage)
  safe_message <- .shiny_app_safe_failure_message(category)
  logger(sprintf(
    "regionalepi analysis load failed: stage=%s; category=%s; condition=%s; message=%s",
    stage, category, class, safe_message
  ))
  invisible(list(stage = stage, category = category,
                 condition_class = class, message = safe_message))
}

.shiny_app_handle_failure <- function(state, error, stage, logger = message) {
  state$technical_error <- conditionMessage(error)
  diagnostic <- .shiny_app_log_failure(stage, error, logger)
  state$error <- .shiny_app_format_error(error, "Die Live-Analyse")
  invisible(diagnostic)
}

.shiny_app_display_error <- function(error, previous_result_visible) {
  paste0(
    error,
    if (isTRUE(previous_result_visible))
      " Die zuvor geladene Analyse bleibt angezeigt."
  )
}

.shiny_reference_years <- function(period_row) {
  start <- as.integer(format(period_row$start_date, "%Y"))
  end <- as.integer(format(period_row$end_date, "%Y"))
  seq.int(start, end)
}

.shiny_period_context <- function(pathogen, range, reviewed_period = NULL,
                                  typology_mode = "dynamic",
                                  demographic_years = NULL, k = NULL) {
  title <- if (!is.null(reviewed_period) && nrow(reviewed_period))
    paste(pathogen,reviewed_period$label,sep=" \u00b7 ") else
    paste(pathogen,range$label,sep=" \u00b7 ")
  start_iso <- .iso_week_label(range$start_date)
  end_iso <- .iso_week_label(range$end_date)
  same_year <- substr(start_iso,1L,4L)==substr(end_iso,1L,4L)
  iso <- if(same_year) paste0(start_iso,"\u2013",sub("^[0-9]{4}-","",end_iso)) else
    paste0(start_iso,"\u2013",end_iso)
  subtitle <- paste0(format(range$start_date,"%d.%m.%Y"),"\u2013",
    format(range$end_date,"%d.%m.%Y")," \u00b7 ",iso)
  typology <- if(identical(typology_mode,"dissertation"))
    "Typologie: Historische Referenztypologie (2017\u20132020)" else paste0(
      "Typologie: Aktualisiert \u00b7 Referenzzeitraum ",min(demographic_years),
      "\u2013",max(demographic_years)," \u00b7 k=",as.integer(k))
  list(title=title,subtitle=subtitle,typology=typology)
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
      "Exact canonical geography compatibility for the Shiny application"
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
  states <- regionalepi_state_boundaries()$features
  descriptions <- if (is.null(fit$profiles)) NULL else
    .shiny_profile_descriptions(fit$profiles,
      if (is.null(fit$mode)) "dynamic" else fit$mode)
  profile <- if (is.null(descriptions)) rep(NA_character_, nrow(features)) else
    descriptions$profile_description[match(
      assignments$display_cluster_id[position], descriptions$display_cluster_id)]
  state_id <- substr(features$geo_id, 1L, 2L)
  state_position <- match(state_id, states$geo_id)
  if (anyNA(state_position)) stop("Reviewed state context is incomplete.", call. = FALSE)
  data.frame(
    geo_id = features$geo_id,
    geo_name = features$geo_name,
    state_name = states$geo_name[state_position],
    display_cluster_id = assignments$display_cluster_id[position],
    profile_description = profile,
    stringsAsFactors = FALSE
  ) -> joined
  list(data = joined, map_only_geo_ids = missing, typology_only_geo_ids = extra)
}

.shiny_current_display_assignments <- function(map_join) {
  .require_named_list(map_join, "data", "Shiny current-display assignments")
  .require_data_frame(map_join$data, "Shiny current-display assignments")
  .require_columns(map_join$data, c("geo_id", "display_cluster_id"),
                   "Shiny current-display assignments")
  output <- map_join$data[c("geo_id", "display_cluster_id")]
  names(output)[[2L]] <- "cluster_id"
  output
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
    statuses <- demographic$snapshot_provenance$source_data_status
    typology_date <- format(max(.validate_snapshot_statuses(
      statuses[names(statuses) != "annual_average_population"]
    )$parsed), "%d.%m.%Y")
    population_date <- format(.validate_snapshot_statuses(
      statuses, "annual_average_population"
    )$parsed[[which(names(statuses) == "annual_average_population")]],
    "%d.%m.%Y")
    return(paste0(
      "Gepr\u00fcfter Snapshot \u2013 Typologie-Datenstand ", typology_date,
      "; Jahresdurchschnittsbev\u00f6lkerung ", population_date
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

.shiny_fetch_surveillance_counts <- function(pathogen, years, resources) {
  token <- .stable_text_hash(c(pathogen, years, "cases"))
  fetched <- tryCatch(list(
    base = fetch_survstat_cases(
      pathogen, years, "kreis", query_id = paste0("shiny-kreis-cases-", token)
    ),
    berlin = fetch_survstat_cases(
      pathogen, years, "bundesland", geography_filter = "Berlin",
      query_id = paste0("shiny-berlin-cases-", token)
    )
  ), error = function(error) {
    stop("SurvStat@RKI count retrieval failed: ", conditionMessage(error),
         call. = FALSE)
  })
  base <- .shiny_resolve_survstat(
    fetched$base, resources, resources$survstat_aliases,
    resources$survstat_spatial_units
  )
  berlin <- .shiny_resolve_survstat(
    fetched$berlin, resources, resources$survstat_incidence_aliases
  )
  assembled <- assemble_surveillance_cases(
    base$data, berlin$data, base$provenance, berlin$provenance,
    resources$survstat_spatial_units$source_geo_id, "11000"
  )
  assembled$resolution <- list(kreis = base$resolution, berlin = berlin$resolution)
  assembled
}

.shiny_fetch_surveillance_bundle <- function(pathogen, years, resources) {
  incidence_time <- system.time(incidence <- .shiny_fetch_surveillance(
    pathogen, years, resources))[['elapsed']]
  count_time <- system.time(counts <- .shiny_fetch_surveillance_counts(
    pathogen, years, resources))[['elapsed']]
  combined_time <- system.time(combined <- combine_surveillance_incidence_counts(
    incidence, counts))[['elapsed']]
  combined$resolution <- incidence$resolution
  combined$timings <- c(incidence_retrieval = incidence_time,
    count_retrieval = count_time, compatibility = combined_time)
  combined
}

.shiny_limit_surveillance_range <- function(bundle, range) {
  if (is.null(range)) return(bundle)
  keep <- bundle$data$date >= range$start_date &
    bundle$data$date <= range$end_date
  bundle$data <- bundle$data[keep, , drop = FALSE]
  if (!nrow(bundle$data)) {
    stop("SurvStat returned no observations in the effective analysis range.",
         call. = FALSE)
  }
  bundle
}

.shiny_population_for_reporting_years <- function(years, source_mode) {
  candidates <- sort(unique(c(years, years - 1L)))
  candidates <- candidates[candidates >= 2017L]
  snapshot <- regionalepi_demographic_snapshot()
  if (identical(source_mode, "snapshot")) {
    candidates <- intersect(
      candidates, snapshot$provenance$covered_reporting_years
    )
    if (!length(candidates)) {
      stop("The reviewed snapshot contains no acceptable annual-average population denominator.",
           call. = FALSE)
    }
    population <- .demographic_snapshot_average_population(snapshot, candidates)
  } else {
    population <- fetch_regional_average_population(candidates)
    if (any(population$data$year <= 2020L)) {
      relations <- snapshot$provenance$denominator_geography_harmonization$relations
      target <- snapshot$data$population[
        snapshot$data$population$reference_date == as.Date("2025-12-31"),
        c("geo_id", "geo_name"), drop = FALSE
      ]
      population <- .harmonize_reviewed_annual_average_population(
        population, relations, target
      )
    }
  }
  available <- sort(unique(population$data$year))
  denominator <- vapply(years, function(year) {
    if (year %in% available) year else if ((year - 1L) %in% available) {
      year - 1L
    } else NA_integer_
  }, integer(1L))
  names(denominator) <- as.character(years)
  supported <- years[!is.na(denominator)]
  if (!length(supported)) {
    stop("No reporting year has an available same-year or preceding-year annual-average population denominator.",
         call. = FALSE)
  }
  provisional <- denominator[!is.na(denominator) & denominator < years]
  list(
    population = population,
    provisional = provisional,
    supported_years = supported,
    unsupported_years = years[is.na(denominator)]
  )
}

.shiny_fetch_analysis_bundle <- function(
    pathogen, years, resources, source_mode = "snapshot",
    effective_range = NULL) {
  years <- sort(unique(as.integer(years)))
  if (!source_mode %in% c("snapshot", "live")) {
    stop("Unsupported annual-average population source mode.", call. = FALSE)
  }
  if (identical(source_mode, "live") &&
      !.shiny_regional_credentials_available()) {
    stop(
      "Dynamic annual-average-population incidence requires configured Regionaldatenbank credentials.",
      call. = FALSE
    )
  }
  source_time <- system.time(source <- .shiny_fetch_surveillance_bundle(
    pathogen, years, resources
  ))[["elapsed"]]
  source <- .shiny_limit_surveillance_range(source, effective_range)
  observed_years <- if (is.data.frame(source$data) && nrow(source$data)) {
    sort(unique(source$data$reporting_year))
  } else years
  population_time <- system.time(selection <-
    .shiny_population_for_reporting_years(observed_years, source_mode))[["elapsed"]]
  if (length(selection$unsupported_years)) {
    source$data <- source$data[
      source$data$reporting_year %in% selection$supported_years, , drop = FALSE
    ]
  }
  derivation_time <- system.time(result <- prepare_analysis_incidence(
    source, selection$population, selection$provisional
  ))[["elapsed"]]
  result$resolution <- source$resolution
  result$diagnostics$population_source_mode <- source_mode
  result$diagnostics$unsupported_reporting_years <- selection$unsupported_years
  result$provenance$population_source_mode <- source_mode
  if (!is.null(effective_range)) {
    result$provenance$analysis_range <- list(
      observation_window_start = effective_range$nominal_start_date,
      observation_window_end = effective_range$nominal_end_date,
      analysis_as_of_date = effective_range$analysis_as_of_date,
      effective_analysis_start = effective_range$start_date,
      effective_analysis_end = effective_range$end_date
    )
  }
  result$timings <- c(source$timings, source_bundle = source_time,
                      population_retrieval = population_time,
                      incidence_derivation = derivation_time)
  result
}

.shiny_analysis_cache_metadata <- function(bundle) {
  status <- bundle$diagnostics$status %||% "source"
  status_by_year <- bundle$diagnostics$status_by_reporting_year
  provisional_year <- if (is.data.frame(status_by_year)) {
    unique(status_by_year$population_year[
      status_by_year$incidence_status == "provisional"
    ])
  } else {
    NA_integer_
  }
  incidence_queries <- bundle$provenance$source_incidence %||%
    bundle$provenance$incidence
  source_status <- sort(unique(c(
    .shiny_app_query_status(incidence_queries),
    .shiny_app_query_status(bundle$provenance$counts)
  )))
  population_status <- if ("population" %in% names(bundle$provenance)) {
    vapply(bundle$provenance$population, function(x) {
      as.character(x$data_status %||% "unknown")
    }, character(1L))
  } else {
    "not_applicable"
  }
  list(
    surveillance_data_status = paste(sort(unique(source_status)), collapse = ","),
    population_data_status = paste(sort(unique(population_status)), collapse = ","),
    population_source_mode = bundle$diagnostics$population_source_mode %||%
      "not_applicable",
    incidence_status = status,
    provisional_denominator_year = provisional_year
  )
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
  packages <- c("shiny", "leaflet", "ggplot2", "plotly")
  packages[!vapply(packages, checker, logical(1L), quietly = TRUE)]
}

#' Run the regionalepi Shiny application
#'
#' Launches the installed interactive application. The bundled demographic
#' snapshot supplies typology indicators and annual-average population for
#' dynamic incidence by default. Optional live retrieval of both demographic
#' components uses the Regionaldatenbank credential environment variables;
#' SurvStat surveillance is fetched from the official live service.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return The value returned by `shiny::runApp()`, invisibly.
#' @export
run_regionalepi_app <- function(...) {
  missing <- .shiny_missing_dependencies()
  if (length(missing)) {
    stop(
      "The regionalepi Shiny application requires optional package(s): ",
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
