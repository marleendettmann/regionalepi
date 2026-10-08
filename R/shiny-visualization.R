.profile_description_spec <- function() list(
  specification_id = "dynamic_profile_descriptions_v2",
  mild_threshold = 0.25, moderate_threshold = 0.5, strong_threshold = 1.0,
  maximum_indicators = 3L, tie_method = "indicator_order",
  degree = c(mild = "leicht ", moderate = "", strong = "deutlich ")
)

.shiny_profile_descriptions <- function(profiles, mode = "dynamic") {
  if (identical(mode, "dissertation")) return(NULL)
  spec <- .profile_description_spec()
  display <- .shiny_app_indicator_display()
  rows <- split(profiles, profiles$display_cluster_id)
  descriptions <- vapply(rows, function(x) {
    x$order <- match(x$indicator_id, display$indicator_id)
    x <- x[order(-abs(x$standardized_center), x$order), , drop = FALSE]
    x <- x[abs(x$standardized_center) >= spec$mild_threshold, , drop = FALSE]
    if (!nrow(x)) return("durchschnittliches Profil")
    x <- utils::head(x, spec$maximum_indicators)
    parts <- vapply(seq_len(nrow(x)), function(i) {
      z <- x$standardized_center[[i]]
      strength <- if (abs(z) >= spec$strong_threshold) "strong" else
        if (abs(z) >= spec$moderate_threshold) "moderate" else "mild"
      adjective <- switch(x$indicator_id[[i]],
        population_density = if (z > 0) "h\u00f6here" else "niedrigere",
        mean_age = if (z > 0) "h\u00f6heres" else "niedrigeres",
        youth_dependency_ratio = if (z > 0) "h\u00f6herer" else "niedrigerer")
      paste0(spec$degree[[strength]], adjective, " ",
        display$label[match(x$indicator_id[[i]], display$indicator_id)])
    }, character(1L))
    paste(parts, collapse = ", ")
  }, character(1L))
  data.frame(display_cluster_id = names(descriptions),
             profile_description = unname(descriptions),
             description_specification_id = spec$specification_id,
             stringsAsFactors = FALSE)
}

.shiny_select_analysis_range <- function(window, mode, reviewed_period = NULL,
                                         custom_dates = NULL) {
  validate_observation_windows(window)
  if (nrow(window) != 1L || !mode %in% c("window", "reviewed", "custom"))
    stop("Analysis range requires one window and a supported mode.", call. = FALSE)
  if (mode == "reviewed") {
    if (is.null(reviewed_period) || nrow(reviewed_period) != 1L)
      stop("No reviewed epidemiological period is available for this window.", call. = FALSE)
    start <- reviewed_period$start_date; end <- reviewed_period$end_date
    label <- reviewed_period$label
  } else if (mode == "custom") {
    if (!inherits(custom_dates, "Date") || length(custom_dates) != 2L || anyNA(custom_dates))
      stop("Custom analysis range must contain two dates.", call. = FALSE)
    start <- custom_dates[[1L]]; end <- custom_dates[[2L]]
    if (!identical(start, .iso_week_monday(
          as.integer(format(start, "%G")), as.integer(format(start, "%V")))) ||
        !identical(end, .iso_week_monday(
          as.integer(format(end, "%G")), as.integer(format(end, "%V"))) + 6L)) {
      stop("Custom analysis ranges must use complete ISO calendar weeks.",
           call. = FALSE)
    }
    label <- "Benutzerdefinierter Analysezeitraum"
  } else {
    start <- window$start_date; end <- window$end_date
    label <- "Gesamter Beobachtungszeitraum"
  }
  if (start > end || start < window$start_date || end > window$end_date)
    stop(paste("Der benutzerdefinierte Zeitraum liegt au\u00dferhalb des",
               "gew\u00e4hlten Beobachtungszeitraums."), call. = FALSE)
  list(mode = mode, label = label, start_date = start, end_date = end,
       start_iso = .iso_week_label(start), end_iso = .iso_week_label(end))
}

.shiny_iso_week_display <- function(date) {
  sprintf("%s \u2013 KW %02d", format(date, "%G"), as.integer(format(date, "%V")))
}

.shiny_custom_week_choices <- function(window, as_of_date) {
  validate_observation_windows(window)
  if (nrow(window) != 1L) stop("Custom week choices require one window.", call. = FALSE)
  as_of_date <- .shiny_analysis_as_of_date(function() as_of_date)
  last <- window$end_date
  if (window$end_date > as_of_date) {
    if (as_of_date < window$start_date) return(stats::setNames(character(), character()))
    last <- .iso_week_monday(as.integer(format(as_of_date, "%G")),
                             as.integer(format(as_of_date, "%V")))
  }
  mondays <- seq(window$start_date, last, by = "week")
  stats::setNames(.iso_week_label(mondays), vapply(mondays,
    .shiny_iso_week_display, character(1L)))
}

.shiny_reconcile_custom_weeks <- function(window, start_week = NULL,
                                          end_week = NULL, as_of_date,
                                          changed = NULL) {
  choices <- .shiny_custom_week_choices(window, as_of_date)
  if (!length(choices)) stop("No ISO calendar week is available at the analysis cutoff.",
                             call. = FALSE)
  values <- unname(choices)
  start_valid <- length(start_week) == 1L && !is.na(start_week) &&
    start_week %in% values
  end_valid <- length(end_week) == 1L && !is.na(end_week) && end_week %in% values
  if (start_valid && end_valid) {
    start <- start_week
    end <- end_week
  } else {
    start <- values[[1L]]
    end <- utils::tail(values, 1L)
  }
  if (match(start, values) > match(end, values)) {
    if (identical(changed, "end")) start <- end else end <- start
  }
  list(choices = choices, start_week = start, end_week = end)
}

.shiny_select_custom_week_range <- function(window, start_week, end_week,
                                            as_of_date) {
  selection <- .shiny_reconcile_custom_weeks(
    window, start_week, end_week, as_of_date)
  values <- unname(selection$choices)
  mondays <- seq(window$start_date, by = "week", length.out = length(values))
  start <- mondays[[match(selection$start_week, values)]]
  end <- mondays[[match(selection$end_week, values)]] + 6L
  range <- .shiny_select_analysis_range(window, "custom",
    custom_dates = as.Date(c(start, end)))
  range$selected_start_iso <- selection$start_week
  range$selected_end_iso <- selection$end_week
  range
}

.shiny_analysis_as_of_date <- function(clock = Sys.Date) {
  value <- clock()
  if (!inherits(value, "Date") || length(value) != 1L || is.na(value)) {
    stop("Analysis cutoff must be one complete Date.", call. = FALSE)
  }
  value
}

.shiny_effective_analysis_range <- function(range, as_of_date) {
  as_of_date <- .shiny_analysis_as_of_date(function() as_of_date)
  if (as_of_date < range$start_date) {
    stop("Der gew\u00e4hlte Beobachtungszeitraum hat am Analyse-Stichtag noch nicht begonnen.",
         call. = FALSE)
  }
  effective <- range
  effective$nominal_start_date <- range$start_date
  effective$nominal_end_date <- range$end_date
  effective$analysis_as_of_date <- as_of_date
  effective$end_date <- min(range$end_date, as_of_date)
  effective$end_iso <- .iso_week_label(effective$end_date)
  effective$is_truncated <- effective$end_date < range$end_date
  effective
}

.shiny_early_window_note <- function(window, effective_range) {
  if (!is.data.frame(window) || nrow(window) != 1L ||
      !isTRUE(effective_range$is_truncated) ||
      is.null(effective_range$analysis_as_of_date)) return(NULL)
  elapsed_days <- as.numeric(difftime(
    effective_range$analysis_as_of_date, window$start_date, units = "days"))
  weeks <- as.integer(elapsed_days %/% 7L) + 1L
  if (weeks < 1L || weeks > 5L) return(NULL)
  label <- sub(" \\(laufend\\)$", "", window$label)
  unit <- if (weeks == 1L) "Kalenderwoche" else "Kalenderwochen"
  paste0(
    "Der Beobachtungszeitraum ", label, " hat in KW ",
    window$start_iso_week, " begonnen und umfasst bis zum Analyse-Stichtag erst ",
    weeks, " ", unit, ". Zeitliche Verl\u00e4ufe sind daher noch eingeschr\u00e4nkt ",
    "interpretierbar."
  )
}

.shiny_attach_typology_range <- function(bundle, fit, range) {
  data <- bundle$data
  data <- data[data$date >= range$start_date & data$date <= range$end_date, , drop = FALSE]
  if (!nrow(data)) {
    data$cluster_id <- character()
    data$typology_id <- character()
    data$typology_definition_version <- character()
    return(data)
  }
  pos <- match(data$geo_id, fit$assignments$geo_id)
  if (anyNA(pos)) stop("Typology does not cover the selected observations.", call. = FALSE)
  data$cluster_id <- fit$assignments$display_cluster_id[pos]
  data$typology_id <- fit$provenance$fit_id
  data$typology_definition_version <- fit$indicator_set$definition_version
  data
}

.shiny_pairwise_differences <- function(weekly, cluster_order) {
  cluster_order <- as.character(cluster_order)
  if (length(cluster_order) < 2L || anyDuplicated(cluster_order))
    stop("Pairwise comparisons require a unique cluster display order.",call.=FALSE)
  cell_key <- paste(weekly$date, as.character(weekly$cluster_id), sep="\r")
  if (anyDuplicated(cell_key))
    stop("Weekly cluster summaries must be unique by date and cluster.",call.=FALSE)
  groups <- split(seq_len(nrow(weekly)), weekly$date)
  rows <- lapply(groups,function(i) {
    x <- weekly[i,,drop=FALSE]
    if (!setequal(as.character(x$cluster_id),cluster_order))
      stop("Weekly cluster summaries do not contain every displayed cluster.",call.=FALSE)
    x <- x[match(cluster_order,as.character(x$cluster_id)),,drop=FALSE]
    pairs <- utils::combn(seq_along(cluster_order),2L)
    do.call(rbind,lapply(seq_len(ncol(pairs)),function(j) {
      a <- x[pairs[1L,j],,drop=FALSE]; b <- x[pairs[2L,j],,drop=FALSE]
      data.frame(date=a$date,cluster_a=as.character(a$cluster_id),
        cluster_b=as.character(b$cluster_id),
        pair_key=paste(a$cluster_id,b$cluster_id,sep="\r"),
        pair_label=paste(a$cluster_id,"\u2212",b$cluster_id),
        median_a=a$median_incidence,median_b=b$median_incidence,
        difference=a$median_incidence-b$median_incidence,
        stringsAsFactors=FALSE)
    }))
  })
  result <- do.call(rbind,rows); rownames(result)<-NULL
  key <- paste(result$date,result$pair_key,sep="\r")
  expected <- length(groups)*choose(length(cluster_order),2L)
  if(anyDuplicated(key)||nrow(result)!=expected)
    stop("Pairwise date and canonical pair keys must be unique and complete.",call.=FALSE)
  result
}

.shiny_exploration_summaries <- function(bundle, fit, range,
                                         display_metadata = NULL) {
  data <- .shiny_attach_typology_range(bundle, fit, range)
  if (!nrow(data)) stop(
    "F\u00fcr den gew\u00e4hlten Zeitraum liegen keine darstellbaren Beobachtungen vor.",
    call. = FALSE)
  expected_dates <- sort(unique(data$date))
  groups <- split(seq_len(nrow(data)), paste(data$cluster_id, data$date, sep = "\r"))
  weekly <- do.call(rbind, lapply(groups, function(i) {
    x <- data[i, , drop = FALSE]; incidence <- x$incidence; cases <- x$cases
    observed <- sum(!is.na(incidence))
    data.frame(date = x$date[[1L]], cluster_id = x$cluster_id[[1L]],
      median_incidence = if (observed) stats::median(incidence, na.rm = TRUE) else NA_real_,
      q1_incidence = if (observed) unname(stats::quantile(incidence, .25, na.rm = TRUE)) else NA_real_,
      q3_incidence = if (observed) unname(stats::quantile(incidence, .75, na.rm = TRUE)) else NA_real_,
      observed_districts = observed, expected_districts = nrow(x),
      missing_districts = sum(is.na(incidence)),
      observed_case_districts = sum(!is.na(cases)),
      reported_cases_observed = if (all(is.na(cases))) NA_real_ else sum(cases, na.rm = TRUE),
      stringsAsFactors = FALSE)
  }))
  all_week <- do.call(rbind, lapply(split(data$incidence, data$date), function(x) data.frame(
    all_district_median = if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE))))
  all_week$date <- as.Date(rownames(all_week)); rownames(all_week) <- NULL
  weekly$all_district_median <- all_week$all_district_median[match(weekly$date, all_week$date)]
  weekly$relative_activity <- weekly$median_incidence - weekly$all_district_median
  districts <- do.call(rbind, lapply(split(seq_len(nrow(data)), data$geo_id), function(i) {
    x <- data[i, , drop = FALSE]; incidence <- x$incidence; cases <- x$cases
    data.frame(geo_id = x$geo_id[[1L]], geo_name = x$geo_name[[1L]],
      cluster_id = x$cluster_id[[1L]],
      median_period_incidence = if (all(is.na(incidence))) NA_real_ else stats::median(incidence, na.rm = TRUE),
      cumulative_observed_cases = if (all(is.na(cases))) NA_real_ else sum(cases, na.rm = TRUE),
      observed_week_count = sum(!is.na(incidence)), expected_week_count = length(expected_dates),
      missing_week_count = sum(is.na(incidence)), observed_case_week_count = sum(!is.na(cases)),
      missing_case_week_count = sum(is.na(cases)), stringsAsFactors = FALSE)
  }))
  cluster_order <- if (is.null(display_metadata)) {
    if (identical(fit$mode,"dissertation")) c("ClD","ClJ","ClA") else
      sort(unique(weekly$cluster_id))
  } else display_metadata$display_cluster_id
  pairwise <- .shiny_pairwise_differences(weekly,cluster_order)
  list(data = data, weekly = weekly, district_period = districts,
       pairwise = pairwise, expected_dates = expected_dates)
}

.shiny_week_distribution <- function(data, selected_date, cluster_order) {
  .require_data_frame(data, "weekly district distribution")
  .require_columns(data, c("date", "geo_id", "geo_name", "cluster_id",
    "incidence", "cases"), "weekly district distribution")
  if (!inherits(selected_date, "Date") || length(selected_date) != 1L ||
      is.na(selected_date)) {
    stop("Weekly district distribution requires one complete Date.",
      call. = FALSE)
  }
  if (!is.character(cluster_order) || !length(cluster_order) ||
      anyNA(cluster_order) || any(!nzchar(cluster_order)) ||
      anyDuplicated(cluster_order)) {
    stop("Weekly district distribution requires unique cluster IDs.",
      call. = FALSE)
  }
  selected <- data[data$date == selected_date, , drop = FALSE]
  if (!nrow(selected)) {
    stop("The selected ISO week is not part of the loaded analysis.",
      call. = FALSE)
  }
  if (anyDuplicated(selected$geo_id)) {
    stop("Weekly district observations must be unique by geo_id.",
      call. = FALSE)
  }
  unknown <- setdiff(unique(as.character(selected$cluster_id)), cluster_order)
  if (length(unknown)) {
    stop("Weekly district observations contain an unknown cluster ID.",
      call. = FALSE)
  }
  selected$cluster_id <- factor(as.character(selected$cluster_id),
    levels = cluster_order)
  support <- do.call(rbind, lapply(cluster_order, function(cluster_id) {
    values <- selected$incidence[selected$cluster_id == cluster_id]
    data.frame(cluster_id = cluster_id,
      expected_districts = length(values),
      observed_districts = sum(!is.na(values)),
      missing_districts = sum(is.na(values)),
      zero_districts = sum(values == 0, na.rm = TRUE),
      stringsAsFactors = FALSE)
  }))
  selected$cluster_display <- paste0(as.character(selected$cluster_id),
    " (n=", support$observed_districts[
      match(as.character(selected$cluster_id), support$cluster_id)], ")")
  selected$cluster_display <- factor(selected$cluster_display,
    levels = paste0(cluster_order, " (n=",
      support$observed_districts[match(cluster_order, support$cluster_id)], ")"))
  list(date = selected_date, data = selected, support = support)
}

.shiny_state_composition_widget <- function(composition, display_metadata) {
  composition <- .shiny_apply_display_metadata(
    composition, display_metadata, "cluster_id"
  )
  name_col <- if ("comparison_name" %in% names(composition))
    "comparison_name" else "state_name"
  total_col <- if ("comparison_total_districts" %in% names(composition))
    "comparison_total_districts" else "state_total_districts"
  composition$comparison_name <- factor(
    composition[[name_col]], levels = rev(sort(unique(composition[[name_col]])))
  )
  composition$hover <- sprintf(
    paste0(
      "%s<br>Bundesweit bestimmter Regionaltyp: %s<br>Kreise im Cluster: %d",
      "<br>Kreise im Land: %d<br>Anteil der Kreise: %.1f%%"
    ),
    composition$comparison_name, composition$cluster_id,
    composition$n_districts, composition[[total_col]],
    100 * composition$district_fraction
  )
  colours <- stats::setNames(
    display_metadata$display_colour, display_metadata$display_cluster_id
  )
  graph <- ggplot2::ggplot(
    composition,
    ggplot2::aes(
      x = comparison_name, y = district_fraction, fill = cluster_id,
      text = hover
    )
  ) +
    ggplot2::geom_col(width = .78) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = colours, drop = FALSE) +
    ggplot2::scale_y_continuous(
      labels = function(value) paste0(round(100 * value), "%"),
      limits = c(0, 1), expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = NULL, y = "Anteil der Kreise", fill = "Demografischer Regionaltyp"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  plotly::ggplotly(graph, tooltip = "text", source = "state-composition")
}

.shiny_state_comparison_display <- function(summary, selected_cluster,
                                              crosswalk) {
  out <- .shiny_regional_comparison_display(
    summary, selected_cluster, crosswalk, "states")
  out$groups$state_id <- out$groups$comparison_id
  out$groups$state_name <- out$groups$comparison_name
  out$groups$state_display <- out$groups$comparison_display
  out$data$state_display <- out$data$comparison_display
  out
}

.shiny_regional_comparison_display <- function(
    summary, selected_cluster, crosswalk,
    comparison_level = c("grossregion", "aggregated_state", "state"),
    comparison_groups = .regional_comparison_groups()) {
  comparison_level <- comparison_level[[1L]]
  .check_scalar_nonempty_character(
    selected_cluster, "selected_cluster", "state comparison display"
  )
  validate_district_state_crosswalk(crosswalk)
  membership <- .regional_comparison_membership(
    crosswalk, comparison_level, comparison_groups)
  selected <- summary[summary$cluster_id == selected_cluster, , drop = FALSE]
  selected$comparison_id <- membership$comparison_id[
    match(selected$geo_id, membership$geo_id)]
  selected$comparison_name <- membership$comparison_name[
    match(selected$geo_id, membership$geo_id)]
  units <- unique(membership[c("comparison_id", "comparison_name")])
  counts <- table(factor(selected$comparison_id, levels = units$comparison_id))
  groups <- data.frame(
    comparison_id = units$comparison_id,
    comparison_name = units$comparison_name,
    n_districts = as.integer(counts), stringsAsFactors = FALSE
  )
  groups$display_rule <- ifelse(
    groups$n_districts == 0L, "unavailable",
    ifelse(groups$n_districts == 1L, "point_only",
      ifelse(groups$n_districts <= 4L, "points_and_median",
             "points_and_boxplot"))
  )
  groups$comparison_display <- paste0(
    groups$comparison_name, " (n=", groups$n_districts, ")")
  selected$comparison_display <- groups$comparison_display[
    match(selected$comparison_id, groups$comparison_id)]
  list(data = selected, groups = groups, comparison_level = comparison_level)
}

.shiny_state_comparison_widget <- function(display, colour) {
  states <- display$groups$comparison_display
  data <- display$data
  data$comparison_display <- factor(
    data$comparison_display, levels = rev(states))
  data$hover <- sprintf(
    paste0(
      "%s<br>AGS: %s<br>Bundesland: %s",
      "<br>Bundesweit bestimmter Regionaltyp: %s",
      "<br>Median der w\u00f6chentlichen Kreisinzidenz: %s",
      "<br>Gemeldete F\u00e4lle: %s<br>Beobachtete/erwartete Wochen: %d/%d"
    ),
    data$geo_name, data$geo_id, data$state_name, data$cluster_id,
    ifelse(is.na(data$period_median_incidence), "fehlend",
           sprintf("%.2f", data$period_median_incidence)),
    ifelse(is.na(data$cumulative_reported_cases), "fehlend",
           format(data$cumulative_reported_cases, scientific = FALSE, trim = TRUE)),
    data$observed_weeks, data$expected_weeks
  )
  graph <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = comparison_display, y = period_median_incidence,
      text = hover, key = geo_id
    )
  ) + ggplot2::labs(
      x = NULL, y = "Median der w\u00f6chentlichen Kreisinzidenz"
    ) +
    ggplot2::coord_flip() + ggplot2::theme_minimal()
  box_states <- display$groups$comparison_id[
    display$groups$display_rule == "points_and_boxplot"
  ]
  box_data <- data[data$comparison_id %in% box_states, , drop = FALSE]
  if (nrow(box_data)) graph <- graph + ggplot2::geom_boxplot(
    data = box_data,
    ggplot2::aes(
      x = comparison_display, y = period_median_incidence,
      group = comparison_display
    ),
    inherit.aes = FALSE,
    width = .48, outlier.shape = NA,
    fill = "white", alpha = .72, colour = colour,
    linewidth = .75, na.rm = TRUE
  )
  median_states <- display$groups$comparison_id[
    display$groups$display_rule == "points_and_median"
  ]
  median_data <- data[data$comparison_id %in% median_states, , drop = FALSE]
  if (nrow(median_data)) {
    medians <- stats::aggregate(
      median_data$period_median_incidence,
      median_data[c("comparison_id", "comparison_display")],
      function(value) if (all(is.na(value))) NA_real_ else stats::median(value, na.rm = TRUE)
    )
    names(medians)[[3L]] <- "period_median_incidence"
    graph <- graph + ggplot2::geom_point(
      data = medians,
      ggplot2::aes(x = comparison_display, y = period_median_incidence),
      inherit.aes = FALSE, shape = 23, size = 3, fill = "white",
      colour = colour, stroke = 1, na.rm = TRUE
    )
  }
  graph <- graph + suppressWarnings(ggplot2::geom_jitter(
    width = .10, height = 0, colour = colour, alpha = .72, size = 1.8,
    na.rm = TRUE
  ))
  plotly::ggplotly(graph, tooltip = "text", source = "state-comparison")
}

.shiny_finite_n_display <- function(values) {
  if (!is.numeric(values)) stop("Adaptive display values must be numeric.", call. = FALSE)
  n <- sum(is.finite(values))
  rule <- if (n == 0L) "unavailable" else if (n == 1L) "point_only" else if (
    n <= 4L) "points_and_median" else if (n <= 9L) "points_and_boxplot" else
      "points_boxplot_and_violin"
  list(
    n_finite = as.integer(n), display_rule = rule,
    show_points = n > 0L, show_median = n >= 2L && n <= 4L,
    show_boxplot = n >= 5L, show_violin = n >= 10L
  )
}

.shiny_adaptive_distribution_layers <- function(data, group_cols, value_col) {
  .require_data_frame(data, "adaptive distribution display")
  .require_columns(data, c(group_cols, value_col), "adaptive distribution display")
  if (!is.numeric(data[[value_col]])) {
    stop("Adaptive distribution values must be numeric.", call. = FALSE)
  }
  key <- interaction(data[group_cols], drop = TRUE, lex.order = TRUE)
  indices <- split(seq_len(nrow(data)), key, drop = TRUE)
  rules <- lapply(indices, function(index) {
    classification <- .shiny_finite_n_display(data[[value_col]][index])
    row <- data[index[[1L]], group_cols, drop = FALSE]
    row$n_finite <- classification$n_finite
    row$display_rule <- classification$display_rule
    row
  })
  rules <- if (length(rules)) do.call(rbind, rules) else {
    output <- data[FALSE, group_cols, drop = FALSE]
    output$n_finite <- integer()
    output$display_rule <- character()
    output
  }
  rownames(rules) <- NULL
  row_key <- interaction(data[group_cols], drop = TRUE, lex.order = TRUE)
  rule_key <- interaction(rules[group_cols], drop = TRUE, lex.order = TRUE)
  display_rule <- rules$display_rule[match(row_key, rule_key)]
  finite <- is.finite(data[[value_col]])
  select <- function(allowed) data[finite & display_rule %in% allowed, , drop = FALSE]
  median_rows <- rules[rules$display_rule == "points_and_median", group_cols,
                       drop = FALSE]
  if (nrow(median_rows)) {
    median_key <- interaction(median_rows[group_cols], drop = TRUE,
                              lex.order = TRUE)
    median_rows$.display_median <- vapply(seq_len(nrow(median_rows)),
      function(index) stats::median(data[[value_col]][
        finite & as.character(row_key) == as.character(median_key[index])]),
      numeric(1))
  } else median_rows$.display_median <- numeric()
  list(
    rules = rules,
    points = data[finite, , drop = FALSE],
    medians = median_rows,
    boxplots = select(c("points_and_boxplot", "points_boxplot_and_violin")),
    violins = select("points_boxplot_and_violin")
  )
}

.shiny_weekly_heatmap_xgap <- function(n_weeks) {
  if (length(n_weeks) != 1L || is.na(n_weeks) || !is.numeric(n_weeks) ||
      !is.finite(n_weeks) || n_weeks < 0 || n_weeks != floor(n_weeks)) {
    stop("Displayed week count must be one non-negative whole number.", call. = FALSE)
  }
  if (n_weeks <= 60L) 1 else 0
}

.shiny_signed_heatmap_colours <- function() {
  c("#5F627B", "#F7F7F7", "#C86600")
}

.shiny_incidence_heatmap_colours <- function() {
  c("#F7F7F9", "#DADAE4", "#9C9EB5", "#5F627B", "#DD7F02")
}

.shiny_heatmap_grid <- function(data, row, column, value, text = NULL,
                                custom = NULL) {
  rows <- unique(as.character(data[[row]]))
  columns <- sort(unique(data[[column]]))
  z <- matrix(NA_real_, nrow = length(rows), ncol = length(columns),
              dimnames = list(rows, as.character(columns)))
  text_matrix <- matrix("", nrow = length(rows), ncol = length(columns))
  custom_matrix <- matrix(NA_character_, nrow = length(rows), ncol = length(columns))
  ri <- match(as.character(data[[row]]), rows)
  ci <- match(data[[column]], columns)
  key <- paste(ri, ci, sep = "\r")
  if (anyDuplicated(key)) stop("Heatmap cells must be unique.", call. = FALSE)
  z[cbind(ri, ci)] <- data[[value]]
  if (!is.null(text)) text_matrix[cbind(ri, ci)] <- data[[text]]
  if (!is.null(custom)) custom_matrix[cbind(ri, ci)] <- data[[custom]]
  list(rows = rows, columns = columns, z = z, text = text_matrix,
       custom = custom_matrix)
}

.shiny_regional_temporal_heatmap_data <- function(data, display_metadata) {
  required <- c(
    "date", "cluster_id", "median_incidence", "regional_reference_median",
    "regional_relative_activity", "observed_districts",
    "expected_districts", "missing_districts", "completeness",
    "regional_observed_districts", "regional_expected_districts",
    "regional_missing_districts", "regional_completeness"
  )
  .require_data_frame(data, "regional relative-activity heatmap")
  .require_columns(data, required, "regional relative-activity heatmap")
  ids <- display_metadata$display_cluster_id
  represented <- ids[ids %in% unique(as.character(
    data$cluster_id[data$expected_districts > 0L]))]
  dates <- sort(unique(data$date))
  selected <- data[data$expected_districts > 0L &
    as.character(data$cluster_id) %in% represented, , drop = FALSE]
  key <- paste(selected$date, selected$cluster_id, sep = "\r")
  expected <- as.vector(outer(as.character(dates), represented, paste,
                              sep = "\r"))
  if (anyDuplicated(key) || !setequal(key, expected)) {
    stop("Regional relative-activity cells must form one complete unique cluster-by-week grid.",
         call. = FALSE)
  }
  selected$cluster_id <- factor(as.character(selected$cluster_id),
    levels = represented, ordered = TRUE)
  selected <- selected[order(selected$cluster_id, selected$date), , drop = FALSE]
  profile <- display_metadata$profile_description[
    match(as.character(selected$cluster_id), display_metadata$display_cluster_id)]
  label <- display_metadata$display_label[
    match(as.character(selected$cluster_id), display_metadata$display_cluster_id)]
  observed <- !is.na(selected$regional_relative_activity)
  selected$hover <- ifelse(observed,
    sprintf(paste0("%s<br>Cluster: %s \u00b7 %s<br>%s<br>",
      "Cluster-Wochenmedian: %.2f<br>Median aller Kreise der Region: %.2f<br>",
      "Relative Aktivit\u00e4t: %+.2f<br>",
      "Cluster-Kreise beobachtet/erwartet/fehlend: %d/%d/%d (%.1f %%)<br>",
      "Region gesamt beobachtet/erwartet/fehlend: %d/%d/%d (%.1f %%)"),
      .iso_week_label(selected$date), as.character(selected$cluster_id), label,
      profile, selected$median_incidence, selected$regional_reference_median,
      selected$regional_relative_activity,
      selected$observed_districts, selected$expected_districts,
      selected$missing_districts, 100 * selected$completeness,
      selected$regional_observed_districts,
      selected$regional_expected_districts,
      selected$regional_missing_districts,
      100 * selected$regional_completeness),
    sprintf(paste0("%s<br>Cluster: %s \u00b7 %s<br>%s<br>",
      "Keine beobachtbare relative Aktivit\u00e4t<br>",
      "Cluster-Kreise beobachtet/erwartet/fehlend: %d/%d/%d (%.1f %%)<br>",
      "Region gesamt beobachtet/erwartet/fehlend: %d/%d/%d (%.1f %%)"),
      .iso_week_label(selected$date), as.character(selected$cluster_id), label,
      profile, selected$observed_districts, selected$expected_districts,
      selected$missing_districts, 100 * selected$completeness,
      selected$regional_observed_districts,
      selected$regional_expected_districts,
      selected$regional_missing_districts,
      100 * selected$regional_completeness))
  grid <- .shiny_heatmap_grid(selected, "cluster_id", "date",
    "regional_relative_activity", "hover")
  grid$rows <- represented
  grid$data <- selected
  grid
}

.shiny_regional_context_styles <- function(region_names, focal_name) {
  region_names <- unique(as.character(region_names))
  if (length(focal_name) != 1L || is.na(focal_name) ||
      !focal_name %in% region_names || !"Deutschland" %in% region_names) {
    stop("Regional context styles require one focal region and Germany.",
         call. = FALSE)
  }
  comparisons <- setdiff(region_names, c(focal_name, "Deutschland"))
  if (length(comparisons) > 3L) {
    stop("Regional context supports at most three comparison regions.",
         call. = FALSE)
  }
  comparison_colours <- c("#4E6478", "#7A8D9E", "#A5B0BA")
  comparison_types <- c("solid", "dashed", "dotdash")
  data.frame(
    comparison_name = c(focal_name, comparisons, "Deutschland"),
    colour = c("#B3261E", comparison_colours[seq_along(comparisons)],
               "#1F2937"),
    linetype = c("solid", comparison_types[seq_along(comparisons)],
                 "longdash"),
    linewidth = c(1.10, rep(.72, length(comparisons)), .88),
    stringsAsFactors = FALSE
  )
}

.shiny_pairwise_heatmap_widget <- function(pairwise, display_metadata,
                                            reviewed_period = NULL) {
  ids <- display_metadata$display_cluster_id
  pair_order <- apply(utils::combn(ids, 2L), 2L, paste, collapse = "\r")
  pairwise$pair_key <- factor(pairwise$pair_key, levels = pair_order,
                              ordered = TRUE)
  pairwise <- pairwise[order(pairwise$pair_key, pairwise$date), ]
  pairwise$hover <- sprintf(
    "%s<br>%s<br>Median A: %.2f<br>Median B: %.2f<br>Differenz: %.2f",
    pairwise$pair_label, .iso_week_label(pairwise$date), pairwise$median_a,
    pairwise$median_b, pairwise$difference)
  grid <- .shiny_heatmap_grid(
    pairwise, "pair_key", "date", "difference", "hover")
  labels <- unique(pairwise[c("pair_key", "pair_label")])
  shapes <- list()
  if (!is.null(reviewed_period) && nrow(reviewed_period) == 1L) {
    shapes <- lapply(
      c(reviewed_period$start_date, reviewed_period$end_date),
      function(day) list(
        type = "line", xref = "x", yref = "paper",
        x0 = format(day), x1 = format(day), y0 = 0, y1 = 1,
        line = list(color = "#4B5563", width = 1, dash = "dot")
      )
    )
  }
  plotly::plot_ly(
    x = grid$columns,
    y = labels$pair_label[match(grid$rows, as.character(labels$pair_key))],
    z = grid$z, type = "heatmap", zmid = 0,
    colors = .shiny_signed_heatmap_colours(), text = grid$text,
    hoverinfo = "text", source = "pairwise", connectgaps = FALSE,
    xgap = .shiny_weekly_heatmap_xgap(length(grid$columns)), ygap = 1
  ) |>
    plotly::layout(
      xaxis = list(title = "", rangeslider = list(visible = TRUE)),
      yaxis = list(title = ""), shapes = shapes,
      plot_bgcolor = "#D1D5DB")
}

.shiny_district_heatmap_layout <- function(data, selected_geo_id = NULL) {
  cluster_order <- if (is.factor(data$cluster_id)) levels(data$cluster_id) else
    sort(unique(data$cluster_id))
  ordered <- unique(data[, c("geo_id", "geo_name", "cluster_id")])
  ordered$display_order <- match(as.character(ordered$cluster_id), cluster_order)
  ordered <- ordered[order(ordered$display_order, ordered$geo_name,
                           ordered$geo_id), , drop = FALSE]
  ordered$cluster_id <- as.character(ordered$cluster_id)
  ordered$row_position <- seq_len(nrow(ordered))
  boundaries <- which(ordered$cluster_id[-nrow(ordered)] !=
                      ordered$cluster_id[-1L]) + .5
  selected <- ordered$row_position[ordered$geo_id == selected_geo_id]
  list(rows = ordered, boundaries = boundaries,
       selected_position = if (length(selected)) selected[[1L]] else NULL)
}

.shiny_cluster_display_metadata <- function(fit, mode = fit$mode,
                                            colour_variant = "neutral",
                                            colour_policy = NULL,
                                            display_geo_ids = NULL) {
  ids <- unique(as.character(fit$assignments$display_cluster_id))
  order_ids <- if (identical(mode, "dissertation")) c("ClD", "ClJ", "ClA") else
    sort(ids)
  if (!setequal(ids, order_ids))
    stop("Display metadata does not cover the fitted cluster identities.", call. = FALSE)
  colours <- .shiny_app_cluster_colours(order_ids, mode, colour_variant,
                                         colour_policy)
  descriptions <- .shiny_profile_descriptions(fit$profiles, mode)
  historical_labels <- c(ClD = "dichte Regionen",
                         ClJ = "familiengepr\u00e4gte Regionen",
                         ClA = "\u00e4ltere, l\u00e4ndliche Regionen")
  raw <- unique(fit$assignments[c("display_cluster_id", "raw_cluster")])
  selected <- fit$assignments
  if (!is.null(display_geo_ids)) selected <- selected[
    selected$geo_id %in% display_geo_ids, , drop = FALSE]
  counts <- table(factor(selected$display_cluster_id, levels = order_ids))
  anchors <- stats::setNames(rep(NA_character_, length(order_ids)), order_ids)
  if (identical(mode, "dynamic") && !is.null(colour_policy$anchor_alignment)) {
    continuation <- colour_policy$continuation
    for (anchor_id in names(continuation)) {
      target <- continuation[[anchor_id]]
      anchors[[target]] <- c(ClD = "dense", ClJ = "family_youth",
                              ClA = "older_low_density")[[
        colour_policy$anchor_alignment$mapping[[anchor_id]]]]
    }
    branches <- colour_policy$branch_alignment
    if (!is.null(branches) && length(branches)) {
      for (target in names(branches)) {
        anchors[[target]] <- paste0(c(
          ClD = "dense", ClJ = "family_youth", ClA = "older_low_density"
        )[[colour_policy$anchor_alignment$mapping[[branches[[target]]]]]],
        "_branch")
      }
    }
  }
  labels <- if (identical(mode, "dissertation"))
    paste0(order_ids, " \u00b7 ", historical_labels[order_ids]) else order_ids
  profile <- if (identical(mode, "dissertation"))
    unname(historical_labels[order_ids]) else descriptions$profile_description[
      match(order_ids, descriptions$display_cluster_id)]
  data.frame(
    fit_id = fit$provenance$fit_id,
    display_cluster_id = order_ids,
    raw_cluster_id = raw$raw_cluster[match(order_ids, raw$display_cluster_id)],
    profile_anchor = unname(anchors[order_ids]),
    display_label = unname(labels), profile_description = unname(profile),
    display_colour = unname(colours[order_ids]),
    display_order = seq_along(order_ids), n_districts = as.integer(counts),
    mode = mode,
    historical_code = if (identical(mode, "dissertation")) order_ids else NA_character_,
    historical_label = if (identical(mode, "dissertation"))
      unname(historical_labels[order_ids]) else NA_character_,
    stringsAsFactors = FALSE
  )
}

.shiny_apply_display_metadata <- function(data, metadata,
                                          cluster_col = "cluster_id") {
  ids <- as.character(data[[cluster_col]])
  position <- match(ids, metadata$display_cluster_id)
  if (anyNA(position)) stop("Display metadata is incomplete.", call. = FALSE)
  data[[cluster_col]] <- factor(ids, levels = metadata$display_cluster_id,
                                ordered = TRUE)
  data$display_colour <- metadata$display_colour[position]
  data$display_label <- metadata$display_label[position]
  data$display_order <- metadata$display_order[position]
  data
}

.shiny_regional_type_choices <- function(metadata) {
  description <- sub("[,;].*$", "", metadata$profile_description)
  label <- ifelse(
    metadata$mode == "dynamic",
    paste0(metadata$display_cluster_id, " \u2013 ", description),
    metadata$display_label
  )
  stats::setNames(metadata$display_cluster_id, label)
}

.shiny_district_heatmap_data <- function(data, metadata,
                                         selected_geo_id = NULL) {
  required <- c("geo_id", "geo_name", "cluster_id", "date", "incidence", "cases")
  if (!all(required %in% names(data)))
    stop("District heatmap data are incomplete.", call. = FALSE)
  key <- paste(data$geo_id, as.character(data$date), sep = "\r")
  if (anyDuplicated(key)) stop("District-week cells must be unique.", call. = FALSE)
  data <- .shiny_apply_display_metadata(data, metadata)
  layout <- .shiny_district_heatmap_layout(data, selected_geo_id)
  layout$rows$display_colour <- metadata$display_colour[
    match(layout$rows$cluster_id, metadata$display_cluster_id)]
  layout$rows$display_label <- metadata$display_label[
    match(layout$rows$cluster_id, metadata$display_cluster_id)]
  data$row_position <- layout$rows$row_position[
    match(data$geo_id, layout$rows$geo_id)]
  data <- data[order(data$row_position, data$date), , drop = FALSE]
  data$cell_key <- paste(data$geo_id, as.character(data$date), sep = "|")
  data$hover <- sprintf(
    "%s<br>AGS: %s<br>Cluster: %s<br>%s<br>Inzidenz: %s<br>Gemeldete F\u00e4lle: %s",
    data$geo_name, data$geo_id, as.character(data$cluster_id),
    .iso_week_label(data$date),
    ifelse(is.na(data$incidence), "Keine beobachtete Inzidenz",
           round(data$incidence, 2)),
    ifelse(is.na(data$cases), "fehlend", data$cases))
  incidence <- .shiny_heatmap_grid(data, "row_position", "date", "incidence",
                                    "hover", "cell_key")
  cases <- .shiny_heatmap_grid(data, "row_position", "date", "cases")$z
  missing <- is.na(incidence$z)
  weeks <- data.frame(column_position = seq_along(incidence$columns),
    date = as.Date(incidence$columns), stringsAsFactors = FALSE)
  week_rows <- unique(data[c("date", "reporting_year", "reporting_week")])
  weeks$reporting_year <- week_rows$reporting_year[match(weeks$date, week_rows$date)]
  weeks$reporting_week <- week_rows$reporting_week[match(weeks$date, week_rows$date)]
  blocks <- do.call(rbind, lapply(metadata$display_cluster_id, function(id) {
    positions <- layout$rows$row_position[layout$rows$cluster_id == id]
    if (!length(positions)) return(NULL)
    data.frame(cluster_id = id, first_row = min(positions), last_row = max(positions),
      n = length(positions), colour = metadata$display_colour[
        metadata$display_cluster_id == id], stringsAsFactors = FALSE)
  }))
  result <- list(data = data, districts = layout$rows, weeks = weeks,
       incidence = incidence$z, cases = cases, missing = missing,
       hover = incidence$text, customdata = incidence$custom,
       boundaries = layout$boundaries, blocks = blocks,
       selected_position = layout$selected_position)
  if (!identical(dim(result$incidence), c(nrow(result$districts), nrow(result$weeks))) ||
      !identical(dim(result$cases), dim(result$incidence)) ||
      !identical(dim(result$missing), dim(result$incidence)) ||
      !identical(rownames(result$incidence), as.character(result$districts$row_position)) ||
      !identical(colnames(result$incidence), as.character(result$weeks$date)) ||
      any(result$blocks$last_row - result$blocks$first_row + 1L != result$blocks$n) ||
      sum(result$blocks$n) != nrow(result$districts))
    stop("District heatmap matrix invariants failed.", call. = FALSE)
  matrix_position <- cbind(data$row_position, match(data$date, result$weeks$date))
  if (!isTRUE(all.equal(result$incidence[matrix_position], data$incidence,
                        check.attributes = FALSE)) ||
      !isTRUE(all.equal(result$cases[matrix_position], data$cases,
                        check.attributes = FALSE)) ||
      !identical(result$missing[matrix_position], is.na(data$incidence)))
    stop("District heatmap cells do not match the analytical data.", call. = FALSE)
  result
}

.shiny_district_heatmap_widget <- function(audit, source = "district-heat") {
  x <- audit$weeks$date; y <- audit$districts$row_position
  xgap <- .shiny_weekly_heatmap_xgap(nrow(audit$weeks))
  figure <- plotly::plot_ly(x = x, y = y, z = audit$incidence,
    type = "heatmap", colors = .shiny_incidence_heatmap_colours(),
    text = audit$hover, hoverinfo = "text", customdata = audit$customdata,
    source = source, connectgaps = FALSE, xgap = xgap, ygap = 0,
    colorbar = list(title = "Inzidenz"))
  if (any(audit$missing)) figure <- plotly::add_trace(figure, x = x, y = y,
    z = ifelse(audit$missing, 1, NA_real_), type = "heatmap",
    colorscale = list(c(0, "#D9D9D9"), c(1, "#D9D9D9")),
    zmin = 0, zmax = 1, showscale = FALSE, text = audit$hover,
    customdata = audit$customdata, hoverinfo = "text", connectgaps = FALSE,
    xgap = xgap, ygap = 0,
    inherit = FALSE)
  strip <- plotly::plot_ly(x = rep("Cluster", nrow(audit$districts)), y = y,
    type = "scatter", mode = "markers", hoverinfo = "text",
    text = paste("Cluster", audit$districts$cluster_id),
    marker = list(symbol = "square", size = 8,
      color = audit$districts$display_colour, line = list(width = 0)),
    showlegend = FALSE, source = source)
  combined <- plotly::subplot(strip, figure, widths = c(.045, .955),
                              shareY = TRUE, titleX = FALSE, margin = .01)
  shapes <- lapply(audit$boundaries, function(position) list(type = "line",
    xref = "paper", yref = "y", x0 = 0, x1 = 1, y0 = position,
    y1 = position, line = list(color = "#FFFFFF", width = 1.5)))
  if (!is.null(audit$selected_position)) shapes <- c(shapes, list(list(
    type = "line", xref = "paper", yref = "y", x0 = 0, x1 = 1,
    y0 = audit$selected_position, y1 = audit$selected_position,
    line = list(color = "#111111", width = 2))))
  annotations <- lapply(seq_len(nrow(audit$blocks)), function(i) list(
    xref = "paper", yref = "y", x = .018,
    y = mean(c(audit$blocks$first_row[[i]], audit$blocks$last_row[[i]])),
    text = audit$blocks$cluster_id[[i]], showarrow = FALSE, textangle = -90,
    font = list(size = 10, color = "#FFFFFF")))
  widget <- plotly::layout(combined, plot_bgcolor = "#D9D9D9", shapes = shapes,
    annotations = annotations, xaxis = list(title = ""),
    xaxis2 = list(title = "", rangeslider = list(visible = TRUE)),
    yaxis = list(showticklabels = FALSE,
      title = paste(nrow(audit$districts), "Kreise, nach Cluster gruppiert")))
  widget <- plotly::plotly_build(widget)
  heatmaps <- which(vapply(widget$x$data, function(trace)
    identical(trace$type, "heatmap"), logical(1L)))
  if (!length(heatmaps)) stop("District heatmap widget has no incidence trace.",
                              call. = FALSE)
  widget$x$data[[heatmaps[[1L]]]]$customdata <- audit$customdata
  if (length(heatmaps) > 1L)
    widget$x$data[[heatmaps[[2L]]]]$customdata <- audit$customdata
  widget
}

.shiny_format_number <- function(x, digits = 0L) {
  ifelse(is.na(x), "fehlend", format(round(x, digits), nsmall = digits,
    big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE))
}

.shiny_compare_partitions <- function(coarser_fit, finer_fit) {
  ids <- intersect(coarser_fit$assignments$geo_id,finer_fit$assignments$geo_id)
  if (length(ids) != nrow(coarser_fit$assignments) ||
      length(ids) != nrow(finer_fit$assignments))
    stop("Partition comparison requires the same geography set.",call.=FALSE)
  source <- coarser_fit$assignments$display_cluster_id[
    match(ids,coarser_fit$assignments$geo_id)]
  target <- finer_fit$assignments$display_cluster_id[
    match(ids,finer_fit$assignments$geo_id)]
  tab <- table(source,target)
  choose2 <- function(x)x*(x-1)/2
  observed <- sum(choose2(tab)); row_pairs <- sum(choose2(rowSums(tab)))
  column_pairs <- sum(choose2(colSums(tab))); all_pairs <- choose2(sum(tab))
  expected <- row_pairs*column_pairs/all_pairs
  ari <- (observed-expected)/((row_pairs+column_pairs)/2-expected)
  source_centers <- stats::xtabs(standardized_center~display_cluster_id+indicator_id,
                          coarser_fit$profiles)
  target_centers <- stats::xtabs(standardized_center~display_cluster_id+indicator_id,
                          finer_fit$profiles)
  distances <- outer(seq_len(nrow(source_centers)),seq_len(nrow(target_centers)),
    Vectorize(function(i,j)sqrt(sum((source_centers[i,]-target_centers[j,])^2))))
  dimnames(distances)<-list(rownames(source_centers),rownames(target_centers))
  transition <- as.data.frame(tab,stringsAsFactors=FALSE)
  names(transition)<-c("source_cluster","target_cluster","n_shared")
  transition$source_k<-nrow(source_centers);transition$target_k<-nrow(target_centers)
  transition$source_fraction<-transition$n_shared/rowSums(tab)[transition$source_cluster]
  transition$target_fraction<-transition$n_shared/colSums(tab)[transition$target_cluster]
  transition$source_center_distance<-distances[cbind(
    match(transition$source_cluster,rownames(distances)),
    match(transition$target_cluster,colnames(distances)))]
  transition<-transition[c("source_k","source_cluster","target_k","target_cluster",
    "n_shared","source_fraction","target_fraction","source_center_distance")]
  list(contingency=tab,source_proportions=prop.table(tab,1L),ari=unname(ari),
       center_distances=distances,transition=transition)
}

.shiny_typology_transition <- function(reference_fit, target_fit) {
  if (anyDuplicated(reference_fit$assignments$geo_id) ||
      anyDuplicated(target_fit$assignments$geo_id))
    stop("Typology assignments must contain unique geo_id values.",call.=FALSE)
  comparison <- .shiny_compare_partitions(reference_fit,target_fit)
  table <- comparison$transition
  names(table)[names(table)=="source_cluster"] <- "reference_cluster"
  names(table)[names(table)=="target_cluster"] <- "target_cluster"
  names(table)[names(table)=="source_fraction"] <- "reference_fraction"
  names(table)[names(table)=="source_center_distance"] <-
    "reference_center_distance"
  reference_sizes <- table(reference_fit$assignments$display_cluster_id)
  target_sizes <- table(target_fit$assignments$display_cluster_id)
  table$reference_fit_id <- reference_fit$provenance$fit_id
  table$target_fit_id <- target_fit$provenance$fit_id
  table$reference_n <- as.integer(reference_sizes[table$reference_cluster])
  table$target_n <- as.integer(target_sizes[table$target_cluster])
  table$target_k <- length(target_sizes)
  table <- table[c("reference_fit_id","reference_cluster","target_fit_id",
    "target_cluster","n_shared","reference_n","target_n",
    "reference_fraction","target_fraction","reference_center_distance",
    "target_k")]
  dominant <- stats::aggregate(n_shared~target_cluster,table,max)
  list(data=table,ari=comparison$ari,
    dominant_ancestry_districts=sum(dominant$n_shared),
    target_sizes=target_sizes,reference_sizes=reference_sizes)
}

.shiny_stability_interpretation <- function(stability) {
  comparison <- stability$comparisons
  dominant <- function(k) {
    x <- comparison[[as.character(k)]]$data
    stats::aggregate(target_fraction ~ target_cluster, x, max)
  }
  k2 <- dominant(2L)
  k4 <- dominant(4L)
  k5 <- dominant(5L)
  paste(
    paste0("k=2 ist eine grobe Zweiteilung; die dominante k=3-Herkunft umfasst je Zielcluster ",
      paste0(round(100*k2$target_fraction), "%", collapse = " und "), "."),
    paste0("Bei k=4 liegen die dominanten Herkunftsanteile zwischen ",
      round(100*min(k4$target_fraction)), "% und ",
      round(100*max(k4$target_fraction)), "%; \u00dcberlappung bleibt das prim\u00e4re Kontinuit\u00e4tskriterium."),
    paste0("k=5 zeigt Anteile zwischen ", round(100*min(k5$target_fraction)),
      "% und ", round(100*max(k5$target_fraction)),
      "% und ist daher als m\u00f6gliche Reorganisation, nicht als vorgegebene Hierarchie, zu lesen."),
    sep = " "
  )
}

.shiny_typology_stability <- function(fits) {
  keys <- as.character(vapply(fits,function(fit)
    length(unique(fit$assignments$display_cluster_id)),integer(1L)))
  names(fits) <- keys
  if (!all(c("2","3","4","5")%in%names(fits)))
    stop("Typology stability requires fits for k = 2 through 5.",call.=FALSE)
  comparisons <- lapply(c("2","4","5"),function(k)
    .shiny_typology_transition(fits[["3"]],fits[[k]]))
  names(comparisons)<-c("2","4","5")
  summary <- do.call(rbind,lapply(names(comparisons),function(k){x<-comparisons[[k]]
    data.frame(target_k=as.integer(k),ari=x$ari,
      dominant_ancestry_districts=x$dominant_ancestry_districts,
      target_cluster_sizes=paste(names(x$target_sizes),as.integer(x$target_sizes),
        collapse="; "),stringsAsFactors=FALSE)}))
  transitions <- do.call(rbind,lapply(comparisons,`[[`,"data"));rownames(transitions)<-NULL
  list(reference_k=3L,summary=summary,transitions=transitions,
       comparisons=comparisons)
}

.shiny_stability_alluvial_widget <- function(stability, display_metadata) {
  if (!identical(stability$reference_k, 3L)) {
    stop("Stability alluvials require the reviewed k=3 reference.", call. = FALSE)
  }
  source_ids <- display_metadata$display_cluster_id
  source_colours <- stats::setNames(
    display_metadata$display_colour, display_metadata$display_cluster_id
  )
  rgba <- function(colour, alpha = .52) {
    rgb <- grDevices::col2rgb(colour)
    sprintf("rgba(%d,%d,%d,%.2f)", rgb[1L, ], rgb[2L, ], rgb[3L, ], alpha)
  }
  trace <- function(target_k, x_domain, y_domain) {
    x <- stability$transitions[
      stability$transitions$target_k == target_k &
        stability$transitions$n_shared > 0L, , drop = FALSE
    ]
    target_ids <- sort(unique(as.character(x$target_cluster)))
    nodes <- c(
      paste0("k=3 \u00b7 ", source_ids),
      paste0("k=", target_k, " \u00b7 ", target_ids)
    )
    source <- match(as.character(x$reference_cluster), source_ids) - 1L
    target <- length(source_ids) +
      match(as.character(x$target_cluster), target_ids) - 1L
    list(
      type = "sankey", orientation = "h", arrangement = "snap",
      domain = list(x = x_domain, y = y_domain),
      node = list(
        label = nodes, pad = 12, thickness = 16,
        color = c(unname(source_colours[source_ids]),
          rep("#AEB7C2", length(target_ids)))
      ),
      link = list(
        source = source, target = target, value = x$n_shared,
        color = unname(rgba(source_colours[as.character(x$reference_cluster)])),
        customdata = sprintf(
          "%s \u2192 %s<br>Gemeinsame Kreise: %d",
          x$reference_cluster, x$target_cluster, x$n_shared
        ),
        hovertemplate = "%{customdata}<extra></extra>"
      )
    )
  }
  first <- trace(2L, c(0, .47), c(.55, 1))
  second <- trace(4L, c(.53, 1), c(.55, 1))
  third <- trace(5L, c(.14, .86), c(0, .41))
  plot <- do.call(plotly::plot_ly, first)
  plot <- do.call(plotly::add_trace, c(list(p = plot), second))
  plot <- do.call(plotly::add_trace, c(list(p = plot), third))
  plotly::layout(
    plot,
    annotations = list(
      list(x = .235, y = 1.035, xref = "paper", yref = "paper",
        text = "k=3 \u2192 k=2", showarrow = FALSE),
      list(x = .765, y = 1.035, xref = "paper", yref = "paper",
        text = "k=3 \u2192 k=4", showarrow = FALSE),
      list(x = .5, y = .46, xref = "paper", yref = "paper",
        text = "k=3 \u2192 k=5", showarrow = FALSE)
    ),
    margin = list(l = 12, r = 12, t = 34, b = 12)
  )
}

.shiny_transition_profile_labels <- function() {
  c(older = "\u00c4lter/l\u00e4ndlich", family = "Familie/Jugend",
    dense = "Dicht")
}

.shiny_transition_profile_order <- function() {
  c("older", "family", "dense")
}

.shiny_transition_profile_colours <- function() {
  c(older = "#274f66", family = "#748c61", dense = "#bc5e21")
}

.shiny_transition_sankey_widget <- function(transition) {
  required <- c("assignments", "count_matrix", "profile_alignment",
    "diagnostics", "provenance")
  if (!is.list(transition) || !all(required %in% names(transition))) {
    stop("Transition Sankey requires a validated transition result.",call.=FALSE)
  }
  classes <- .shiny_transition_profile_order()
  labels <- .shiny_transition_profile_labels()
  colours <- .shiny_transition_profile_colours()
  counts <- as.data.frame(transition$count_matrix,stringsAsFactors=FALSE)
  names(counts)<-c("from","to","n")
  counts<-counts[counts$n>0L,,drop=FALSE]
  nodes<-c(paste0("2017\u20132020 \u00b7 ",labels[classes]),
    paste0("2022\u20132025 \u00b7 ",labels[classes]))
  rgba<-function(colour,alpha=.55){rgb<-grDevices::col2rgb(colour);sprintf(
    "rgba(%d,%d,%d,%.2f)",rgb[1L,],rgb[2L,],rgb[3L,],alpha)}
  plotly::layout(plotly::plot_ly(
    type="sankey",orientation="h",arrangement="snap",
    node=list(label=unname(nodes),pad=16,thickness=18,
      color=rep(unname(colours[classes]),2L)),
    link=list(
      source=match(as.character(counts$from),classes)-1L,
      target=length(classes)+match(as.character(counts$to),classes)-1L,
      value=counts$n,
      color=unname(rgba(colours[as.character(counts$from)])),
      customdata=sprintf("%s \u2192 %s<br>Kreise: %d",
        labels[as.character(counts$from)],labels[as.character(counts$to)],
        counts$n),hovertemplate="%{customdata}<extra></extra>")),
    margin=list(l=15,r=15,t=20,b=20))
}

.shiny_transition_map_widget <- function(transition, map, bounds, states,
    exterior_geojson, filter = "all") {
  assignments<-transition$assignments
  categories<-ifelse(assignments$changed,
    paste(assignments$from_profile_class,assignments$to_profile_class,sep="_to_"),
    "unchanged")
  allowed<-c("all","changed","unchanged",sort(unique(categories[assignments$changed])))
  if(length(filter)!=1L||is.na(filter)||!filter%in%allowed)
    stop("Unknown transition map filter.",call.=FALSE)
  selected<-if(identical(filter,"all"))rep(TRUE,nrow(assignments)) else
    if(identical(filter,"changed"))assignments$changed else categories==filter
  display_id<-ifelse(selected,categories,"not_selected")
  map_assignments<-data.frame(geo_id=assignments$geo_id,
    display_cluster_id=display_id,stringsAsFactors=FALSE)
  transitions<-sort(unique(categories[assignments$changed]),method="radix")
  category_levels<-c(
    if("unchanged"%in%display_id)"unchanged",
    transitions[transitions%in%display_id],
    if("not_selected"%in%display_id)"not_selected")
  transition_colours<-.shiny_transition_profile_colours()
  category_colour<-vapply(category_levels,function(category){
    if(identical(category,"not_selected"))return("#E1E3E6")
    if(identical(category,"unchanged"))return("#9C9EB5")
    target<-sub("^.*_to_","",category)
    unname(transition_colours[[target]])
  },character(1L))
  category_label<-vapply(category_levels,function(category){
    if(identical(category,"not_selected"))return("Nicht ausgew\u00e4hlt")
    if(identical(category,"unchanged"))return("Profil unver\u00e4ndert")
    bits<-strsplit(category,"_to_",fixed=TRUE)[[1L]]
    paste(.shiny_transition_profile_labels()[bits],collapse=" \u2192 ")
  },character(1L))
  metadata<-data.frame(display_cluster_id=category_levels,
    display_label=unname(category_label),display_colour=unname(category_colour),
    display_order=seq_along(category_levels),
    n_districts=as.integer(table(factor(display_id,levels=category_levels))),
    profile_description=unname(category_label),stringsAsFactors=FALSE)
  widget<-.shiny_app_leaflet_geojson(map$browser_geojson,bounds,map_assignments,
    "dynamic",states$browser_geojson,exterior_geojson,"neutral",NULL,metadata)
  shown<-metadata$display_cluster_id!="not_selected"
  leaflet::addLegend(widget,position="bottomright",
    colors=metadata$display_colour[shown],
    labels=metadata$display_label[shown],opacity=.88,
    title="Typologie\u00fcbergang")
}
