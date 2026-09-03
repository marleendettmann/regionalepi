.profile_description_spec <- function() list(
  specification_id = "dynamic_profile_descriptions_v1",
  moderate_threshold = 0.5, strong_threshold = 1.0,
  maximum_indicators = 2L, tie_method = "indicator_order",
  positive = c(moderate = "h\u00f6here", strong = "deutlich h\u00f6here"),
  negative = c(moderate = "niedrigere", strong = "deutlich niedrigere")
)

.shiny_profile_descriptions <- function(profiles, mode = "dynamic") {
  if (identical(mode, "dissertation")) return(NULL)
  spec <- .profile_description_spec()
  display <- .shiny_app_indicator_display()
  rows <- split(profiles, profiles$display_cluster_id)
  descriptions <- vapply(rows, function(x) {
    x$order <- match(x$indicator_id, display$indicator_id)
    x <- x[order(-abs(x$standardized_center), x$order), , drop = FALSE]
    x <- x[abs(x$standardized_center) >= spec$moderate_threshold, , drop = FALSE]
    if (!nrow(x)) return("durchschnittliches Profil")
    x <- utils::head(x, spec$maximum_indicators)
    parts <- vapply(seq_len(nrow(x)), function(i) {
      z <- x$standardized_center[[i]]
      strength <- if (abs(z) >= spec$strong_threshold) "strong" else "moderate"
      word <- if (z > 0) spec$positive[[strength]] else spec$negative[[strength]]
      paste(word, display$label[match(x$indicator_id[[i]], display$indicator_id)])
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
  figure <- plotly::plot_ly(x = x, y = y, z = audit$incidence,
    type = "heatmap", colors = c("#F7FBFF", "#6BAED6", "#08306B"),
    text = audit$hover, hoverinfo = "text", customdata = audit$customdata,
    source = source, connectgaps = FALSE, colorbar = list(title = "Inzidenz"))
  if (any(audit$missing)) figure <- plotly::add_trace(figure, x = x, y = y,
    z = ifelse(audit$missing, 1, NA_real_), type = "heatmap",
    colorscale = list(c(0, "#D9D9D9"), c(1, "#D9D9D9")),
    zmin = 0, zmax = 1, showscale = FALSE, text = audit$hover,
    customdata = audit$customdata, hoverinfo = "text", connectgaps = FALSE,
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
