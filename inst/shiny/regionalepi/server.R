server <- function(input, output, session) {
  cache <- new.env(parent = emptyenv())
  cache$map <- regionalepi::regionalepi_map_geometry()
  cache$map_bounds <- regionalepi:::.shiny_app_map_bounds(cache$map)
  cache$surveillance_index <- list()
  resources <- get(
    "regionalepi_geography_resources_2024",
    envir = asNamespace("regionalepi"), inherits = TRUE
  )
  state <- reactiveValues(result = NULL, error = NULL, technical_error = NULL,
                          busy = FALSE, selected_geo_id = NULL)

  observeEvent(input$pathogen, {
    choices <- regionalepi:::.shiny_period_choices(input$pathogen)
    updateSelectInput(session, "period_id", choices = choices,
                      selected = unname(choices[[length(choices)]]))
  }, ignoreInit = FALSE)

  observeEvent(input$selected_geo_id, {
    state$selected_geo_id <- input$selected_geo_id
  })

  observeEvent(input$load_analysis, {
    state$error <- NULL
    state$technical_error <- NULL
    state$busy <- TRUE
    on.exit(state$busy <- FALSE, add = TRUE)
    tryCatch({
      withProgress(message = "Analyse wird vorbereitet …", value = 0, {
        setProgress(0.03, detail = "Auswahl und Referenzzeiträume werden geprüft …")
        selected <- regionalepi:::.shiny_selected_period(
          input$pathogen, input$period_id
        )
        demographic_years <- regionalepi:::.shiny_demographic_periods()[[
          input$demographic_period
        ]]
        demographic_key <- regionalepi:::.shiny_demographic_cache_key(
          input$demographic_period
        )
        if (!exists(demographic_key, cache, inherits = FALSE)) {
          setProgress(0.08, detail = "Demographische Regionaldaten werden geladen …")
          assign(demographic_key,
                 regionalepi:::.shiny_fetch_demography(demographic_years), cache)
        }
        demographic <- get(demographic_key, cache, inherits = FALSE)

        setProgress(0.57, detail = "Demographische Typologie wird angepasst …")
        typology_key <- regionalepi:::.shiny_typology_cache_key(
          demographic$summary, as.integer(input$k)
        )
        if (!exists(typology_key, cache, inherits = FALSE)) {
          assign(typology_key, regionalepi::fit_dynamic_typology(
            demographic$summary, k = as.integer(input$k)
          ), cache)
        }
        fit <- get(typology_key, cache, inherits = FALSE)

        reporting_years <- regionalepi:::.shiny_reference_years(selected$row)
        surveillance_key <- regionalepi:::.shiny_surveillance_cache_match(
          cache$surveillance_index, input$pathogen, reporting_years
        )
        if (is.null(surveillance_key)) {
          surveillance_key <- regionalepi:::.shiny_surveillance_cache_key(
            input$pathogen, reporting_years
          )
          setProgress(0.62, detail = "SurvStat-Kreisdaten werden geladen …")
          assign(surveillance_key, regionalepi:::.shiny_fetch_surveillance(
            input$pathogen, reporting_years, resources
          ), cache)
          cache$surveillance_index[[surveillance_key]] <- list(
            key = surveillance_key, pathogen = input$pathogen,
            reporting_years = reporting_years
          )
        }
        surveillance <- get(surveillance_key, cache, inherits = FALSE)

        setProgress(0.78, detail = "Geographien und epidemiologischer Zeitraum werden verknüpft …")
        summary_key <- regionalepi:::.shiny_summary_cache_key(
          surveillance_key, input$period_id, fit$provenance$fit_id
        )
        if (!exists(summary_key, cache, inherits = FALSE)) {
          assign(summary_key, regionalepi:::.shiny_analyse_period(
            surveillance, selected, fit
          ), cache)
        }
        epidemiology <- get(summary_key, cache, inherits = FALSE)
        map_join <- regionalepi:::.shiny_map_assignments(cache$map, fit)

        setProgress(0.9, detail = "Kartendarstellung wird vorbereitet …")
        map_key <- paste0("map-widget:", fit$provenance$fit_id)
        if (!exists(map_key, cache, inherits = FALSE)) {
          geojson <- regionalepi:::.shiny_app_map_geojson(cache$map, map_join$data)
          assign(map_key, regionalepi:::.shiny_app_leaflet_geojson(
            geojson, cache$map_bounds
          ), cache)
        }
        state$result <- list(
          demographic = demographic, fit = fit, surveillance = surveillance,
          epidemiology = epidemiology, selected_period = selected,
          map_join = map_join, map_widget = get(map_key, cache, inherits = FALSE),
          demographic_years = demographic_years,
          cache_keys = list(demographic = demographic_key,
                            typology = typology_key,
                            surveillance = surveillance_key,
                            epidemiology = summary_key, map = map_key)
        )
        if (is.null(state$selected_geo_id) ||
            !state$selected_geo_id %in% map_join$data$geo_id) {
          state$selected_geo_id <- "11000"
        }
        setProgress(1, detail = "Darstellung ist bereit.")
      })
    }, error = function(error) {
      state$technical_error <- conditionMessage(error)
      state$error <- regionalepi:::.shiny_app_format_error(
        error, "Die Live-Analyse"
      )
    })
  })

  output$load_status <- renderUI({
    if (isTRUE(state$busy)) return(p("Daten werden geladen …"))
    if (!is.null(state$error)) return(p(class = "status-error", state$error))
    if (is.null(state$result)) return(p("Noch keine Analyse geladen."))
    p(class = "status-ok", "Analyse geladen: ",
      state$result$fit$provenance$fit_id)
  })

  output$district_map <- leaflet::renderLeaflet({
    req(state$result)
    state$result$map_widget
  })

  output$map_attribution <- renderUI({
    provenance <- cache$map$provenance
    p(class = "app-note", provenance$attribution, " · ",
      a(provenance$license, href = provenance$license_url,
        target = "_blank", rel = "noopener noreferrer"), " · ",
      a("BKG-Quelle", href = provenance$source_reference,
        target = "_blank", rel = "noopener noreferrer"), " · ",
      "Durch regionalepi für die Kartendarstellung bearbeitet.")
  })

  output$profile_plot <- renderPlot({
    req(state$result)
    profiles <- state$result$fit$profiles
    clusters <- sort(unique(profiles$display_cluster_id))
    indicators <- state$result$fit$matrix$indicator_order
    display <- regionalepi:::.shiny_app_indicator_display()
    matrix <- matrix(NA_real_, nrow = length(indicators), ncol = length(clusters),
                     dimnames = list(indicators, clusters))
    for (i in seq_len(nrow(profiles))) {
      matrix[profiles$indicator_id[[i]], profiles$display_cluster_id[[i]]] <-
        profiles$standardized_center[[i]]
    }
    colours <- regionalepi:::.shiny_app_cluster_colours(clusters)
    graphics::matplot(seq_along(indicators), matrix, type = "b", pch = 19,
      lty = 1, col = colours, xaxt = "n", xlab = "",
      ylab = "Standardisiertes Clusterprofil")
    graphics::axis(1, at = seq_along(indicators),
      labels = display$label[match(indicators, display$indicator_id)])
    graphics::abline(h = 0, col = "grey70", lty = 2)
    graphics::legend("topright", legend = clusters, col = colours,
                     pch = 19, lty = 1, bty = "n")
  })

  output$profile_table <- renderTable({
    req(state$result)
    profiles <- state$result$fit$profiles
    display <- regionalepi:::.shiny_app_indicator_display()
    position <- match(profiles$indicator_id, display$indicator_id)
    data.frame(
      Cluster = profiles$display_cluster_id,
      N = profiles$cluster_size,
      Indikator = display$label[position],
      Einheit = display$unit[position],
      Mittelwert = round(profiles$original_mean, 2),
      Median = round(profiles$original_median, 2), check.names = FALSE
    )
  }, striped = TRUE, spacing = "xs", rownames = FALSE)

  output$cluster_warning <- renderUI({
    req(state$result)
    diagnostics <- state$result$fit$cluster_diagnostics
    flagged <- diagnostics$display_cluster_id[diagnostics$minimum_size_warning]
    if (!length(flagged)) return(p(class = "app-note", "Keine Mindestgrößenwarnung."))
    p(class = "status-error", "Mindestgrößenwarnung für: ",
      paste(flagged, collapse = ", "))
  })

  output$incidence_plot <- renderPlot({
    req(state$result)
    data <- state$result$epidemiology$summary$data
    clusters <- sort(unique(data$cluster_id))
    colours <- regionalepi:::.shiny_app_cluster_colours(clusters)
    limits <- range(data$median_incidence, na.rm = TRUE)
    if (!all(is.finite(limits))) limits <- c(0, 1)
    graphics::plot(range(data$date), limits, type = "n", xlab = "Meldewoche",
      ylab = "Mediane source-provided Inzidenz",
      main = paste(input$pathogen, state$result$selected_period$row$label))
    for (cluster in clusters) {
      rows <- data$cluster_id == cluster
      values <- data[rows, , drop = FALSE]
      values <- values[order(values$date), ]
      graphics::lines(values$date, values$median_incidence,
                      col = colours[[cluster]], lwd = 2)
    }
    graphics::legend("topright", legend = clusters, col = colours,
                     lwd = 2, bty = "n")
  })

  output$incidence_warning <- renderUI({
    req(state$result)
    data <- state$result$epidemiology$summary$data
    severe <- data$expected_district_count > 0 &
      data$observed_non_missing_count / data$expected_district_count < 0.8
    if (!any(severe)) {
      return(p(class = "app-note",
        "Fehlende Inzidenzen bleiben NA; dargestellt wird der Median der beobachteten Kreise."))
    }
    p(class = "status-error", sum(severe),
      " Wochen-/Clustergruppen haben weniger als 80 % beobachtete Kreise.")
  })

  output$district_detail <- renderUI({
    req(state$result, state$selected_geo_id)
    id <- state$selected_geo_id
    assignment <- state$result$map_join$data[
      state$result$map_join$data$geo_id == id, , drop = FALSE
    ]
    summary <- state$result$demographic$summary$data
    values <- summary[summary$geo_id == id, , drop = FALSE]
    display <- regionalepi:::.shiny_app_indicator_display()
    position <- match(values$indicator_id, display$indicator_id)
    tagList(
      h4(assignment$geo_name),
      p(strong("AGS: "), id),
      p(strong("Cluster: "), assignment$display_cluster_id),
      tags$ul(lapply(seq_len(nrow(values)), function(i) tags$li(
        display$label[[position[[i]]]], ": ",
        format(round(values$indicator_value[[i]], 2), trim = TRUE), " ",
        display$unit[[position[[i]]]]
      )))
    )
  })

  output$provenance <- renderUI({
    req(state$result)
    result <- state$result
    period <- result$selected_period$row
    source_status <- regionalepi:::.shiny_app_source_status(
      result$demographic$source
    )
    sizes <- result$fit$cluster_diagnostics
    size_text <- paste0(sizes$display_cluster_id, " = ", sizes$size,
                        collapse = ", ")
    queries <- result$surveillance$provenance
    query_text <- paste(vapply(queries, function(query) paste0(
      query$query_id, " [", paste(query$reporting_years, collapse = "–"), "]"
    ), character(1L)), collapse = "; ")
    tagList(
      h4("Demographie"), p("Regionaldatenbank Deutschland; Referenzjahre ",
        paste(result$demographic_years, collapse = ", "), "; ",
        result$fit$indicator_set$indicator_set_id, " / ",
        result$fit$indicator_set$definition_version,
        if (length(source_status)) paste0("; Datenstand ", paste(source_status, collapse = ", "))),
      h4("Typologie"), p(result$fit$fitting_specification$fitting_specification_id,
        " / ", result$fit$fitting_specification$definition_version,
        "; k = ", result$fit$diagnostics$k,
        "; Fit-ID ", result$fit$provenance$fit_id,
        "; Clustergrößen ", size_text,
        "; Seed ", result$fit$diagnostics$seed,
        "; Z-Standardisierung mit Stichproben-SD (n−1)."),
      h4("Surveillance"), p("SurvStat@RKI; ", input$pathogen,
        "; source-provided Inzidenz; Kreisabfrage mit reviewtem separatem Berlin-Ersatz; Datenstand ",
        paste(regionalepi:::.shiny_app_query_status(queries), collapse = ", "),
        "; Queries ", query_text, "."),
      h4("Epidemiologischer Zeitraum"), p(period$label, "; ",
        period$period_set_id, " / ", period$definition_version,
        "; ", period$source_reference),
      h4("Karte"), p("BKG VG2500, Gebietsstand 2024-12-31; ",
        cache$map$provenance$attribution, " ",
        cache$map$provenance$change_notice),
      h4("Paket"), p("regionalepi ", as.character(utils::packageVersion("regionalepi"))),
      if (length(result$map_join$typology_only_geo_ids)) p(class = "app-note",
        "Der Fit basiert auf 401 Einheiten. Nicht auf der 2024-Karte dargestellt: ",
        paste(result$map_join$typology_only_geo_ids, collapse = ", "), ".")
    )
  })
}
