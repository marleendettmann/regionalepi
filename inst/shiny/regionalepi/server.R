server <- function(input, output, session) {
  cache <- new.env(parent=emptyenv()); cache$map <- regionalepi::regionalepi_map_geometry()
  cache$states <- regionalepi::regionalepi_state_boundaries(); cache$crosswalk <- regionalepi::regionalepi_district_state_crosswalk(); cache$map_bounds <- regionalepi:::.shiny_app_map_bounds(cache$map)
  cache$surveillance_index <- list()
  resources <- get("regionalepi_geography_resources_2024", envir=asNamespace("regionalepi"), inherits=TRUE)
  state <- reactiveValues(result=NULL,error=NULL,technical_error=NULL,busy=FALSE,
    selected_geo_id=NULL,selected_date=NULL,retrieval_count=0L)
  session$userData$plotlyShinyEventIDs <- paste("plotly_click",
    c("demography","period","district-time","district-heat"),sep="-")

  initial_selection <- regionalepi:::.shiny_reconcile_selection(
    "Influenza, saisonal", "influenza_2025_26", "window")
  selection <- reactiveVal(initial_selection)
  custom_week_selection <- reactiveVal(
    regionalepi:::.shiny_reconcile_custom_weeks(initial_selection$window,
      as_of_date=regionalepi:::.shiny_analysis_as_of_date()))
  reconcile_custom_weeks <- function(window, start_week=NULL, end_week=NULL,
                                     changed=NULL) {
    next_custom <- regionalepi:::.shiny_reconcile_custom_weeks(
      window,start_week,end_week,regionalepi:::.shiny_analysis_as_of_date(),changed)
    custom_week_selection(next_custom)
    if(!identical(next_custom$start_week,start_week)) {
      freezeReactiveValue(input,"custom_start_week")
      updateSelectInput(session,"custom_start_week",selected=next_custom$start_week)
    }
    if(!identical(next_custom$end_week,end_week)) {
      freezeReactiveValue(input,"custom_end_week")
      updateSelectInput(session,"custom_end_week",selected=next_custom$end_week)
    }
    next_custom
  }
  reconcile_selection <- function(pathogen, window_id, range_mode,
                                  reviewed_period_id = NULL) {
    next_selection <- regionalepi:::.shiny_reconcile_selection(
      pathogen, window_id, range_mode, reviewed_period_id)
    selection(next_selection)
    if(!identical(next_selection$range_mode,range_mode)) {
      freezeReactiveValue(input,"range_mode")
      updateRadioButtons(session,"range_mode",selected=next_selection$range_mode)
    }
    if(!identical(next_selection$reviewed_period_id,reviewed_period_id)) {
      freezeReactiveValue(input,"reviewed_period_id")
      updateSelectInput(session,"reviewed_period_id",
        selected=if(is.na(next_selection$reviewed_period_id)) character() else
          next_selection$reviewed_period_id)
    }
    reconcile_custom_weeks(next_selection$window,isolate(input$custom_start_week),
      isolate(input$custom_end_week))
    next_selection
  }
  observeEvent(input$pathogen, {
    current <- isolate(input$window_id); mode <- isolate(input$range_mode)
    period <- isolate(input$reviewed_period_id)
    next_selection <- reconcile_selection(input$pathogen,current,mode,period)
    if(!identical(next_selection$window_id,current)) {
      freezeReactiveValue(input,"window_id")
      updateSelectInput(session,"window_id",choices=next_selection$choices,
        selected=next_selection$window_id)
    } else updateSelectInput(session,"window_id",choices=next_selection$choices,
      selected=next_selection$window_id)
  }, ignoreInit=TRUE,priority=200)
  observeEvent(input$window_id, {
    req(input$pathogen,input$window_id)
    choices <- regionalepi:::.shiny_window_choices(input$pathogen)
    if(input$window_id %in% unname(choices)) {
      mode<-isolate(input$range_mode)
      period<-isolate(input$reviewed_period_id)
      reconcile_selection(input$pathogen,input$window_id,mode,period)
    }
  },ignoreInit=TRUE,priority=200)
  observeEvent(input$range_mode, {
    req(input$pathogen,input$window_id,input$range_mode)
    reconcile_selection(input$pathogen,input$window_id,input$range_mode,
      isolate(input$reviewed_period_id))
  },ignoreInit=TRUE,priority=200)
  invalidate_stale_result <- function() {
    if(is.null(state$result)||is.null(input$pathogen)||is.null(input$window_id))return()
    loaded<-state$result$window
    if(!identical(input$pathogen,loaded$pathogen)||
       !identical(input$window_id,loaded$observation_window_id)) {
      state$result<-NULL
      state$error<-NULL
      state$technical_error<-NULL
      state$selected_date<-NULL
    }
  }
  observeEvent(input$pathogen,invalidate_stale_result(),ignoreInit=FALSE,priority=100)
  observeEvent(input$window_id,invalidate_stale_result(),ignoreInit=FALSE,priority=100)
  selected_window <- reactive(selection()$window)
  reviewed_periods <- reactive(selection()$periods)
  observe({
    window <- selected_window(); periods <- reviewed_periods()
    choices <- regionalepi:::.shiny_period_choices_in_window(window)
    modes <- c("Gesamter Beobachtungszeitraum"="window","Benutzerdefinierter Zeitraum"="custom")
    if(nrow(periods)) modes <- c("Gesamter Beobachtungszeitraum"="window","Vordefinierter epidemiologischer Zeitraum"="reviewed","Benutzerdefinierter Zeitraum"="custom")
    selected_mode <- selection()$range_mode
    if(!selected_mode %in% unname(modes)) selected_mode <- "window"
    updateRadioButtons(session,"range_mode",choices=modes,selected=selected_mode)
    selected_period <- selection()$reviewed_period_id
    updateSelectInput(session,"reviewed_period_id",choices=choices,
      selected=if(length(choices) && !is.na(selected_period)) selected_period else character())
    custom <- custom_week_selection()
    updateSelectInput(session,"custom_start_week",choices=custom$choices,
      selected=custom$start_week)
    updateSelectInput(session,"custom_end_week",choices=custom$choices,
      selected=custom$end_week)
  })
  output$reviewed_period_note <- renderUI({
    if(nrow(reviewed_periods())) return(NULL)
    p(class="app-note","Für diesen Beobachtungszeitraum ist kein vordefinierter epidemiologischer Zeitraum hinterlegt.")
  })
  output$period_availability_note <- renderUI({
    if(identical(input$pathogen,"Norovirus-Gastroenteritis"))
      p(class="app-note","Für Norovirus sind keine vordefinierten epidemiologischen Zeiträume hinterlegt.")
  })
  output$range_mode_note <- renderUI({
    text<-switch(input$range_mode,
      window="Umfasst alle Wochen des gewählten Beobachtungszeitraums.",
      reviewed="Verwendet einen für den ausgewählten Erreger hinterlegten epidemiologischen Zeitraum, z. B. eine AGI-Influenzawelle, eine retrospektive Pandemiephase oder eine definierte COVID-19-Welle des RKI.",
      custom="Start- und End-KW können innerhalb des Beobachtungszeitraums gewählt werden.",NULL)
    if(is.null(text))NULL else p(class="app-note",text)
  })
  output$demographic_source_control <- renderUI({
    available <- regionalepi:::.shiny_regional_credentials_available()
    live_label <- if(available) "Live-Abruf Regionaldatenbank" else
      "Live-Abruf Regionaldatenbank (Zugangsdaten erforderlich)"
    source_choices <- stats::setNames(
      c("snapshot", "live"),
      c("Geprüfter Snapshot (empfohlen)", live_label)
    )
    control <- selectInput(
      "demographic_source", "Demografische Daten für die Typologie",
      source_choices, "snapshot"
    )
    explanation <- p(class="app-note",
      "Der geprüfte Snapshot enthält sowohl die demografischen Daten der Typologie als auch die amtliche durchschnittliche Jahresbevölkerung für dynamische Inzidenzen. Der Live-Abruf bezieht beide Komponenten neu aus der Regionaldatenbank.")
    if(available) return(tagList(control,explanation))
    tagList(
      control,
      tags$script(HTML(
        "setTimeout(function(){var e=document.getElementById('demographic_source');if(!e)return;var o=e.querySelector('option[value=\"live\"]');if(o)o.disabled=true;if(e.selectize){var s=e.selectize;var d=function(){var x=s.$dropdown_content.find('[data-value=\"live\"]');x.addClass('disabled').attr('aria-disabled','true');x.off('.regionalepi').on('mousedown.regionalepi click.regionalepi',function(v){v.preventDefault();v.stopImmediatePropagation();});};s.on('dropdown_open',d);s.on('change',function(v){if(v==='live')s.setValue('snapshot');});if(s.getValue()==='live')s.setValue('snapshot');d();}},0);"
      )),
      explanation,
      p(class="app-note",
        "Der geprüfte Snapshot unterstützt die vollständige dynamische Analyse ohne Regionaldatenbank-Zugangsdaten. Nur der optionale Live-Abruf erfordert REGIONALSTATISTIK_USER und REGIONALSTATISTIK_PASSWORD. Hinweise zur sicheren Einrichtung: Methodik und Daten → Datenquellen → Regionaldatenbank.")
    )
  })
  reviewed_period <- reactive({
    x <- reviewed_periods(); if(!nrow(x)) return(NULL)
    x[x$period_id==selection()$reviewed_period_id,,drop=FALSE]
  })
  observeEvent(input$reviewed_period_id,{
    req(input$pathogen,input$window_id)
    reconcile_selection(input$pathogen,input$window_id,
      isolate(input$range_mode),input$reviewed_period_id)
    if(is.null(state$result)||!identical(input$range_mode,"reviewed"))return()
    period<-reviewed_period();if(is.null(period)||!nrow(period))return()
    required<-regionalepi:::.shiny_reference_years(period)
    if(!all(required%in%state$result$reporting_years))state$result<-NULL
  },ignoreInit=TRUE,priority=200)
  observeEvent(input$custom_start_week,{
    req(input$custom_start_week)
    reconcile_custom_weeks(selected_window(),input$custom_start_week,
      isolate(input$custom_end_week),"start")
  },ignoreInit=TRUE,priority=200)
  observeEvent(input$custom_end_week,{
    req(input$custom_end_week)
    reconcile_custom_weeks(selected_window(),isolate(input$custom_start_week),
      input$custom_end_week,"end")
  },ignoreInit=TRUE,priority=200)
  analysis_range <- reactive({
    mode <- selection()$range_mode
    if(identical(mode,"custom")) {
      custom <- custom_week_selection()
      return(regionalepi:::.shiny_select_custom_week_range(selected_window(),
        custom$start_week,custom$end_week,regionalepi:::.shiny_analysis_as_of_date()))
    }
    regionalepi:::.shiny_select_analysis_range(selected_window(),mode,reviewed_period())
  })

  observeEvent(input$selected_geo_id,{ state$selected_geo_id <- input$selected_geo_id })
  select_plotly <- function(source) observeEvent(suppressWarnings(plotly::event_data("plotly_click",source=source)),{
    event <- suppressWarnings(plotly::event_data("plotly_click",source=source))
    if(!is.null(event$key) && nzchar(event$key[[1L]])) state$selected_geo_id <- as.character(event$key[[1L]])
    if(!is.null(event$customdata) && length(event$customdata)) {
      value <- as.character(event$customdata[[1L]])
      bits <- strsplit(value,"\\|")[[1L]]
      if(length(bits)>=1L && grepl("^[0-9]{5}$",bits[[1L]])) state$selected_geo_id <- bits[[1L]]
      if(length(bits)>=2L && !is.na(as.Date(bits[[2L]]))) state$selected_date <- as.Date(bits[[2L]])
    }
    if(identical(source,"district-time") && !is.null(event$x)) {
      candidate <- tryCatch(suppressWarnings(as.Date(as.character(event$x[[1L]]))),
                            error=function(e) as.Date(NA))
      if(!is.na(candidate)) state$selected_date <- candidate
    }
  },ignoreInit=TRUE)
  select_plotly("demography"); select_plotly("period"); select_plotly("district-time"); select_plotly("district-heat")

  observeEvent(input$load_analysis,{
    state$error<-NULL; state$technical_error<-NULL; state$busy<-TRUE; on.exit(state$busy<-FALSE,add=TRUE)
    tryCatch(withProgress(message="Analyse wird vorbereitet …",value=0,{
      window <- selected_window();load_range<-analysis_range()
      request_range<-regionalepi:::.shiny_effective_analysis_range(
        load_range,regionalepi:::.shiny_analysis_as_of_date())
      years<-seq.int(
        as.integer(format(request_range$start_date,"%Y")),
        as.integer(format(request_range$end_date,"%Y")))
      mode <- input$typology_mode; period_label <- if(mode=="dissertation") "2017–2020" else input$demographic_period
      source_mode <- input$demographic_source
      if(is.null(source_mode)||!source_mode%in%c("snapshot","live"))source_mode<-"snapshot"
      if(identical(source_mode,"live") &&
         !regionalepi:::.shiny_regional_credentials_available())
        stop("Live-Abruf derzeit nicht konfiguriert. Erforderlich sind die Umgebungsvariablen REGIONALSTATISTIK_USER und REGIONALSTATISTIK_PASSWORD. Hinweise zur sicheren Einrichtung: Methodik und Daten → Datenquellen → Regionaldatenbank.",call.=FALSE)
      demographic_years <- regionalepi:::.shiny_demographic_periods()[[period_label]]
      dkey <- regionalepi:::.shiny_demographic_cache_key(period_label,source_mode)
      if(!exists(dkey,envir=cache,inherits=FALSE)) assign(dkey,if(source_mode=="snapshot") regionalepi:::.shiny_fetch_snapshot_demography(demographic_years) else regionalepi:::.shiny_fetch_demography(demographic_years),envir=cache)
      demographic <- get(dkey,envir=cache,inherits=FALSE); setProgress(.2,detail="Typologie wird angepasst …")
      tkey <- regionalepi:::.shiny_typology_cache_key(demographic$summary,if(mode=="dissertation")3L else as.integer(input$k),mode)
      if(!exists(tkey,envir=cache,inherits=FALSE)) assign(tkey,regionalepi:::.shiny_fit_typology(demographic$summary,mode,if(mode=="dissertation")3L else as.integer(input$k)),envir=cache)
      fit <- get(tkey,envir=cache,inherits=FALSE)
      reference_key <- "typology:dissertation_v1:historical_reference"
      if(!exists(reference_key,envir=cache,inherits=FALSE)) assign(reference_key,
        regionalepi:::.shiny_fit_typology(
          regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary,
          "dissertation",3L),envir=cache)
      stability_fits <- lapply(2:5,function(stability_k) {
        key <- regionalepi:::.shiny_typology_cache_key(
          demographic$summary,stability_k,"dynamic")
        if(!exists(key,envir=cache,inherits=FALSE)) assign(key,
          regionalepi:::.shiny_fit_typology(demographic$summary,"dynamic",stability_k),
          envir=cache)
        get(key,envir=cache,inherits=FALSE)
      })
      stability <- regionalepi:::.shiny_typology_stability(stability_fits)
      stability_policy <- regionalepi:::.shiny_dynamic_colour_policy(
        stability_fits[[2L]],get(reference_key,envir=cache,inherits=FALSE),
        stability_fits[[2L]])
      stability_metadata <- regionalepi:::.shiny_cluster_display_metadata(
        stability_fits[[2L]],"dynamic","profile_aligned",stability_policy,
        regionalepi_map_geometry()$features$geo_id)
      palette_variant <- if(mode=="dynamic"&&as.integer(input$k)>=2L) "profile_aligned" else "neutral"
      palette_alignment <- NULL
      if(mode=="dynamic"&&as.integer(input$k)>=2L) {
        anchor_key <- regionalepi:::.shiny_typology_cache_key(
          demographic$summary,3L,"dynamic")
        if(!exists(anchor_key,envir=cache,inherits=FALSE)) assign(anchor_key,
          regionalepi:::.shiny_fit_typology(demographic$summary,"dynamic",3L),envir=cache)
        palette_alignment <- regionalepi:::.shiny_dynamic_colour_policy(
          fit,get(reference_key,envir=cache,inherits=FALSE),
          get(anchor_key,envir=cache,inherits=FALSE))
      }
      incidence_definition <- if(mode=="dissertation") "source_survstat_incidence" else "annual_average_population_v1"
      population_source_mode <- if(mode=="dissertation") "not_applicable" else source_mode
      skey <- regionalepi:::.shiny_surveillance_cache_match(cache$surveillance_index,input$pathogen,years,incidence_definition,population_source_mode,request_range$end_date)
      if(is.null(skey)) {
        setProgress(.35,detail=if(mode=="dissertation") "Historische SurvStat-Inzidenzen und gemeldete Fälle werden geladen …" else "SurvStat-Fallzahlen und amtliche durchschnittliche Jahresbevölkerung werden geladen …")
        fetched_bundle <- if(mode=="dissertation") {
          historical<-regionalepi:::.shiny_fetch_surveillance_bundle(input$pathogen,years,resources)
          historical<-regionalepi:::.shiny_limit_surveillance_range(historical,request_range)
          historical$provenance$analysis_range<-list(
            observation_window_start=request_range$nominal_start_date,
            observation_window_end=request_range$nominal_end_date,
            analysis_as_of_date=request_range$analysis_as_of_date,
            effective_analysis_start=request_range$start_date,
            effective_analysis_end=request_range$end_date)
          historical
        } else regionalepi:::.shiny_fetch_analysis_bundle(input$pathogen,years,resources,source_mode,request_range)
        cache_metadata <- regionalepi:::.shiny_analysis_cache_metadata(fetched_bundle)
        skey <- do.call(regionalepi:::.shiny_surveillance_bundle_cache_key,c(list(pathogen=input$pathogen,reporting_years=years,incidence_definition=incidence_definition,effective_analysis_end=request_range$end_date),cache_metadata))
        assign(skey,fetched_bundle,envir=cache)
        cache$surveillance_index[[skey]] <- c(list(key=skey,pathogen=input$pathogen,reporting_years=years,incidence_definition=incidence_definition,effective_analysis_end=request_range$end_date),cache_metadata)
        state$retrieval_count <- state$retrieval_count+1L
      }
      bundle <- get(skey,envir=cache,inherits=FALSE); map_join <- regionalepi:::.shiny_map_assignments(cache$map,fit)
      display_metadata <- regionalepi:::.shiny_cluster_display_metadata(
        fit,mode,palette_variant,palette_alignment,map_join$data$geo_id)
      mkey <- paste0("map:",fit$provenance$fit_id,":",mode,":",palette_variant)
      if(!exists(mkey,envir=cache,inherits=FALSE)) assign(mkey,regionalepi:::.shiny_app_leaflet_geojson(cache$map$browser_geojson,
        cache$map_bounds,map_join$data,mode,cache$states$browser_geojson,
        regionalepi:::.shiny_germany_outline_geojson(),
        palette_variant,palette_alignment,display_metadata),envir=cache)
      state$result <- list(window=window,demographic=demographic,fit=fit,bundle=bundle,map_join=map_join,
        map_widget=get(mkey,envir=cache,inherits=FALSE),typology_mode=mode,typology_config=regionalepi:::.shiny_typology_config(mode),
        demographic_years=demographic_years,palette_variant=palette_variant,
        palette_alignment=palette_alignment,display_metadata=display_metadata,
        stability=stability,stability_metadata=stability_metadata,
        reporting_years=sort(unique(bundle$data$reporting_year)),
        analysis_range=request_range,
        loaded_selection=list(typology_mode=mode,
          demographic_period=if(mode=="dynamic")period_label else NULL,
          k=if(mode=="dynamic")as.integer(input$k) else NULL,
          demographic_source=source_mode),
        cache_keys=list(demographic=dkey,typology=tkey,surveillance=skey,map=mkey))
      if(is.null(state$selected_geo_id)||!state$selected_geo_id%in%map_join$data$geo_id) state$selected_geo_id<-"11000"
      state$selected_date <- request_range$start_date; setProgress(1,detail="Darstellung ist bereit.")
    }),error=function(e){state$technical_error<-conditionMessage(e);state$error<-regionalepi:::.shiny_app_format_error(e,"Die Live-Analyse")})
  })

  exploration <- reactive({ req(state$result); regionalepi:::.shiny_exploration_summaries(state$result$bundle,state$result$fit,state$result$analysis_range,state$result$display_metadata) })
  regional_membership <- reactive({
    req(input$regional_level)
    regionalepi:::.regional_comparison_membership(cache$crosswalk,input$regional_level)
  })
  observeEvent(input$regional_level, {
    choices <- regionalepi:::.regional_group_choices(input$regional_level,cache$crosswalk)
    focal <- isolate(input$regional_focal)
    if(is.null(focal)||!focal%in%unname(choices)) focal<-unname(choices)[[1L]]
    updateSelectInput(session,"regional_focal",choices=choices,selected=focal)
    comparisons<-isolate(input$regional_comparisons)
    comparisons<-regionalepi:::.reconcile_regional_comparisons(
      input$regional_level,focal,comparisons,unname(choices))
    updateSelectizeInput(session,"regional_comparisons",
      choices=choices[names(choices)!=names(choices)[match(focal,choices)]],
      selected=comparisons,server=FALSE)
  },ignoreInit=FALSE)
  observeEvent(input$regional_focal, {
    req(input$regional_level,input$regional_focal)
    choices<-regionalepi:::.regional_group_choices(input$regional_level,cache$crosswalk)
    allowed<-setdiff(unname(choices),input$regional_focal)
    selected<-regionalepi:::.reconcile_regional_comparisons(
      input$regional_level,input$regional_focal,input$regional_comparisons,
      unname(choices))
    updateSelectizeInput(session,"regional_comparisons",
      choices=choices[unname(choices)%in%allowed],selected=selected,server=FALSE)
  })
  output$regional_focal_control <- renderUI({
    choices<-regionalepi:::.regional_group_choices(input$regional_level,cache$crosswalk)
    selectInput("regional_focal","Ausgewählte Region",choices,selected=unname(choices)[[1L]])
  })
  output$regional_comparison_control <- renderUI({
    choices<-regionalepi:::.regional_group_choices(input$regional_level,cache$crosswalk)
    allowed<-choices[unname(choices)!=input$regional_focal]
    limit<-regionalepi:::.regional_comparison_limit(input$regional_level)
    selectizeInput("regional_comparisons","Vergleich mit",allowed,multiple=TRUE,
      options=list(maxItems=limit,placeholder=if(limit==3L)"Optional bis zu drei Regionen"else"Optional bis zu zwei Regionen"))
  })
  regional_selection <- reactive({
    req(state$result,input$regional_level,input$regional_focal)
    membership<-regional_membership()
    choices<-regionalepi:::.regional_group_choices(input$regional_level,cache$crosswalk)
    focal<-input$regional_focal
    if(length(focal)!=1L||!focal%in%unname(choices)) focal<-unname(choices)[[1L]]
    comparisons<-regionalepi:::.reconcile_regional_comparisons(
      input$regional_level,focal,input$regional_comparisons,unname(choices))
    list(membership=membership,focal=focal,comparisons=comparisons,
      focal_name=unique(membership$comparison_name[membership$comparison_id==focal]))
  })
  palette_colours <- function(ids) {metadata<-state$result$display_metadata;
    colours<-stats::setNames(metadata$display_colour,metadata$display_cluster_id);
    colours[metadata$display_cluster_id[metadata$display_cluster_id%in%as.character(ids)]]}
  display_data <- function(data,cluster_col="cluster_id")
    regionalepi:::.shiny_apply_display_metadata(data,state$result$display_metadata,cluster_col)
  output$load_status <- renderUI({if(state$busy)p("Daten werden geladen …") else if(!is.null(state$error))p(class="status-error",regionalepi:::.shiny_app_display_error(state$error,!is.null(state$result))) else if(is.null(state$result))p("Noch keine Analyse geladen.") else p(class="status-ok","Analyse geladen: ",state$result$fit$provenance$fit_id)})
  loaded_selection_changed <- reactive({
    if(is.null(state$result))return(FALSE)
    regionalepi:::.shiny_loaded_selection_changed(state$result,input$typology_mode,
      input$demographic_period,input$k,input$demographic_source)
  })
  output$analysis_wide_status <- renderUI({
    req(state$result)
    tagList(
      if(loaded_selection_changed())p(class="status-quality",
        "Auswahl geändert – Analyse aktualisieren. Die angezeigten Ergebnisse entsprechen weiterhin der zuvor geladenen Analyse."),
      if(regionalepi:::.shiny_loaded_analysis_is_provisional(state$result)) {
        status<-state$result$bundle$diagnostics$status_by_reporting_year
        provisional<-status[status$incidence_status=="provisional",,drop=FALSE]
        p(class="status-quality",paste(sprintf(
          "Vorläufig: Für Berichtsjahr %d wird die zuletzt verfügbare amtliche durchschnittliche Jahresbevölkerung %d als Bezugsbevölkerung verwendet.",
          provisional$reporting_year,provisional$population_year),collapse=" "))
      }
    )
  })
  output$range_status <- renderUI({r<-if(is.null(state$result))regionalepi:::.shiny_effective_analysis_range(analysis_range(),regionalepi:::.shiny_analysis_as_of_date()) else state$result$analysis_range;w<-if(is.null(state$result))selected_window() else state$result$window;early<-regionalepi:::.shiny_early_window_note(w,r);range_display<-if(identical(r$mode,"custom"))tagList(p(class="app-note",regionalepi:::.shiny_iso_week_display(r$start_date)," bis ",regionalepi:::.shiny_iso_week_display(if(!is.null(r$nominal_end_date))r$nominal_end_date else r$end_date)),p(class="app-note",format(r$start_date,"%d.%m.%Y"),"–",format(r$end_date,"%d.%m.%Y")))else p(class="app-note",r$label,": ",format(r$start_date,"%d.%m.%Y"),"–",format(r$end_date,"%d.%m.%Y")," (",r$start_iso," bis ",r$end_iso,")");running_notice<-if(isTRUE(r$is_truncated))if(is.null(early))"Laufender Beobachtungszeitraum: Die Analyse ist auf den aktuellen Analyse-Stichtag begrenzt. Der von SurvStat ausgewiesene Datenstand wird davon getrennt dokumentiert."else paste("Laufender Beobachtungszeitraum: Die Analyse ist auf den aktuellen Analyse-Stichtag begrenzt. Der von SurvStat ausgewiesene Datenstand wird davon getrennt dokumentiert.",early)else NULL;tagList(range_display,if(!is.null(running_notice))p(class="status-quality",running_notice))})
  output$demography_status <- renderUI({req(state$result);p(class="app-note","Demografie: ",regionalepi:::.shiny_demography_status(state$result$demographic))})
  output$district_map <- leaflet::renderLeaflet({req(state$result);state$result$map_widget})
  observe({req(state$result,state$selected_geo_id);session$sendCustomMessage("regionalepi-select",state$selected_geo_id)})
  output$map_attribution <- renderUI({p(class="app-note",cache$map$provenance$attribution," · Ländergrenzen: ",cache$states$provenance$source_layer," · ",a(cache$map$provenance$license,href=cache$map$provenance$license_url,target="_blank"))})

  output$analysis_heading <- renderUI({req(state$result);w<-state$result$window;if(state$result$typology_mode=="dissertation")context<-"Historische Referenztypologie (2017–2020) · ClD / ClJ / ClA" else {ids<-state$result$display_metadata$display_cluster_id;context<-paste0("Aktualisierte Typologie (",paste(ids,collapse=" / "),") · Referenzzeitraum ",format(min(state$result$demographic$summary$data$period_start),"%Y"),"–",format(max(state$result$demographic$summary$data$period_end),"%Y")," · k = ",length(ids)," · ",nrow(state$result$map_join$data)," Kreise")};tagList(h3("Demografische Regionaltypologie"),p(class="app-note",context),p(class="app-note","Epidemiologischer Beobachtungszeitraum: ",w$label))})
  output$period_heading <- renderUI({req(state$result);r<-analysis_range();rp<-reviewed_period();context<-regionalepi:::.shiny_period_context(input$pathogen,r,rp,state$result$typology_mode,state$result$demographic_years,length(unique(state$result$fit$assignments$display_cluster_id)));tagList(h3(context$title),p(class="app-note",context$subtitle),p(class="app-note",context$typology),p(class="app-note",if(state$result$typology_mode=="dissertation")"Historische SurvStat-Inzidenz"else"Inzidenz auf Basis der durchschnittlichen Jahresbevölkerung"),p(class="app-note","Beobachtungskontext: ",state$result$window$label))})
  output$cluster_summary <- renderTable({req(state$result);x<-state$result$display_metadata;data.frame(Cluster=x$display_cluster_id,`Anzahl Kreise`=x$n_districts,`Demografisches Profil`=x$profile_description,check.names=FALSE)},striped=TRUE,rownames=FALSE)

  output$profile_plot <- plotly::renderPlotly({req(state$result);p<-display_data(state$result$fit$profiles,"display_cluster_id");d<-regionalepi:::.shiny_app_indicator_display();p$label<-d$label[match(p$indicator_id,d$indicator_id)];p$text<-sprintf("Cluster: %s<br>Indikator: %s<br>z: %.2f<br>Mittelwert: %.2f<br>Median: %.2f<br>Kreise: %d",p$display_cluster_id,p$label,p$standardized_center,p$original_mean,p$original_median,p$cluster_size);wide<-stats::xtabs(standardized_center~display_cluster_id+label,p);plotly::layout(plotly::plot_ly(x=colnames(wide),y=rownames(wide),z=wide,type="heatmap",zmid=0,colors=regionalepi:::.shiny_signed_heatmap_colours(),text=matrix(p$text[match(paste(rep(rownames(wide),each=ncol(wide)),rep(colnames(wide),times=nrow(wide))),paste(p$display_cluster_id,p$label))],nrow=nrow(wide),byrow=TRUE),hoverinfo="text"),xaxis=list(title=""),yaxis=list(title="Cluster"))})
  output$profile_table <- renderTable({req(state$result);p<-display_data(state$result$fit$profiles,"display_cluster_id");p<-p[order(p$display_order),];d<-regionalepi:::.shiny_app_indicator_display();data.frame(Cluster=p$display_cluster_id,N=p$cluster_size,Indikator=d$label[match(p$indicator_id,d$indicator_id)],Einheit=d$unit[match(p$indicator_id,d$indicator_id)],Mittelwert=round(p$original_mean,2),Median=round(p$original_median,2),check.names=FALSE)},striped=TRUE,rownames=FALSE)
  output$cluster_warning <- renderUI({req(state$result);p(class="app-note",if(state$result$typology_mode=="dissertation")"Etablierte historische Referenzbezeichnungen; keine dynamische Umbenennung."else"Beschreibungen sind reproduzierbare Profiltexte, keine Clusteridentitäten.")})
  output$stability_summary <- renderTable({req(state$result);x<-state$result$stability$summary;data.frame(`k=3 Referenz`="k=3",Ziel=paste0("k=",x$target_k),`Ähnlichkeit der Partitionen (ARI)`=round(x$ari,3),`Dominante Herkunft (Kreise)`=x$dominant_ancestry_districts,`Größen der Zielcluster`=x$target_cluster_sizes,check.names=FALSE)},striped=TRUE,rownames=FALSE)
  output$stability_plot <- plotly::renderPlotly({req(state$result,state$result$typology_mode=="dynamic");regionalepi:::.shiny_stability_alluvial_widget(state$result$stability,state$result$stability_metadata)})
  output$stability_note <- renderUI({req(state$result,state$result$typology_mode=="dynamic");tagList(p(class="app-note","k=3 ist die geprüfte interpretative Referenz, nicht ein statistisch optimales oder korrektes k. k=2, k=4 und k=5 sind unabhängig angepasste Alternativen; die Flussbreite zeigt die Neuordnung anhand der Zahl gemeinsam zugeordneter Kreise."),p(class="app-note",regionalepi:::.shiny_stability_interpretation(state$result$stability)))})

  output$demographic_distribution_plot <- plotly::renderPlotly({
    req(state$result);x<-merge(state$result$demographic$summary$data,state$result$fit$assignments[c("geo_id","display_cluster_id")],by="geo_id");x<-display_data(x,"display_cluster_id");x$geo_name<-state$result$map_join$data$geo_name[match(x$geo_id,state$result$map_join$data$geo_id)];d<-regionalepi:::.shiny_app_indicator_display();x$indicator_label<-factor(d$label[match(x$indicator_id,d$indicator_id)],levels=d$label);x$unit<-d$unit[match(x$indicator_id,d$indicator_id)];density<-x$indicator_id=="population_density";density_text<-formatC(x$indicator_value,format="f",digits=0,big.mark=".",decimal.mark=",");x$text<-sprintf("%s<br>AGS: %s<br>Cluster: %s<br>%s: %s %s%s",x$geo_name,x$geo_id,x$display_cluster_id,x$indicator_label,ifelse(density,density_text,sprintf("%.2f",x$indicator_value)),x$unit,ifelse(density&identical(input$density_scale,"log10"),"<br>Darstellung: logarithmische Achse",""));cols<-palette_colours(x$display_cluster_id)
    make_plot<-function(z,log_density=FALSE){layers<-regionalepi:::.shiny_adaptive_distribution_layers(z,"display_cluster_id","indicator_value");g<-ggplot2::ggplot(z,ggplot2::aes(display_cluster_id,indicator_value,fill=display_cluster_id,colour=display_cluster_id));if(nrow(layers$violins))g<-g+ggplot2::geom_violin(data=layers$violins,alpha=.2,trim=FALSE,show.legend=FALSE);if(nrow(layers$boxplots))g<-g+ggplot2::geom_boxplot(data=layers$boxplots,width=.15,outlier.shape=NA,alpha=.65,show.legend=FALSE);if(nrow(layers$medians))g<-g+ggplot2::geom_point(data=layers$medians,ggplot2::aes(display_cluster_id,.display_median),inherit.aes=FALSE,shape=23,size=3,fill="white",colour="#4B5563",stroke=.9,show.legend=FALSE);g<-g+suppressWarnings(ggplot2::geom_jitter(data=layers$points,ggplot2::aes(key=geo_id,text=text),width=.09,alpha=.45,size=1,show.legend=FALSE))+ggplot2::scale_colour_manual(values=cols)+ggplot2::scale_fill_manual(values=cols)+ggplot2::labs(x="Cluster",y=as.character(z$indicator_label[[1L]]))+ggplot2::theme_minimal()+ggplot2::theme(legend.position="none");sel<-layers$points[layers$points$geo_id==state$selected_geo_id,,drop=FALSE];if(nrow(sel))g<-g+ggplot2::geom_point(data=sel,shape=21,fill="#FFFFFF",colour="#111111",size=3,stroke=1.1,show.legend=FALSE);if(log_density){if(any(!is.finite(z$indicator_value)|z$indicator_value<=0))stop("Logarithmic population-density display requires positive finite values.",call.=FALSE);g<-g+ggplot2::scale_y_log10(breaks=c(50,100,250,500,1000,2500,5000),labels=c("50","100","250","500","1.000","2.500","5.000"))};g}
    if(!identical(input$density_scale,"log10")){g<-make_plot(x)+ggplot2::facet_wrap(~indicator_label,scales="free_y",ncol=1)+ggplot2::labs(y=NULL)+ggplot2::theme(panel.spacing.y=grid::unit(1.25,"lines"),strip.text=ggplot2::element_text(face="bold",size=11));return(plotly::ggplotly(g,tooltip="text",source="demography"))}
    widgets<-lapply(d$indicator_id,function(id)plotly::ggplotly(make_plot(x[x$indicator_id==id,,drop=FALSE],id=="population_density"),tooltip="text",source="demography"));plotly::subplot(widgets,nrows=3L,shareX=FALSE,shareY=FALSE,titleY=TRUE,margin=.07)
  })

  output$incidence_plot <- plotly::renderPlotly({
    x<-exploration();w<-display_data(x$weekly);cols<-palette_colours(w$cluster_id)
    metadata<-state$result$display_metadata;legend_labels<-stats::setNames(metadata$display_label,metadata$display_cluster_id)
    w$text<-sprintf("%s · %s<br>Cluster: %s<br>Median: %.2f<br>Q1/Q3: %.2f / %.2f<br>Kreise beobachtet/erwartet/fehlend: %d/%d/%d<br>Gemeldete Fälle in den beobachteten Kreisen: %s",format(w$date),regionalepi:::.iso_week_label(w$date),w$cluster_id,w$median_incidence,w$q1_incidence,w$q3_incidence,w$observed_districts,w$expected_districts,w$missing_districts,ifelse(is.na(w$reported_cases_observed),"fehlend",w$reported_cases_observed))
    g<-ggplot2::ggplot(w,ggplot2::aes(date,median_incidence,colour=cluster_id,fill=cluster_id,group=cluster_id,text=text))+ggplot2::geom_ribbon(ggplot2::aes(ymin=q1_incidence,ymax=q3_incidence),alpha=.15,colour=NA,show.legend=FALSE)+ggplot2::geom_line(linewidth=.8)+ggplot2::scale_colour_manual(values=cols,labels=legend_labels)+ggplot2::scale_fill_manual(values=cols,guide="none")+ggplot2::labs(x=NULL,y="Mediane wöchentliche Kreisinzidenz",colour="Cluster")+ggplot2::theme_minimal()+ggplot2::theme(legend.position="bottom")
    rp<-reviewed_period();if(!is.null(rp)&&nrow(rp))g<-g+ggplot2::annotate("rect",xmin=rp$start_date,xmax=rp$end_date,ymin=-Inf,ymax=Inf,alpha=.08,fill="#5B6770")
    if(isTRUE(input$district_overlay)&&!is.null(state$selected_geo_id)){d<-x$data[x$data$geo_id==state$selected_geo_id,,drop=FALSE];d$text<-sprintf("%s<br>%s · %s<br>Inzidenz: %s je 100.000<br>Gemeldete Fälle: %s<br>Cluster: %s",d$geo_name,d$reporting_year,sprintf("KW%02d",d$reporting_week),ifelse(is.na(d$incidence),"fehlend",round(d$incidence,2)),ifelse(is.na(d$cases),"fehlend",d$cases),d$cluster_id);g<-g+suppressWarnings(ggplot2::geom_line(data=d,ggplot2::aes(date,incidence,group=geo_id,text=text,key=geo_id),inherit.aes=FALSE,colour="#111111",linewidth=.55))+suppressWarnings(ggplot2::geom_point(data=d,ggplot2::aes(date,incidence,text=text,key=geo_id),inherit.aes=FALSE,colour="#111111",size=1))}
    widget<-regionalepi:::.shiny_finalize_incidence_legend(plotly::ggplotly(g,tooltip="text",source="district-time"))
    plotly::layout(widget,xaxis=list(rangeslider=list(visible=TRUE),autorange=TRUE))
  })
  output$incidence_warning <- renderUI({x<-exploration()$weekly;bad<-x$expected_districts>0&x$observed_districts/x$expected_districts<.8;p(class=if(any(bad))"status-quality"else"app-note",if(any(bad))paste0("Datenvollständigkeit: In ",sum(bad)," Wochen-Cluster-Kombinationen liegen für weniger als 80 % der Kreise beobachtete Werte vor. Fehlende Werte bleiben NA.")else"Fehlende Inzidenzen bleiben NA; Median und empirischer IQR verwenden beobachtete Kreise.")})
  output$incidence_early_window_note <- renderUI({req(state$result);early<-regionalepi:::.shiny_early_window_note(state$result$window,state$result$analysis_range);if(is.null(early))NULL else p(class="status-quality",paste("Früher Beobachtungsstand:",early))})
  output$period_distribution_plot <- plotly::renderPlotly({x<-display_data(exploration()$district_period);x$text<-sprintf("%s<br>AGS: %s<br>Cluster: %s<br>Median: %s<br>Gemeldete Fälle in beobachteten Wochen: %s<br>Daten vorhanden: %d von %d Wochen%s",x$geo_name,x$geo_id,x$cluster_id,round(x$median_period_incidence,2),ifelse(is.na(x$cumulative_observed_cases),"fehlend",x$cumulative_observed_cases),x$observed_case_week_count,x$expected_week_count,ifelse(x$missing_case_week_count>0,paste0("<br>Fehlende Wochen: ",x$missing_case_week_count),""));cols<-palette_colours(x$cluster_id);layers<-regionalepi:::.shiny_adaptive_distribution_layers(x,"cluster_id","median_period_incidence");g<-ggplot2::ggplot(x,ggplot2::aes(cluster_id,median_period_incidence,fill=cluster_id,colour=cluster_id));if(nrow(layers$violins))g<-g+ggplot2::geom_violin(data=layers$violins,alpha=.2,trim=FALSE,na.rm=TRUE,show.legend=FALSE);if(nrow(layers$boxplots))g<-g+ggplot2::geom_boxplot(data=layers$boxplots,width=.15,outlier.shape=NA,alpha=.65,na.rm=TRUE,show.legend=FALSE);if(nrow(layers$medians))g<-g+ggplot2::geom_point(data=layers$medians,ggplot2::aes(cluster_id,.display_median),inherit.aes=FALSE,shape=23,size=3,fill="white",colour="#4B5563",stroke=.9,show.legend=FALSE);g<-g+suppressWarnings(ggplot2::geom_jitter(data=layers$points,ggplot2::aes(key=geo_id,text=text),width=.09,alpha=.5,size=1,na.rm=TRUE,show.legend=FALSE))+ggplot2::scale_colour_manual(values=cols)+ggplot2::scale_fill_manual(values=cols)+ggplot2::labs(x="Cluster",y="Median der wöchentlichen Kreisinzidenz")+ggplot2::theme_minimal()+ggplot2::theme(legend.position="none");sel<-layers$points[layers$points$geo_id==state$selected_geo_id,,drop=FALSE];if(nrow(sel))g<-g+ggplot2::geom_point(data=sel,shape=21,fill="#FFFFFF",colour="#111111",size=3,stroke=1.1,show.legend=FALSE);plotly::ggplotly(g,tooltip="text",source="period")})
  heat <- function(x,y,z,text,source,zmid=0,colors=regionalepi:::.shiny_signed_heatmap_colours(),customdata=NULL,xgap=0){rp<-reviewed_period();shapes<-list();if(!is.null(rp)&&nrow(rp))shapes<-lapply(c(rp$start_date,rp$end_date),function(day)list(type="line",xref="x",yref="paper",x0=format(day),x1=format(day),y0=0,y1=1,line=list(color="#4B5563",width=1,dash="dot")));plotly::layout(plotly::plot_ly(x=x,y=y,z=z,type="heatmap",zmid=zmid,colors=colors,text=text,hoverinfo="text",customdata=customdata,source=source,connectgaps=FALSE,xgap=xgap,ygap=1),xaxis=list(title="",rangeslider=list(visible=TRUE)),yaxis=list(title=""),shapes=shapes,plot_bgcolor="#D1D5DB")}
  output$relative_heatmap <- plotly::renderPlotly({w<-display_data(exploration()$weekly);w$hover<-sprintf("Cluster: %s<br>%s<br>Cluster-Median: %.2f<br>Median aller Kreise: %.2f<br>Differenz: %.2f<br>Kreise: %d/%d<br>Gemeldete Fälle in beobachteten Kreisen: %s",w$cluster_id,regionalepi:::.iso_week_label(w$date),w$median_incidence,w$all_district_median,w$relative_activity,w$observed_districts,w$expected_districts,ifelse(is.na(w$reported_cases_observed),"fehlend",w$reported_cases_observed));g<-regionalepi:::.shiny_heatmap_grid(w,"cluster_id","date","relative_activity","hover");heat(g$columns,g$rows,g$z,g$text,"relative",xgap=regionalepi:::.shiny_weekly_heatmap_xgap(length(g$columns)))})
  output$pairwise_heatmap <- plotly::renderPlotly({req(state$result);metadata<-state$result$display_metadata;req(nrow(metadata),length(metadata$display_cluster_id)>=2L);regionalepi:::.shiny_pairwise_heatmap_widget(exploration()$pairwise,metadata,reviewed_period())})
  output$district_heatmap <- plotly::renderPlotly({
    audit<-regionalepi:::.shiny_district_heatmap_data(
      exploration()$data,state$result$display_metadata,state$selected_geo_id)
    regionalepi:::.shiny_district_heatmap_widget(audit)
  })
  regional_composition <- reactive({
    s<-regional_selection();a<-regionalepi:::.shiny_current_display_assignments(
      state$result$map_join)
    x<-regionalepi::summarize_cluster_composition_by_region(a,input$regional_level,cache$crosswalk)
    x[x$comparison_id==s$focal,,drop=FALSE]
  })
  output$regional_demographic_heading <- renderUI({s<-regional_selection();tagList(
    h3("Demografische Struktur: ",s$focal_name),
    p(class="app-note","Dargestellt sind die Anteile der bundesweit bestimmten demografischen Regionaltypen an den Kreisen der ausgewählten Region. Die Prozentwerte beziehen sich auf Kreise, nicht auf Bevölkerungsanteile."),
    p(class="app-note","Die demografischen Regionaltypen werden bundesweit bestimmt und für die regionale Analyse nicht neu berechnet."))})
  output$regional_composition_plot <- plotly::renderPlotly({
    regionalepi:::.shiny_state_composition_widget(regional_composition(),state$result$display_metadata)
  })
  output$regional_analysis_context <- renderUI({
    req(state$result)
    p(class="status-ok",regionalepi:::.shiny_loaded_analysis_context(state$result))
  })
  output$regional_demographic_distribution_plot <- plotly::renderPlotly({
    s<-regional_selection(); ids<-s$membership$geo_id[s$membership$comparison_id==s$focal]
    all<-merge(state$result$demographic$summary$data,state$result$fit$assignments[c("geo_id","display_cluster_id")],by="geo_id")
    all<-display_data(all,"display_cluster_id"); all$geo_name<-state$result$map_join$data$geo_name[match(all$geo_id,state$result$map_join$data$geo_id)]
    x<-all[all$geo_id%in%ids,,drop=FALSE]; d<-regionalepi:::.shiny_app_indicator_display()
    x$indicator_label<-factor(d$label[match(x$indicator_id,d$indicator_id)],levels=d$label)
    all$indicator_label<-factor(d$label[match(all$indicator_id,d$indicator_id)],levels=d$label)
    x$unit<-d$unit[match(x$indicator_id,d$indicator_id)]
    x$text<-sprintf("%s<br>AGS: %s<br>Bundesweit bestimmter Regionaltyp: %s<br>Wert: %.2f %s",x$geo_name,x$geo_id,x$display_cluster_id,x$indicator_value,x$unit)
    cols<-palette_colours(x$display_cluster_id)
    national<-stats::aggregate(all$indicator_value,all[c("display_cluster_id","indicator_id","indicator_label")],stats::median)
    names(national)[[4L]]<-"national_median"
    national$unit<-d$unit[match(national$indicator_id,d$indicator_id)]
    national$text<-sprintf("Deutschlandreferenz<br>Nationaler Regionaltyp: %s<br>Nationaler Median: %.2f %s",national$display_cluster_id,national$national_median,national$unit)
    make_plot<-function(z,n,log_density=FALSE){layers<-regionalepi:::.shiny_adaptive_distribution_layers(z,"display_cluster_id","indicator_value");q<-ggplot2::ggplot(z,ggplot2::aes(display_cluster_id,indicator_value,colour=display_cluster_id,fill=display_cluster_id));if(nrow(layers$violins))q<-q+ggplot2::geom_violin(data=layers$violins,alpha=.2,trim=FALSE,show.legend=FALSE);if(nrow(layers$boxplots))q<-q+ggplot2::geom_boxplot(data=layers$boxplots,width=.15,outlier.shape=NA,alpha=.55,show.legend=FALSE);if(nrow(layers$medians))q<-q+ggplot2::geom_point(data=layers$medians,ggplot2::aes(display_cluster_id,.display_median),inherit.aes=FALSE,shape=23,size=3,fill="white",colour="#6B7280",stroke=.9,show.legend=FALSE);q<-q+suppressWarnings(ggplot2::geom_jitter(data=layers$points,ggplot2::aes(key=geo_id,text=text),width=.1,alpha=.65,size=1.5,show.legend=FALSE))+suppressWarnings(ggplot2::geom_point(data=n,ggplot2::aes(display_cluster_id,national_median,text=text),inherit.aes=FALSE,shape=23,size=3.5,fill="white",colour="#374151",stroke=1.1,show.legend=FALSE))+ggplot2::scale_colour_manual(values=cols)+ggplot2::scale_fill_manual(values=cols)+ggplot2::labs(x="Demografischer Regionaltyp",y=NULL)+ggplot2::theme_minimal()+ggplot2::theme(legend.position="none");if(log_density)q<-q+ggplot2::scale_y_log10();q}
    if(identical(input$regional_density_scale,"log10")){
      # Faceted mixed transformations require separate widgets, as in the national view.
      widgets<-lapply(d$indicator_id,function(id){z<-x[x$indicator_id==id,,drop=FALSE];n<-national[national$indicator_id==id,,drop=FALSE];q<-make_plot(z,n,id=="population_density")+ggplot2::labs(y=d$label[d$indicator_id==id]);plotly::ggplotly(q,tooltip="text")});return(plotly::subplot(widgets,nrows=3L,shareX=FALSE,shareY=FALSE,titleY=TRUE,margin=.07))
    }
    layers<-regionalepi:::.shiny_adaptive_distribution_layers(x,c("display_cluster_id","indicator_id"),"indicator_value")
    g<-ggplot2::ggplot(x,ggplot2::aes(display_cluster_id,indicator_value,colour=display_cluster_id,fill=display_cluster_id));if(nrow(layers$violins))g<-g+ggplot2::geom_violin(data=layers$violins,alpha=.2,trim=FALSE,show.legend=FALSE);if(nrow(layers$boxplots))g<-g+ggplot2::geom_boxplot(data=layers$boxplots,width=.15,outlier.shape=NA,alpha=.55,show.legend=FALSE);if(nrow(layers$medians))g<-g+ggplot2::geom_point(data=layers$medians,ggplot2::aes(display_cluster_id,.display_median),inherit.aes=FALSE,shape=23,size=3,fill="white",colour="#6B7280",stroke=.9,show.legend=FALSE);g<-g+suppressWarnings(ggplot2::geom_jitter(data=layers$points,ggplot2::aes(key=geo_id,text=text),width=.1,alpha=.65,size=1.5,show.legend=FALSE))+suppressWarnings(ggplot2::geom_point(data=national,ggplot2::aes(display_cluster_id,national_median,text=text),inherit.aes=FALSE,shape=23,size=3.5,fill="white",colour="#374151",stroke=1.1,show.legend=FALSE))+ggplot2::scale_colour_manual(values=cols)+ggplot2::scale_fill_manual(values=cols)+ggplot2::facet_wrap(~indicator_label,scales="free_y",ncol=1)+ggplot2::labs(x="Demografischer Regionaltyp",y=NULL)+ggplot2::theme_minimal()+ggplot2::theme(legend.position="none");plotly::ggplotly(g,tooltip="text")
  })
  regional_cluster_weekly <- reactive({
    s<-regional_selection();regionalepi:::.summarize_weekly_regional_incidence(
      exploration()$data,s$membership,s$focal,TRUE)
  })
  output$regional_cluster_heading <- renderUI({s<-regional_selection();tagList(
    h3("Infektionsgeschehen nach demografischem Regionaltyp: ",s$focal_name),
    p(class="app-note","Median und Interquartilsabstand der wöchentlichen Kreisinzidenzen innerhalb der ausgewählten Region."),
    p(class="app-note","Die Clusterzuordnung wird bundesweit bestimmt und durch die regionale Auswahl nicht verändert."))})
  output$regional_cluster_time_plot <- plotly::renderPlotly({
    w<-display_data(regional_cluster_weekly()); w<-w[w$expected_districts>0,,drop=FALSE]
    w$text<-sprintf("%s<br>Cluster: %s<br>Median: %s<br>Q1/Q3: %s / %s<br>Kreise beobachtet/erwartet/fehlend: %d/%d/%d<br>Vollständigkeit: %.1f%%",regionalepi:::.iso_week_label(w$date),w$cluster_id,ifelse(is.na(w$median_incidence),"fehlend",sprintf("%.2f",w$median_incidence)),ifelse(is.na(w$q1_incidence),"fehlend",sprintf("%.2f",w$q1_incidence)),ifelse(is.na(w$q3_incidence),"fehlend",sprintf("%.2f",w$q3_incidence)),w$observed_districts,w$expected_districts,w$missing_districts,100*w$completeness)
    cols<-palette_colours(w$cluster_id); labels<-stats::setNames(state$result$display_metadata$display_label,state$result$display_metadata$display_cluster_id)
    g<-ggplot2::ggplot(w,ggplot2::aes(date,median_incidence,colour=cluster_id,fill=cluster_id,group=cluster_id,text=text))+ggplot2::geom_ribbon(ggplot2::aes(ymin=q1_incidence,ymax=q3_incidence),alpha=.14,colour=NA,show.legend=FALSE)+ggplot2::geom_line(linewidth=.8,na.rm=TRUE)+ggplot2::scale_colour_manual(values=cols,labels=labels)+ggplot2::scale_fill_manual(values=cols,guide="none")+ggplot2::labs(x=NULL,y="Median der wöchentlichen Kreisinzidenzen",colour="Bundesweit bestimmter Regionaltyp")+ggplot2::theme_minimal()+ggplot2::theme(legend.position="bottom")
    regionalepi:::.shiny_finalize_incidence_legend(plotly::ggplotly(g,tooltip="text"))
  })
  regional_relative_activity <- reactive({
    s<-regional_selection();regionalepi:::.regional_relative_activity(
      exploration()$data,s$membership,s$focal)
  })
  output$regional_relative_activity_plot <- plotly::renderPlotly({
    grid<-regionalepi:::.shiny_regional_temporal_heatmap_data(
      regional_relative_activity(),state$result$display_metadata)
    limit<-max(abs(grid$z),na.rm=TRUE);if(!is.finite(limit)||limit==0)limit<-1
    plotly::layout(plotly::plot_ly(x=grid$columns,y=grid$rows,z=grid$z,
      type="heatmap",zmin=-limit,zmax=limit,zmid=0,
      colors=regionalepi:::.shiny_signed_heatmap_colours(),
      colorbar=list(title="Differenz zum<br>regionalen Median"),
      text=grid$text,hoverinfo="text",connectgaps=FALSE,
      xgap=regionalepi:::.shiny_weekly_heatmap_xgap(length(grid$columns)),ygap=1,
      source="regional-relative-activity"),
      xaxis=list(title="",rangeslider=list(visible=FALSE)),
      yaxis=list(title="",categoryorder="array",categoryarray=rev(grid$rows)),
      plot_bgcolor="#D1D5DB")
  })
  output$regional_cluster_quality <- renderUI({w<-regional_cluster_weekly();bad<-w$expected_districts>0&w$completeness<.8;absent<-setdiff(state$result$display_metadata$display_cluster_id,unique(w$cluster_id[w$expected_districts>0]));tagList(p(class=if(any(bad))"status-quality"else"app-note",if(any(bad))paste0("Bei ",sum(bad)," Kombinationen aus Woche und Regionaltyp liegen für weniger als 80 % der zugehörigen Kreise Beobachtungen vor. Die Mediane werden ausschließlich aus beobachteten Kreisen berechnet; fehlende Werte werden nicht als Null interpretiert.")else"Für alle Kombinationen aus Woche und Regionaltyp liegen Beobachtungen für mindestens 80 % der zugehörigen Kreise vor."),if(length(absent))p(class="app-note","In der ausgewählten Region nicht vertreten: ",paste(absent,collapse=", "),"."))})
  regional_context_weekly <- reactive({
    s<-regional_selection();ids<-c(s$focal,s$comparisons)
    regional<-regionalepi:::.summarize_weekly_regional_incidence(exploration()$data,s$membership,ids,FALSE)
    rbind(regional,regionalepi:::.summarize_germany_weekly_incidence(exploration()$data))
  })
  output$regional_context_time_plot <- plotly::renderPlotly({
    w<-regional_context_weekly();s<-regional_selection();w$text<-sprintf("%s<br>%s<br>Median: %s<br>Q1/Q3: %s / %s<br>Kreise beobachtet/erwartet/fehlend: %d/%d/%d<br>Vollständigkeit: %.1f%%",w$comparison_name,regionalepi:::.iso_week_label(w$date),ifelse(is.na(w$median_incidence),"fehlend",sprintf("%.2f",w$median_incidence)),ifelse(is.na(w$q1_incidence),"fehlend",sprintf("%.2f",w$q1_incidence)),ifelse(is.na(w$q3_incidence),"fehlend",sprintf("%.2f",w$q3_incidence)),w$observed_districts,w$expected_districts,w$missing_districts,100*w$completeness)
    w$role<-ifelse(w$comparison_id==s$focal,"Ausgewählte Region",ifelse(w$comparison_id=="DE","Deutschlandreferenz","Vergleichsregion"));w$text<-paste0(w$text,"<br>Rolle: ",w$role);w$comparison_name<-factor(w$comparison_name,levels=unique(w$comparison_name))
    comparison_names<-s$membership$comparison_name[match(s$comparisons,s$membership$comparison_id)]
    style<-regionalepi:::.shiny_regional_context_styles(
      c(s$focal_name,comparison_names,"Deutschland"),s$focal_name)
    w$comparison_name<-factor(as.character(w$comparison_name),levels=style$comparison_name)
    colours<-stats::setNames(style$colour,style$comparison_name);types<-stats::setNames(style$linetype,style$comparison_name);widths<-stats::setNames(style$linewidth,style$comparison_name)
    g<-ggplot2::ggplot(w,ggplot2::aes(date,median_incidence,colour=comparison_name,linetype=comparison_name,linewidth=comparison_name,group=comparison_name,text=text))+ggplot2::geom_line(na.rm=TRUE)+ggplot2::scale_colour_manual(values=colours)+ggplot2::scale_linewidth_manual(values=widths,guide="none")+ggplot2::scale_linetype_manual(values=types)+ggplot2::labs(x=NULL,y="Median der wöchentlichen Kreisinzidenzen",colour="Vergleichseinheit",linetype="Vergleichseinheit")+ggplot2::theme_minimal()+ggplot2::theme(legend.position="bottom")
    regionalepi:::.shiny_clean_named_legend(plotly::ggplotly(g,tooltip="text"),style$comparison_name)
  })
  output$regional_context_quality <- renderUI({w<-regional_context_weekly();bad<-w$completeness<.8;p(class=if(any(bad))"status-quality"else"app-note",if(any(bad))paste0("Bei ",sum(bad)," Kombinationen aus Woche und dargestellter Vergleichsregion liegen für weniger als 80 % der zugehörigen Kreise Beobachtungen vor. Die Mediane werden ausschließlich aus beobachteten Kreisen berechnet; fehlende Werte werden nicht als Null interpretiert.")else"Für alle Kombinationen aus Woche und dargestellter Vergleichsregion liegen Beobachtungen für mindestens 80 % der zugehörigen Kreise vor.")})
  regional_districts <- reactive({s<-regional_selection();x<-regionalepi::summarize_state_cluster_incidence(exploration()$data,cache$crosswalk);x[x$geo_id%in%s$membership$geo_id[s$membership$comparison_id==s$focal],,drop=FALSE]})
  output$regional_district_heading <- renderUI({s<-regional_selection();h3("Kreise: ",s$focal_name)})
  output$regional_district_plot <- plotly::renderPlotly({
    x<-display_data(regional_districts());x$text<-sprintf("%s<br>AGS: %s<br>Bundesweit bestimmter Regionaltyp: %s<br>Periodenmedian: %s<br>Gemeldete Fälle: %s<br>Beobachtete/erwartete/fehlende Wochen: %d/%d/%d",x$geo_name,x$geo_id,x$cluster_id,ifelse(is.na(x$period_median_incidence),"fehlend",sprintf("%.2f",x$period_median_incidence)),ifelse(is.na(x$cumulative_reported_cases),"fehlend",x$cumulative_reported_cases),x$observed_weeks,x$expected_weeks,x$missing_weeks)
    layers<-regionalepi:::.shiny_adaptive_distribution_layers(x,"cluster_id","period_median_incidence");cols<-palette_colours(x$cluster_id);g<-ggplot2::ggplot(x,ggplot2::aes(cluster_id,period_median_incidence,colour=cluster_id,fill=cluster_id));if(nrow(layers$violins))g<-g+ggplot2::geom_violin(data=layers$violins,width=.85,alpha=.2,trim=FALSE,na.rm=TRUE,show.legend=FALSE);if(nrow(layers$boxplots))g<-g+ggplot2::geom_boxplot(data=layers$boxplots,width=.18,outlier.shape=NA,alpha=.35,na.rm=TRUE,show.legend=FALSE);if(nrow(layers$medians))g<-g+ggplot2::geom_point(data=layers$medians,ggplot2::aes(cluster_id,.display_median),inherit.aes=FALSE,shape=23,size=3,fill="white",colour="#4B5563",stroke=.9,show.legend=FALSE);g<-g+suppressWarnings(ggplot2::geom_jitter(data=layers$points,ggplot2::aes(key=geo_id,text=text),width=.09,alpha=.7,size=1.7,na.rm=TRUE,show.legend=FALSE))+ggplot2::scale_colour_manual(values=cols)+ggplot2::scale_fill_manual(values=cols)+ggplot2::labs(x="Demografischer Regionaltyp",y="Median der wöchentlichen Kreisinzidenzen")+ggplot2::theme_minimal()+ggplot2::theme(legend.position="none");plotly::ggplotly(g,tooltip="text")
  })

  output$district_detail <- renderUI({req(state$result,state$selected_geo_id);id<-state$selected_geo_id;a<-state$result$map_join$data[state$result$map_join$data$geo_id==id,,drop=FALSE];p<-exploration()$district_period;v<-p[p$geo_id==id,,drop=FALSE];weekly<-exploration()$data;point<-weekly[weekly$geo_id==id&weekly$date==state$selected_date,,drop=FALSE];tagList(h4(a$geo_name),p(a$state_name),p(strong("AGS: "),id),p(strong("Cluster: "),a$display_cluster_id),if(nrow(point))tagList(p(strong(regionalepi:::.iso_week_label(point$date))),p("Inzidenz: ",regionalepi:::.shiny_format_number(point$incidence,1L)," je 100.000"),p("Gemeldete Fälle: ",regionalepi:::.shiny_format_number(point$cases))),if(nrow(v))tagList(p(strong("Median der wöchentlichen Inzidenz: "),regionalepi:::.shiny_format_number(v$median_period_incidence,2L)),p(strong(if(v$missing_case_week_count>0)"Gemeldete Fälle in beobachteten Wochen: "else"Gemeldete Fälle im Analysezeitraum: "),regionalepi:::.shiny_format_number(v$cumulative_observed_cases)),p("Daten vorhanden: ",v$observed_case_week_count," von ",v$expected_week_count," Wochen"),if(v$missing_case_week_count>0)p("Fehlende Wochen: ",v$missing_case_week_count)))})
  output$methods_methodology <- renderUI({tagList(
    div(class="panel-card method-grid",h3("Demografische Typologie"),
      p("Die Typologie basiert auf Bevölkerungsdichte, Durchschnittsalter und Jugendquotient. Die drei Indikatoren werden über alle einbezogenen Kreise Deutschlands z-standardisiert. Auf dieser Grundlage werden mit k-Means demografische Regionaltypen für die gewählte Clusterzahl ermittelt. Die methodische Konzeption basiert auf der demografischen Regionaltypologie von ",a("Dettmann (2026)",href="https://doi.org/10.17169/refubium-51449",target="_blank",rel="noopener noreferrer"),"."),
      p(strong("Historische Referenztypologie (2017–2020)")),
      p("Die historische Referenztypologie reproduziert die für 2017–2020 festgelegte Drei-Cluster-Lösung nach ",a("Dettmann (2026)",href="https://doi.org/10.17169/refubium-51449",target="_blank",rel="noopener noreferrer"),". Indikatoren, Clusterzahl und Berechnungsverfahren sind für diese Reproduktion fest vorgegeben."),
      p(class="app-note","Die Typologie wird für alle Kreise Deutschlands bestimmt; eine spätere Auswahl einzelner Regionen dient ausschließlich der Darstellung und verändert die Clusterzuordnung nicht.")),
    div(class="panel-card method-grid",h3("Infektionsgeschehen"),
      p("Die epidemiologische Beobachtungseinheit ist der Kreis. Für aktuelle Analysen wird die wöchentliche Inzidenz aus den über SurvStat bezogenen Fallzahlen und der amtlichen durchschnittlichen Jahresbevölkerung des jeweiligen Berichtsjahres berechnet. Für die historische Referenzanalyse wird dagegen die von SurvStat bereitgestellte historische Inzidenz verwendet."),
      p(class="app-note","Leere Zellen in den geprüften vollständigen SurvStat-Fallzahlexporten werden als null gemeldete Fälle interpretiert, sofern die vollständige Quellmatrix mit den ausgewiesenen Summen übereinstimmt. Tatsächlich fehlende Beobachtungen bleiben NA."),
      h4("Vordefinierte epidemiologische Zeiträume"),
      p("Für Influenza können die von der RKI-Arbeitsgemeinschaft Influenza (AGI) definierten Influenzawellen ausgewählt werden. Für COVID-19 sind die retrospektive Phaseneinteilung der Pandemie sowie definierte COVID-19-Wellen des RKI hinterlegt. Die Auswahl wird nur angezeigt, wenn für den gewählten Beobachtungszeitraum ein entsprechender Zeitraum verfügbar ist."),
      p(class="app-note","Quellen: AGI-Saison- und Wochenberichte für die Influenzawellen; RKI, ",a("Epidemiologisches Bulletin 38/2022",href="https://edoc.rki.de/bitstream/handle/176904/10260/EB-38-2022-Phaseneinteilung.pdf?sequence=1",target="_blank",rel="noopener noreferrer")," für die retrospektive Phaseneinteilung der COVID-19-Pandemie; RKI, Epidemiologisches Bulletin 35/2025 für die COVID-19-Wellen 2023/24 und 2024/25."),
      p("Vergleiche zwischen demografischen Regionaltypen basieren auf dem Median der Kreisinzidenzen. Interquartilsabstände beschreiben die empirische Verteilung der Kreise und sind keine Konfidenzintervalle. Die bundesweite relative Aktivität zeigt die Abweichung des wöchentlichen Cluster-Medians vom Median aller beobachteten Kreise Deutschlands. Paarweise Vergleiche zeigen die Differenz zwischen zwei Cluster-Medianen. Kreisbezogene Zusammenfassungen verwenden den Median der beobachteten Wocheninzidenzen im gewählten Analysezeitraum.")),
    div(class="panel-card method-grid",h3("Regionale Analyse"),
      p("Die regionalen Ansichten ermöglichen deskriptive Vergleiche für vier Großregionen nach der Einteilung der RKI-Arbeitsgemeinschaft Influenza (AGI), zwölf Ländergruppen und die 16 Bundesländer. Die demografischen Regionaltypen werden weiterhin bundesweit bestimmt; die regionale Auswahl beschränkt lediglich die dargestellten Kreise."),
      p("Die relative Aktivität innerhalb einer Region beschreibt die Abweichung des wöchentlichen Cluster-Medians vom Median aller beobachteten Kreise dieser Region. Positive Werte kennzeichnen eine höhere, negative Werte eine niedrigere mediane Inzidenz. Die dargestellten regionalen Bezugswerte sind aus Kreisdaten berechnete Mediane und keine amtlichen regionalen Inzidenzen.")),
    div(class="panel-card method-grid",h3("Interpretationsgrenzen"),
      p("Die Anwendung unterstützt die deskriptive Exploration zeitlicher, räumlicher und typologiespezifischer Unterschiede im regionalen Infektionsgeschehen. Sie ermöglicht Vergleiche zwischen demografischen Regionaltypen, Kreisen, Zeiträumen und Regionen."),
      p("Die dargestellten Zusammenhänge sind deskriptiv. Sie erlauben keine kausalen Aussagen über demografische oder regionale Einflüsse und liefern keine inferenzstatistischen Signifikanztests.")))})

  output$methods_sources <- renderUI({tagList(
    div(class="panel-card method-grid",h3("Regionaldatenbank Deutschland"),
      p("Der geprüfte Snapshot enthält die demografischen Daten für die Typologie sowie die amtliche durchschnittliche Jahresbevölkerung für die Inzidenzberechnung. Der optionale Live-Modus ruft diese Daten neu aus der Regionaldatenbank Deutschland der Statistischen Ämter des Bundes und der Länder ab."),
      h4("Optionaler Live-Abruf"),
      p("Für den Live-Abruf müssen REGIONALSTATISTIK_USER und REGIONALSTATISTIK_PASSWORD als Umgebungsvariablen hinterlegt sein. Lokal kann dafür beispielsweise .Renviron verwendet werden; auf einer Deployment-Plattform der jeweilige Secret-Mechanismus. Zugangsdaten dürfen nicht im Quellcode oder Repository gespeichert werden."),
      pre(class="credential-example","REGIONALSTATISTIK_USER=...\nREGIONALSTATISTIK_PASSWORD=..."),
      p("Daten der Statistischen Ämter des Bundes und der Länder; ",a("Datenlizenz Deutschland – Namensnennung – Version 2.0",href="https://www.govdata.de/dl-de/by-2-0",target="_blank",rel="noopener noreferrer"),".")),
    div(class="panel-card method-grid",h3("SurvStat@RKI"),
      p("SurvStat@RKI liefert die wöchentlichen gemeldeten Fallzahlen sowie die für die historische Referenzanalyse verwendeten Kreisinzidenzen. Die App bestimmt den passenden Erreger-Eintrag anhand der SurvStat-Metadaten und verwendet die hinterlegten geografischen Zuordnungen."),p("Daten des Robert Koch-Instituts; es gelten die Nutzungs- und Quellenbedingungen des RKI.")),
    div(class="panel-card method-grid",h3("BKG / geografische Ressourcen"),
      p("Die verwendeten Kreisidentitäten, Kartengeometrien und Ländergrenzen beziehen sich auf den geografischen Referenzstand ",format(cache$map$provenance$source_vintage,"%d.%m.%Y"),"."),
      p(cache$map$provenance$attribution),p(a(cache$map$provenance$license,href=cache$map$provenance$license_url,target="_blank")),p("Die BKG-Ressourcen behalten ihre jeweiligen Quellen- und Lizenzbedingungen. Ausführliche Angaben zu Quellen, Lizenzen und Datenständen stehen in NOTICE und der Paketdokumentation.")))})

  output$methods_status <- renderUI({
    if(is.null(state$result))return(div(class="info-state","Analyse laden, um den aktuellen Datenstand und die verwendeten Definitionen anzuzeigen."))
    b<-state$result$bundle;r<-state$result$analysis_range;incidence_queries<-if("source_incidence"%in%names(b$provenance))b$provenance$source_incidence else b$provenance$incidence;status<-unique(c(regionalepi:::.shiny_app_query_status(incidence_queries),regionalepi:::.shiny_app_query_status(b$provenance$counts)));derived<-state$result$typology_mode!="dissertation";status_by_year<-b$diagnostics$status_by_reporting_year
    tagList(div(class="panel-card method-grid",h3("Aktuelle Analyse"),
      p(strong("Erreger: "),input$pathogen),p(strong("Beobachtungszeitraum: "),state$result$window$label),
      p(strong("Analysezeitraum: "),format(r$start_date)," bis ",format(r$end_date)),
      if(isTRUE(r$is_truncated))p(strong("Analyse-Stichtag: "),format(r$analysis_as_of_date)," (unabhängig vom SurvStat-Datenstatus)"),
      p(strong("Demografischer Referenzzeitraum: "),paste(range(state$result$demographic_years),collapse="–")),
      p(strong("Typologie: "),if(state$result$typology_mode=="dissertation")"Historische Referenztypologie (2017–2020)"else"Aktualisierte demografische Typologie"),
      if(state$result$typology_mode=="dynamic")p(strong("Clusterzahl: "),length(unique(state$result$fit$assignments$display_cluster_id))),
      p(strong("Inzidenzdefinition: "),if(derived)"Inzidenz auf Basis der durchschnittlichen Jahresbevölkerung"else"Historische SurvStat-Inzidenz"),
      if(derived)p(strong("Definition: "),"annual_average_population_v1; Multiplikator 100000"),
      if(derived)p(strong("Quelle des Bevölkerungsnenners: "),if(identical(b$diagnostics$population_source_mode,"snapshot"))"Geprüfter Snapshot"else"Live-Abruf Regionaldatenbank"),
      if(derived)p(strong("Status und Bezugsjahre: "),paste(paste0(status_by_year$reporting_year," = ",ifelse(status_by_year$incidence_status=="provisional","vorläufig",status_by_year$incidence_status)," (Bevölkerung ",status_by_year$population_year,")"),collapse=", ")),
      if(derived)p(strong("Bevölkerungs-Datenstatus: "),paste(vapply(b$provenance$population,function(x)as.character(x$data_status),character(1L)),collapse=", ")),
      p(strong("Geografischer Referenzstand: "),format(cache$map$provenance$source_vintage)),
      p(strong("SurvStat-Datenstatus: "),paste(status,collapse=", "))))})

  output$methods_reproducibility <- renderUI({
    if(is.null(state$result))return(div(class="info-state","Analyse laden, um die technischen Reproduzierbarkeitsangaben anzuzeigen."))
    provenance<-state$result$fit$provenance;source_versions<-if("source_version"%in%names(state$result$bundle$data))unique(as.character(state$result$bundle$data$source_version))else character();source_versions<-source_versions[!is.na(source_versions)];if(!length(source_versions))source_versions<-"nicht verfügbar"
    tagList(div(class="panel-card method-grid",h3("Technische Provenienz"),
      p(strong("Paketversion: "),as.character(utils::packageVersion("regionalepi"))),
      p(strong("Typologie-Indikatorsatz: "),provenance$indicator_set_id," (",provenance$indicator_set_version,")"),
      p(strong("Fitting-Spezifikation: "),provenance$fitting_specification_id," (",provenance$fitting_specification_version,")"),
      p(strong("Fit-ID: "),provenance$fit_id),
      p(strong("Beobachtungsfenster-Definition: "),state$result$window$definition_version),
      p(strong("Kartenressource: "),cache$map$provenance$resource_version),
      p(strong("SurvStat-Quellversion: "),paste(source_versions,collapse=", ")),
      p(strong("Primäre Inzidenz: "),if(state$result$typology_mode=="dissertation")"historische SurvStat-Inzidenz"else"incidence_annual_average (annual_average_population_v1)"),
      if(state$result$typology_mode!="dissertation")p(strong("Nenner: "),"Regionaldatenbank Deutschland, Tabelle 12411-05-01-4; Berichts-/Bevölkerungsjahr und finaler/vorläufiger Status siehe Datenstand und Definitionen."),
      p(strong("Sitzungscache: "),length(ls(cache,all.names=TRUE))," Einträge; ",state$retrieval_count," Surveillance-Abruf(e) in dieser Sitzung."),
      p(strong("Methodische Referenz: "),a("Dettmann (2026)",href="https://doi.org/10.17169/refubium-51449",target="_blank",rel="noopener noreferrer"))))})
}
