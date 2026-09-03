server <- function(input, output, session) {
  cache <- new.env(parent=emptyenv()); cache$map <- regionalepi::regionalepi_map_geometry()
  cache$states <- regionalepi::regionalepi_state_boundaries(); cache$map_bounds <- regionalepi:::.shiny_app_map_bounds(cache$map)
  cache$surveillance_index <- list()
  resources <- get("regionalepi_geography_resources_2024", envir=asNamespace("regionalepi"), inherits=TRUE)
  state <- reactiveValues(result=NULL,error=NULL,technical_error=NULL,busy=FALSE,
    selected_geo_id=NULL,selected_date=NULL,retrieval_count=0L)
  session$userData$plotlyShinyEventIDs <- paste("plotly_click",
    c("demography","period","district-time","district-heat"),sep="-")

  selection <- reactiveVal(regionalepi:::.shiny_reconcile_selection(
    "Influenza, saisonal", "influenza_2025_26", "window"))
  observeEvent(input$pathogen, {
    current <- isolate(input$window_id); mode <- isolate(input$range_mode)
    next_selection <- regionalepi:::.shiny_reconcile_selection(
      input$pathogen, current, mode)
    selection(next_selection)
    freezeReactiveValue(input,"window_id")
    updateSelectInput(session,"window_id",choices=next_selection$choices,
      selected=next_selection$window_id)
    if(!identical(next_selection$range_mode,mode)) {
      updateRadioButtons(session,"range_mode",selected=next_selection$range_mode)
    }
  }, ignoreInit=TRUE)
  observeEvent(input$window_id, {
    req(input$pathogen,input$window_id)
    choices <- regionalepi:::.shiny_window_choices(input$pathogen)
    if(input$window_id %in% unname(choices)) selection(
      regionalepi:::.shiny_reconcile_selection(input$pathogen,input$window_id,
        isolate(input$range_mode)))
  },ignoreInit=TRUE)
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
    choices <- if(nrow(periods)) stats::setNames(periods$period_id,periods$label) else character()
    modes <- c("Gesamter Beobachtungszeitraum"="window","Benutzerdefinierter Zeitraum"="custom")
    if(nrow(periods)) modes <- c("Gesamter Beobachtungszeitraum"="window","RKI-definierter/geprüfter Zeitraum"="reviewed","Benutzerdefinierter Zeitraum"="custom")
    selected_mode <- if(input$range_mode %in% unname(modes)) input$range_mode else "window"
    updateRadioButtons(session,"range_mode",choices=modes,selected=selected_mode)
    updateSelectInput(session,"reviewed_period_id",choices=choices,selected=if(length(choices)) unname(choices[[1L]]) else character())
    updateDateRangeInput(session,"custom_range",start=window$start_date,end=window$end_date,min=window$start_date,max=window$end_date)
  })
  output$reviewed_period_note <- renderUI({
    if(nrow(reviewed_periods())) return(NULL)
    p(class="app-note","Für diesen Beobachtungszeitraum liegt keine abgeschlossene geprüfte RKI-Welle vor.")
  })
  reviewed_period <- reactive({
    x <- reviewed_periods(); if(!nrow(x)) return(NULL)
    x[x$period_id==input$reviewed_period_id,,drop=FALSE]
  })
  analysis_range <- reactive({
    custom <- if(!is.null(input$custom_range)) as.Date(input$custom_range) else NULL
    mode <- input$range_mode
    if(is.null(mode)||!mode%in%c("window","reviewed","custom")) mode<-selection()$range_mode
    if(identical(mode,"reviewed")&&!nrow(reviewed_periods())) mode<-"window"
    regionalepi:::.shiny_select_analysis_range(selected_window(),mode,reviewed_period(),custom)
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
      window <- selected_window(); years <- seq.int(window$start_iso_year,window$end_iso_year)
      mode <- input$typology_mode; period_label <- if(mode=="dissertation") "2017–2020" else input$demographic_period
      demographic_years <- regionalepi:::.shiny_demographic_periods()[[period_label]]
      dkey <- regionalepi:::.shiny_demographic_cache_key(period_label,input$demographic_source)
      if(!exists(dkey,envir=cache,inherits=FALSE)) assign(dkey,if(input$demographic_source=="snapshot") regionalepi:::.shiny_fetch_snapshot_demography(demographic_years) else regionalepi:::.shiny_fetch_demography(demographic_years),envir=cache)
      demographic <- get(dkey,envir=cache,inherits=FALSE); setProgress(.2,detail="Typologie wird angepasst …")
      tkey <- regionalepi:::.shiny_typology_cache_key(demographic$summary,if(mode=="dissertation")3L else as.integer(input$k),mode)
      if(!exists(tkey,envir=cache,inherits=FALSE)) assign(tkey,regionalepi:::.shiny_fit_typology(demographic$summary,mode,if(mode=="dissertation")3L else as.integer(input$k)),envir=cache)
      fit <- get(tkey,envir=cache,inherits=FALSE)
      palette_variant <- if(mode=="dynamic"&&as.integer(input$k)>=2L) "profile_aligned" else "neutral"
      palette_alignment <- NULL
      if(mode=="dynamic"&&as.integer(input$k)>=2L) {
        reference_key <- "typology:dissertation_v1:historical_reference"
        if(!exists(reference_key,envir=cache,inherits=FALSE)) assign(reference_key,
          regionalepi:::.shiny_fit_typology(
            regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary,
            "dissertation",3L),envir=cache)
        anchor_key <- regionalepi:::.shiny_typology_cache_key(
          demographic$summary,3L,"dynamic")
        if(!exists(anchor_key,envir=cache,inherits=FALSE)) assign(anchor_key,
          regionalepi:::.shiny_fit_typology(demographic$summary,"dynamic",3L),envir=cache)
        palette_alignment <- regionalepi:::.shiny_dynamic_colour_policy(
          fit,get(reference_key,envir=cache,inherits=FALSE),
          get(anchor_key,envir=cache,inherits=FALSE))
      }
      skey <- regionalepi:::.shiny_surveillance_cache_match(cache$surveillance_index,input$pathogen,years)
      if(is.null(skey)) {
        skey <- regionalepi:::.shiny_surveillance_bundle_cache_key(input$pathogen,years)
        setProgress(.35,detail="SurvStat-Inzidenzen und gemeldete Fälle werden geladen …")
        assign(skey,regionalepi:::.shiny_fetch_surveillance_bundle(input$pathogen,years,resources),envir=cache)
        cache$surveillance_index[[skey]] <- list(key=skey,pathogen=input$pathogen,reporting_years=years)
        state$retrieval_count <- state$retrieval_count+1L
      }
      bundle <- get(skey,envir=cache,inherits=FALSE); map_join <- regionalepi:::.shiny_map_assignments(cache$map,fit)
      display_metadata <- regionalepi:::.shiny_cluster_display_metadata(
        fit,mode,palette_variant,palette_alignment,map_join$data$geo_id)
      mkey <- paste0("map:",fit$provenance$fit_id,":",mode,":",palette_variant)
      if(!exists(mkey,envir=cache,inherits=FALSE)) assign(mkey,regionalepi:::.shiny_app_leaflet_geojson(cache$map$browser_geojson,
        cache$map_bounds,map_join$data,mode,cache$states$browser_geojson,
        palette_variant,palette_alignment,display_metadata),envir=cache)
      state$result <- list(window=window,demographic=demographic,fit=fit,bundle=bundle,map_join=map_join,
        map_widget=get(mkey,envir=cache,inherits=FALSE),typology_mode=mode,typology_config=regionalepi:::.shiny_typology_config(mode),
        demographic_years=demographic_years,palette_variant=palette_variant,
        palette_alignment=palette_alignment,display_metadata=display_metadata,
        cache_keys=list(demographic=dkey,typology=tkey,surveillance=skey,map=mkey))
      if(is.null(state$selected_geo_id)||!state$selected_geo_id%in%map_join$data$geo_id) state$selected_geo_id<-"11000"
      state$selected_date <- analysis_range()$start_date; setProgress(1,detail="Darstellung ist bereit.")
    }),error=function(e){state$technical_error<-conditionMessage(e);state$error<-regionalepi:::.shiny_app_format_error(e,"Die Live-Analyse")})
  })

  exploration <- reactive({ req(state$result); regionalepi:::.shiny_exploration_summaries(state$result$bundle,state$result$fit,analysis_range(),state$result$display_metadata) })
  palette_colours <- function(ids) {metadata<-state$result$display_metadata;
    colours<-stats::setNames(metadata$display_colour,metadata$display_cluster_id);
    colours[metadata$display_cluster_id[metadata$display_cluster_id%in%as.character(ids)]]}
  display_data <- function(data,cluster_col="cluster_id")
    regionalepi:::.shiny_apply_display_metadata(data,state$result$display_metadata,cluster_col)
  output$load_status <- renderUI({if(state$busy)p("Daten werden geladen …") else if(!is.null(state$error))p(class="status-error",state$error) else if(is.null(state$result))p("Noch keine Analyse geladen.") else p(class="status-ok","Analyse geladen: ",state$result$fit$provenance$fit_id)})
  output$range_status <- renderUI({r<-analysis_range();p(class="app-note",r$label,": ",format(r$start_date,"%d.%m.%Y"),"–",format(r$end_date,"%d.%m.%Y")," (",r$start_iso," bis ",r$end_iso,")")})
  output$demography_status <- renderUI({req(state$result);p(class="app-note","Demographie: ",regionalepi:::.shiny_demography_status(state$result$demographic))})
  output$district_map <- leaflet::renderLeaflet({req(state$result);state$result$map_widget})
  observe({req(state$result,state$selected_geo_id);session$sendCustomMessage("regionalepi-select",state$selected_geo_id)})
  output$map_attribution <- renderUI({p(class="app-note",cache$map$provenance$attribution," · Ländergrenzen: ",cache$states$provenance$source_layer," · ",a(cache$map$provenance$license,href=cache$map$provenance$license_url,target="_blank"))})

  output$analysis_heading <- renderUI({req(state$result);w<-state$result$window;if(state$result$typology_mode=="dissertation")context<-"Dissertation-Referenztypologie · 2017–2020 · ClD / ClJ / ClA" else context<-paste0("Referenzzeitraum ",format(min(state$result$demographic$summary$data$period_start),"%Y"),"–",format(max(state$result$demographic$summary$data$period_end),"%Y")," · ",length(unique(state$result$fit$assignments$display_cluster_id))," Cluster · ",nrow(state$result$map_join$data)," Kreise");tagList(h3("Demografische Regionaltypologie"),p(class="app-note",context),p(class="app-note","Epidemiologischer Beobachtungszeitraum: ",w$label))})
  output$period_heading <- renderUI({req(state$result);r<-analysis_range();rp<-reviewed_period();tagList(h3(r$label),p(class="app-note",format(r$start_date,"%d.%m.%Y"),"–",format(r$end_date,"%d.%m.%Y")," · ",r$start_iso," bis ",r$end_iso),if(!is.null(rp)&&nrow(rp))p(class="app-note","Geprüfter Zeitraum im Beobachtungsfenster: ",rp$label," · ",format(rp$start_date,"%d.%m.%Y"),"–",format(rp$end_date,"%d.%m.%Y")," · ",regionalepi:::.iso_week_label(rp$start_date)," bis ",regionalepi:::.iso_week_label(rp$end_date)))})
  output$cluster_summary <- renderTable({req(state$result);x<-state$result$display_metadata;data.frame(Cluster=x$display_cluster_id,`Anzahl Kreise`=x$n_districts,`Demografisches Profil`=x$profile_description,check.names=FALSE)},striped=TRUE,rownames=FALSE)

  output$profile_plot <- plotly::renderPlotly({req(state$result);p<-display_data(state$result$fit$profiles,"display_cluster_id");d<-regionalepi:::.shiny_app_indicator_display();p$label<-d$label[match(p$indicator_id,d$indicator_id)];p$text<-sprintf("Cluster: %s<br>Indikator: %s<br>z: %.2f<br>Mittelwert: %.2f<br>Median: %.2f<br>Kreise: %d",p$display_cluster_id,p$label,p$standardized_center,p$original_mean,p$original_median,p$cluster_size);wide<-stats::xtabs(standardized_center~display_cluster_id+label,p);plotly::layout(plotly::plot_ly(x=colnames(wide),y=rownames(wide),z=wide,type="heatmap",zmid=0,colors=c("#2166AC","#F7F7F7","#B2182B"),text=matrix(p$text[match(paste(rep(rownames(wide),each=ncol(wide)),rep(colnames(wide),times=nrow(wide))),paste(p$display_cluster_id,p$label))],nrow=nrow(wide),byrow=TRUE),hoverinfo="text"),xaxis=list(title=""),yaxis=list(title="Cluster"))})
  output$profile_table <- renderTable({req(state$result);p<-display_data(state$result$fit$profiles,"display_cluster_id");p<-p[order(p$display_order),];d<-regionalepi:::.shiny_app_indicator_display();data.frame(Cluster=p$display_cluster_id,N=p$cluster_size,Indikator=d$label[match(p$indicator_id,d$indicator_id)],Einheit=d$unit[match(p$indicator_id,d$indicator_id)],Mittelwert=round(p$original_mean,2),Median=round(p$original_median,2),check.names=FALSE)},striped=TRUE,rownames=FALSE)
  output$cluster_warning <- renderUI({req(state$result);p(class="app-note",if(state$result$typology_mode=="dissertation")"Etablierte Dissertation-Bezeichnungen; keine dynamische Umbenennung."else"Beschreibungen sind reproduzierbare Profiltexte, keine Clusteridentitäten.")})

  output$demographic_distribution_plot <- plotly::renderPlotly({req(state$result);x<-merge(state$result$demographic$summary$data,state$result$fit$assignments[c("geo_id","display_cluster_id")],by="geo_id");x<-display_data(x,"display_cluster_id");x$geo_name<-state$result$map_join$data$geo_name[match(x$geo_id,state$result$map_join$data$geo_id)];d<-regionalepi:::.shiny_app_indicator_display();x$indicator_label<-factor(d$label[match(x$indicator_id,d$indicator_id)],levels=d$label);x$unit<-d$unit[match(x$indicator_id,d$indicator_id)];x$text<-sprintf("%s<br>AGS: %s<br>Cluster: %s<br>%s: %.2f %s",x$geo_name,x$geo_id,x$display_cluster_id,x$indicator_label,x$indicator_value,x$unit);cols<-palette_colours(x$display_cluster_id);g<-ggplot2::ggplot(x,ggplot2::aes(display_cluster_id,indicator_value,fill=display_cluster_id,colour=display_cluster_id))+ggplot2::geom_violin(alpha=.2,trim=FALSE)+ggplot2::geom_boxplot(width=.15,outlier.shape=NA,alpha=.65)+suppressWarnings(ggplot2::geom_jitter(ggplot2::aes(key=geo_id,text=text),width=.09,alpha=.45,size=1))+ggplot2::facet_wrap(~indicator_label,scales="free_y",ncol=1)+ggplot2::scale_colour_manual(values=cols)+ggplot2::scale_fill_manual(values=cols)+ggplot2::labs(x="Cluster",y=NULL)+ggplot2::theme_minimal()+ggplot2::theme(legend.position="none",panel.spacing.y=grid::unit(1.25,"lines"),strip.text=ggplot2::element_text(face="bold",size=11));sel<-x[x$geo_id==state$selected_geo_id,,drop=FALSE];if(nrow(sel))g<-g+ggplot2::geom_point(data=sel,shape=21,fill="#FFFFFF",colour="#111111",size=3,stroke=1.1);plotly::ggplotly(g,tooltip="text",source="demography")})

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
  output$incidence_warning <- renderUI({x<-exploration()$weekly;bad<-x$expected_districts>0&x$observed_districts/x$expected_districts<.8;p(class=if(any(bad))"status-error"else"app-note",if(any(bad))paste(sum(bad),"Wochen-/Clustergruppen unter 80 % Vollständigkeit.")else"Fehlende Inzidenzen bleiben NA; Median und empirischer IQR verwenden beobachtete Kreise.")})
  output$period_distribution_plot <- plotly::renderPlotly({x<-display_data(exploration()$district_period);x$text<-sprintf("%s<br>AGS: %s<br>Cluster: %s<br>Median: %s<br>Gemeldete Fälle in beobachteten Wochen: %s<br>Daten vorhanden: %d von %d Wochen%s",x$geo_name,x$geo_id,x$cluster_id,round(x$median_period_incidence,2),ifelse(is.na(x$cumulative_observed_cases),"fehlend",x$cumulative_observed_cases),x$observed_case_week_count,x$expected_week_count,ifelse(x$missing_case_week_count>0,paste0("<br>Fehlende Wochen: ",x$missing_case_week_count),""));cols<-palette_colours(x$cluster_id);g<-ggplot2::ggplot(x,ggplot2::aes(cluster_id,median_period_incidence,fill=cluster_id,colour=cluster_id))+ggplot2::geom_violin(alpha=.2,trim=FALSE,na.rm=TRUE)+ggplot2::geom_boxplot(width=.15,outlier.shape=NA,alpha=.65,na.rm=TRUE)+suppressWarnings(ggplot2::geom_jitter(ggplot2::aes(key=geo_id,text=text),width=.09,alpha=.5,size=1,na.rm=TRUE))+ggplot2::scale_colour_manual(values=cols)+ggplot2::scale_fill_manual(values=cols)+ggplot2::labs(x="Cluster",y="Median der wöchentlichen Kreisinzidenz")+ggplot2::theme_minimal()+ggplot2::theme(legend.position="none");sel<-x[x$geo_id==state$selected_geo_id,,drop=FALSE];if(nrow(sel))g<-g+ggplot2::geom_point(data=sel,shape=21,fill="#FFFFFF",colour="#111111",size=3,stroke=1.1);plotly::ggplotly(g,tooltip="text",source="period")})
  heat <- function(x,y,z,text,source,zmid=0,colors=c("#2166AC","#F7F7F7","#B2182B"),customdata=NULL){rp<-reviewed_period();shapes<-list();if(!is.null(rp)&&nrow(rp))shapes<-lapply(c(rp$start_date,rp$end_date),function(day)list(type="line",xref="x",yref="paper",x0=format(day),x1=format(day),y0=0,y1=1,line=list(color="#4B5563",width=1,dash="dot")));plotly::layout(plotly::plot_ly(x=x,y=y,z=z,type="heatmap",zmid=zmid,colors=colors,text=text,hoverinfo="text",customdata=customdata,source=source,connectgaps=FALSE),xaxis=list(title="",rangeslider=list(visible=TRUE)),yaxis=list(title=""),shapes=shapes)}
  output$relative_heatmap <- plotly::renderPlotly({w<-display_data(exploration()$weekly);w$hover<-sprintf("Cluster: %s<br>%s<br>Cluster-Median: %.2f<br>Median aller Kreise: %.2f<br>Differenz: %.2f<br>Kreise: %d/%d<br>Gemeldete Fälle in beobachteten Kreisen: %s",w$cluster_id,regionalepi:::.iso_week_label(w$date),w$median_incidence,w$all_district_median,w$relative_activity,w$observed_districts,w$expected_districts,ifelse(is.na(w$reported_cases_observed),"fehlend",w$reported_cases_observed));g<-regionalepi:::.shiny_heatmap_grid(w,"cluster_id","date","relative_activity","hover");heat(g$columns,g$rows,g$z,g$text,"relative")})
  output$pairwise_heatmap <- plotly::renderPlotly({p<-exploration()$pairwise;ids<-state$result$display_metadata$display_cluster_id;pair_order<-apply(utils::combn(ids,2L),2L,paste,collapse="\r");p$pair_key<-factor(p$pair_key,levels=pair_order,ordered=TRUE);p<-p[order(p$pair_key,p$date),];p$hover<-sprintf("%s<br>%s<br>Median A: %.2f<br>Median B: %.2f<br>Differenz: %.2f",p$pair_label,regionalepi:::.iso_week_label(p$date),p$median_a,p$median_b,p$difference);g<-regionalepi:::.shiny_heatmap_grid(p,"pair_key","date","difference","hover");labels<-unique(p[c("pair_key","pair_label")]);heat(g$columns,labels$pair_label[match(g$rows,as.character(labels$pair_key))],g$z,g$text,"pairwise")})
  output$district_heatmap <- plotly::renderPlotly({
    audit<-regionalepi:::.shiny_district_heatmap_data(
      exploration()$data,state$result$display_metadata,state$selected_geo_id)
    regionalepi:::.shiny_district_heatmap_widget(audit)
  })

  output$district_detail <- renderUI({req(state$result,state$selected_geo_id);id<-state$selected_geo_id;a<-state$result$map_join$data[state$result$map_join$data$geo_id==id,,drop=FALSE];p<-exploration()$district_period;v<-p[p$geo_id==id,,drop=FALSE];weekly<-exploration()$data;point<-weekly[weekly$geo_id==id&weekly$date==state$selected_date,,drop=FALSE];tagList(h4(a$geo_name),p(a$state_name),p(strong("AGS: "),id),p(strong("Cluster: "),a$display_cluster_id),if(nrow(point))tagList(p(strong(regionalepi:::.iso_week_label(point$date))),p("Inzidenz: ",regionalepi:::.shiny_format_number(point$incidence,1L)," je 100.000"),p("Gemeldete Fälle: ",regionalepi:::.shiny_format_number(point$cases))),if(nrow(v))tagList(p(strong("Median der wöchentlichen Inzidenz: "),regionalepi:::.shiny_format_number(v$median_period_incidence,2L)),p(strong(if(v$missing_case_week_count>0)"Gemeldete Fälle in beobachteten Wochen: "else"Gemeldete Fälle im Analysezeitraum: "),regionalepi:::.shiny_format_number(v$cumulative_observed_cases)),p("Daten vorhanden: ",v$observed_case_week_count," von ",v$expected_week_count," Wochen"),if(v$missing_case_week_count>0)p("Fehlende Wochen: ",v$missing_case_week_count)))})
  output$provenance <- renderUI({req(state$result);b<-state$result$bundle;tagList(h4("Beobachtungsfenster"),p(state$result$window$label,"; ",state$result$window$definition_version,"; ",state$result$window$note),h4("Surveillance"),p("Source-provided Inzidenz und additive gemeldete Fälle; exakte geo_id+date-Kompatibilität; keine Inzidenzberechnung. Datenstatus ",paste(unique(c(regionalepi:::.shiny_app_query_status(b$provenance$incidence),regionalepi:::.shiny_app_query_status(b$provenance$counts))),collapse=", "),"."),h4("Karte"),p(cache$map$provenance$attribution,"; Ländergrenzen aus ",cache$states$provenance$source_layer,"; Analyse ausschließlich auf Kreisebene."),h4("Interaktion"),p("Auswahl, Zoom und Analysebereich arbeiten auf dem Sitzungscache; Abrufanzahl: ",state$retrieval_count,"."),p(class="app-note","Keine gepoolte oder aus Fallzahlen berechnete Inzidenz, keine inferenziellen Tests."))})
}
