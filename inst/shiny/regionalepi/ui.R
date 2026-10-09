initial_window_choices <- regionalepi:::.shiny_window_choices("Influenza, saisonal")
pathogen_choices <- regionalepi:::.shiny_pathogen_choices()

demographic_controls <- tagList(
  selectInput("demographic_period", "Demografischer Referenzzeitraum",
    c("2017–2020", "2022–2025", "Benutzerdefinierter Referenzzeitraum"), "2022–2025"),
  conditionalPanel("input.demographic_period == 'Benutzerdefinierter Referenzzeitraum'",
    selectInput("demographic_start_year", "Startjahr", 2017:2025, 2022),
    selectInput("demographic_end_year", "Endjahr", 2022:2025, 2025)),
  uiOutput("demographic_reference_note"),
  selectInput("k", "Anzahl Cluster", 2:5, 3),
  tags$details(tags$summary("Erweiterte Einstellungen"),
    uiOutput("demographic_source_control")),
  actionButton("prepare_demography", "Typologie vorbereiten / aktualisieren",
    class = "btn-primary"),
  uiOutput("demographic_prepare_status"),
  tags$details(tags$summary("Wissenschaftlicher Export"),
    downloadButton("typology_xlsx", "Typologie als Excel"),
    downloadButton("typology_csv", "Kreiszuordnungen als CSV"),
    downloadButton("typology_png", "Clusterprofile als PNG"),
    p(class = "app-note", "Excel ist das empfohlene Format mit vollst\u00e4ndiger wissenschaftlicher Provenienz.")),
  p(class = "app-note", "Demografische Typologie und Surveillance-Analyse werden getrennt vorbereitet. Ein Live-Abruf startet nur durch diese Schaltfläche oder durch das ausdrückliche Laden einer Analyse.")
)

ui <- fluidPage(
  tags$head(
    tags$title("regionalepi: Regionale Infektionssurveillance und demografische Typologien"),
    tags$style(HTML("
      :root{--re-primary:#5F627B;--re-primary-hover:#4E5066;--re-primary-soft:#ECECF2;--re-action:#DD7F02;--re-action-hover:#C46F00;--re-border:#D9DAE2;--re-bg:#FFF;--re-bg-muted:#F6F6F8;--re-text:#27313A;--re-text-muted:#58616B;--re-success:#556B13;--re-success-soft:#F4F7E6;--re-warning:#8A4A06;--re-warning-soft:#FFF4E5;--re-error:#8B1E1E;--re-error-soft:#FFF2F2}
      body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Arial,sans-serif;font-size:15px;line-height:1.48;color:var(--re-text)}
      .app-note{color:var(--re-text-muted);font-size:12.5px}.panel-card{border:1px solid var(--re-border);border-radius:6px;padding:16px 18px;margin:16px 0 22px;background:var(--re-bg);box-shadow:0 1px 2px rgba(34,36,38,.04)}
      .panel-card h3{font-size:22px}.panel-card h4{font-size:18px}.distribution-panel{border-top:1px solid #E8E8E8;padding-top:12px;margin-top:14px}.small-table table{font-size:12.5px}
      .nav-tabs{border-bottom:1px solid var(--re-border)}.nav-tabs>li>a{color:var(--re-text-muted)}.app-main>.tabbable>.nav-tabs>li.active>a{color:var(--re-primary-hover);border-top:3px solid var(--re-primary)}
      .nested-tabs>.tabbable>.nav-tabs{margin:8px 0 16px}.nested-tabs>.tabbable>.nav-tabs>li.active>a{color:var(--re-primary-hover);background:var(--re-primary-soft);box-shadow:inset 0 -2px 0 var(--re-primary)}
      .btn-primary{background:var(--re-action);border-color:var(--re-action);color:#181D22;font-weight:700}.btn-primary:hover,.btn-primary:focus{background:var(--re-action-hover);border-color:var(--re-action-hover);color:#181D22}
      .status-error{color:var(--re-error);background:var(--re-error-soft);border-left:3px solid var(--re-error);padding:7px 10px;font-weight:600}.status-ok{color:var(--re-success);background:var(--re-success-soft);border-left:3px solid var(--re-success);padding:7px 10px}.status-info{color:var(--re-text-muted);background:var(--re-primary-soft);border-left:3px solid var(--re-primary);padding:7px 10px}.status-quality{color:var(--re-warning);background:var(--re-warning-soft);border-left:3px solid var(--re-warning);padding:7px 10px}.info-state{background:var(--re-primary-soft);border-left:3px solid var(--re-primary);padding:12px 14px;margin:12px 0}.leaflet{min-height:570px}
      @media(max-width:767px){.btn-primary{width:100%}.nav-tabs{display:flex;flex-wrap:nowrap;overflow-x:auto}.nav-tabs>li{float:none;flex:0 0 auto}.nav-tabs>li>a{white-space:nowrap}.leaflet{min-height:400px}}
    "))
  ),
  titlePanel("regionalepi: Regionale Infektionssurveillance und demografische Typologien"),
  div(class = "app-main", tabsetPanel(id="analysis_section",
    tabPanel("Demografische Typologie", value = "typology",
      sidebarLayout(
        sidebarPanel(demographic_controls),
        mainPanel(div(class = "nested-tabs", tabsetPanel(id = "typology_view",
          tabPanel("Karte und Kreise", value = "map_districts",
            div(class = "panel-card", uiOutput("analysis_heading"), leaflet::leafletOutput("district_map", height = "590px"), uiOutput("map_attribution")),
            fluidRow(column(5, div(class = "panel-card", h3("Ausgewählter Kreis"), uiOutput("demographic_district_detail"))),
              column(7, div(class = "panel-card", h3("Clusterübersicht"), tableOutput("cluster_summary"))))),
          tabPanel("Clusterprofile", value = "profiles",
            div(class = "panel-card", h3("Wie unterscheiden sich die Cluster demografisch?"), h4("Standardisierte demografische Clusterprofile"), p(class = "app-note", "z < 0: unter dem Durchschnitt aller Kreise · z > 0: über dem Durchschnitt"), plotly::plotlyOutput("profile_plot", height = "350px"), uiOutput("cluster_warning")),
            div(class = "panel-card", h4("Profile auf Originalskala"), div(class = "small-table", tableOutput("profile_table"))),
            div(class = "panel-card", h4("Verteilungen der Kreise"), radioButtons("density_scale", "Skalierung Bevölkerungsdichte", c("Original" = "original", "Logarithmisch" = "log10"), "original", inline = TRUE), p(class = "app-note", "Die Skalierung betrifft ausschließlich die Bevölkerungsdichte. Durchschnittsalter und Jugendquotient werden stets auf der Originalskala dargestellt."), div(class = "distribution-panel", plotly::plotlyOutput("demographic_distribution_plot", height = "650px")))),
          tabPanel("Stabilität", value = "stability",
            div(class = "panel-card", h3("Wie verändert sich die Drei-Cluster-Referenz bei anderer Clusterzahl?"), p(class = "app-note", strong("k=3 ist die geprüfte interpretative Referenz.")), plotly::plotlyOutput("stability_plot", height = "650px"), div(class = "small-table", tableOutput("stability_summary")), uiOutput("stability_note"))),
          tabPanel("Veränderungen 2017–2025", value = "transitions",
            div(class="panel-card",h3("Veränderungen der demografischen Typologie"),
              uiOutput("transition_summary"),
              downloadButton("transition_xlsx","Übergänge als Excel"),
              downloadButton("transition_csv","Kreisübergänge als CSV"),
              downloadButton("transition_png","Übergangsmatrix als PNG"),
              p(class="app-note","Ein Wechsel der Clusterzuordnung bedeutet nicht automatisch, dass sich die demografische Struktur eines Kreises grundlegend verändert hat. Die Zuordnung hängt auch von der Verteilung aller Kreise und den neu berechneten Clusterzentren ab.")),
            fluidRow(
              column(6,div(class="panel-card",h4("Übergangsmatrix"),
                radioButtons("transition_matrix_mode","Darstellung",
                  c("Absolute Anzahl"="count","Zeilenprozente"="row_percentage"),
                  "count",inline=TRUE),tableOutput("transition_matrix"))),
              column(6,div(class="panel-card",h4("Übergänge 2017–2020 → 2022–2025"),
                plotly::plotlyOutput("transition_sankey",height="360px")))),
            div(class="panel-card",h4("Geografische Verteilung"),
              selectInput("transition_filter","Übergangskategorie",character()),
              leaflet::leafletOutput("transition_map",height="560px"),
              uiOutput("transition_map_attribution")),
            div(class="panel-card",h4("Ausgewählter Kreis"),
              uiOutput("transition_district_detail")))
        )))
      )),
    tabPanel("Infektionsgeschehen", value = "epidemiology",
      sidebarLayout(
        sidebarPanel(
          selectInput("pathogen", "Erreger", pathogen_choices),
          selectInput("window_id", "Beobachtungszeitraum", initial_window_choices, selected = tail(unname(initial_window_choices), 1L)),
          radioButtons("range_mode", "Analysezeitraum", c("Gesamter Beobachtungszeitraum" = "window", "Vordefinierter epidemiologischer Zeitraum" = "reviewed", "Benutzerdefinierter Zeitraum" = "custom"), "window"),
          uiOutput("range_mode_note"), uiOutput("period_availability_note"),
          conditionalPanel("input.range_mode == 'reviewed'", selectInput("reviewed_period_id", "Vordefinierter epidemiologischer Zeitraum", character()), uiOutput("reviewed_period_note")),
          conditionalPanel("input.range_mode == 'custom'", selectInput("custom_start_week", "Start-KW", character()), selectInput("custom_end_week", "End-KW", character())),
          div(class = "info-state", h4("Demografische Grundlage"),
            uiOutput("active_demographic_status"),
            actionLink("show_typology", "Typologie ändern")),
          actionButton("load_analysis", "Analyse laden / aktualisieren", class = "btn-primary"), br(), br(),
          tags$details(tags$summary("Wissenschaftlicher Export"),
            downloadButton("analysis_xlsx","Analyse als Excel"),
            downloadButton("analysis_weekly_csv","Wochenwerte als CSV"),
            downloadButton("analysis_district_csv","Kreisergebnisse als CSV"),
            downloadButton("analysis_weekly_png","Zeitverlauf als PNG"),
            downloadButton("analysis_district_png","Kreisverteilung als PNG"),
            downloadButton("analysis_week_distribution_png","Wochenverteilung als PNG"),
            p(class="app-note","Excel ist das empfohlene Format mit vollständiger wissenschaftlicher Provenienz.")),
          uiOutput("load_status"), uiOutput("analysis_wide_status"), uiOutput("range_status"), uiOutput("demography_status")
        ),
        mainPanel(div(class = "panel-card", uiOutput("period_heading"),
          uiOutput("comparability_note")), tabsetPanel(id = "epidemiology_view",
          tabPanel("Zeitverlauf", plotly::plotlyOutput("incidence_plot", height = "440px"), uiOutput("incidence_early_window_note"), uiOutput("incidence_warning"), p(class="app-note","Ansicht eingrenzen: Im unteren Zeitbalken kann der sichtbare Ausschnitt verschoben oder vergrößert werden. Dies verändert nur die Darstellung, nicht den gewählten Analysezeitraum.")),
          tabPanel("Verteilung der Kreise",
            radioButtons("district_distribution_scope", "Bezugszeitraum",
              c("Gesamter Analysezeitraum" = "period",
                "Ausgewählte ISO-Kalenderwoche" = "week"),
              "period", inline = TRUE),
            conditionalPanel("input.district_distribution_scope == 'period'",
              p(class = "app-note", "Dargestellt ist je Kreis der Median der beobachteten Wocheninzidenzen im geladenen Analysezeitraum."),
              plotly::plotlyOutput("period_distribution_plot", height = "440px")),
            conditionalPanel("input.district_distribution_scope == 'week'",
              uiOutput("distribution_week_control"),
              uiOutput("weekly_distribution_status"),
              plotly::plotlyOutput("weekly_distribution_plot", height = "440px"),
              p(class = "app-note", "Dargestellt sind die ungewichteten Kreisinzidenzen der ausgewählten ISO-Kalenderwoche. Beobachtete Nullwerte bleiben Null; fehlende Werte werden nicht als Null dargestellt."))),
          tabPanel("Wöchentliche Clusterunterschiede",
            tabsetPanel(id="weekly_comparison_view",
              tabPanel("Relative Aktivität",plotly::plotlyOutput("relative_heatmap", height = "330px")),
              tabPanel("Paarweise Clusterunterschiede",p(class="app-note","A − B bezeichnet den wöchentlichen Median der Kreisinzidenzen von Regionaltyp A minus den entsprechenden Median von Regionaltyp B."),plotly::plotlyOutput("pairwise_heatmap", height = "420px")))),
          tabPanel("Kreis × Woche",p(class="app-note","Zeigt die wöchentliche Inzidenz jedes Kreises im Analysezeitraum. Fehlende Werte sind grau und werden nicht als Null interpretiert."),plotly::plotlyOutput("district_heatmap", height = "760px"))
        ))
      )),
    tabPanel("Regionale Analyse", value = "regional",
      div(class = "panel-card", fluidRow(
        column(4, radioButtons("regional_level", "Räumliche Vergleichsebene", c("Großregionen" = "grossregion", "Regionengruppen aus Ländern" = "aggregated_state", "Bundesländer" = "state"), "grossregion")),
        column(4, uiOutput("regional_focal_control")), column(4, uiOutput("regional_comparison_control"))),
        p(class = "app-note", "Die Regionale Analyse verwendet die bereits geladene reguläre Analyse und löst keinen eigenen Datenabruf aus."), uiOutput("regional_analysis_context")),
      tabsetPanel(id = "regional_view",
        tabPanel("Zeitverlauf", div(class = "panel-card", uiOutput("regional_cluster_heading"), plotly::plotlyOutput("regional_cluster_time_plot", height = "440px"), uiOutput("regional_cluster_quality")), div(class = "panel-card", plotly::plotlyOutput("regional_relative_activity_plot", height = "390px")), div(class = "panel-card", plotly::plotlyOutput("regional_context_time_plot", height = "390px"), uiOutput("regional_context_quality"))),
        tabPanel("Demografische Zusammensetzung", div(class = "panel-card", uiOutput("regional_demographic_heading"), plotly::plotlyOutput("regional_composition_plot", height = "260px")), div(class = "panel-card", plotly::plotlyOutput("regional_demographic_distribution_plot", height = "650px"))),
        tabPanel("Kreise", div(class = "panel-card", uiOutput("regional_district_heading"), plotly::plotlyOutput("regional_district_plot", height = "520px"),p(class="app-note","Unterschiede zwischen den Regionaltypen sind nicht als kausale Effekte demografischer oder administrativer Merkmale zu interpretieren.")))
      ), div(class="panel-card",h4("Export der gewählten Region"),
        downloadButton("regional_csv","Regionale Ergebnisse als CSV"),
        downloadButton("regional_png","Regionalen Zeitverlauf als PNG"))),
    tabPanel("Methodik und Daten", value = "methods",
      uiOutput("methods_page"))
  ))
)
