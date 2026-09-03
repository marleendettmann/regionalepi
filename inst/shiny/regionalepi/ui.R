initial_window_choices <- regionalepi:::.shiny_window_choices("Influenza, saisonal")
ui <- fluidPage(
  tags$head(tags$title("regionalepi – Regionale Infektionsepidemiologie"), tags$style(HTML("
    body{font-size:15px;background:#f7f8fa}.app-note{color:#4b5563;font-size:.92rem}
    .status-error{color:#9b1c1c;font-weight:600}.status-ok{color:#166534;font-weight:600}
    .panel-card{border:1px solid #d7dde5;border-radius:7px;padding:14px;margin:12px 0;background:#fff}
    .distribution-panel{border-top:1px solid #e5e7eb;padding-top:12px;margin-top:14px}
    .leaflet{min-height:570px}.leaflet-container{background:#f3f5f7}.small-table table{font-size:12px}
    @media(max-width:900px){.leaflet{min-height:430px}}
  "))),
  titlePanel("regionalepi – Regionale Infektionsepidemiologie im demographischen Kontext"),
  sidebarLayout(sidebarPanel(
    selectInput("pathogen", "Erreger", c("Influenza"="Influenza, saisonal", "COVID-19"="COVID-19")),
    selectInput("window_id", "Beobachtungszeitraum", initial_window_choices,
      selected=tail(unname(initial_window_choices),1L)),
    radioButtons("range_mode", "Analysezeitraum", c("Gesamter Beobachtungszeitraum"="window",
      "RKI-definierter/geprüfter Zeitraum"="reviewed", "Benutzerdefinierter Zeitraum"="custom"), "window"),
    conditionalPanel("input.range_mode == 'reviewed'", selectInput("reviewed_period_id", "Geprüfter analytischer Zeitraum", character()), uiOutput("reviewed_period_note")),
    conditionalPanel("input.range_mode == 'custom'", dateRangeInput("custom_range", "Benutzerdefinierter Analysezeitraum")),
    selectInput("typology_mode", "Typologie", c("Dynamische demographische Typologie"="dynamic", "Dissertation-Referenz"="dissertation"), "dynamic"),
    conditionalPanel("input.typology_mode == 'dynamic'",
      selectInput("demographic_period", "Demographischer Referenzzeitraum", c("2022–2024", "2017–2020"), "2022–2024"),
      selectInput("k", "Anzahl Cluster (explorativ)", 2:5, 3),
      p(class="app-note", "Dynamische Clusterfarben werden aus der demografischen Profilausrichtung zugewiesen; Cluster-IDs bleiben fit-spezifisch.")),
    conditionalPanel("input.typology_mode == 'dissertation'", p(class="app-note", strong("Historische Referenzreproduktion der Dissertation (2017–2020)"), br(), "k = 3 und Indikatorsatz sind fest vorgegeben.")),
    tags$details(tags$summary("Erweiterte Einstellungen"),
      selectInput("demographic_source", "Demographische Daten", c("Geprüfter Snapshot (empfohlen)"="snapshot", "Live-Abruf Regionaldatenbank"="live"), "snapshot"),
      checkboxInput("district_overlay", "Ausgewählten Kreis im Zeitverlauf anzeigen", TRUE),
      p(class="app-note", "Live-Abrufe gelten nur für diese Sitzung.")),
    actionButton("load_analysis", "Analyse laden / aktualisieren", class="btn-primary"), br(), br(),
    uiOutput("load_status"), uiOutput("range_status"), uiOutput("demography_status"),
    p(class="app-note", "Bereich, Zoom, Tabs und Auswahl lösen keinen neuen Datenabruf aus.")),
    mainPanel(tabsetPanel(id="analysis_section",
      tabPanel("Übersicht", value="overview",
        div(class="panel-card", uiOutput("analysis_heading"), leaflet::leafletOutput("district_map", height="590px"), uiOutput("map_attribution")),
        fluidRow(column(5,div(class="panel-card",h3("Ausgewählter Kreis"),uiOutput("district_detail"))),
          column(7,div(class="panel-card",h3("Clusterübersicht"),tableOutput("cluster_summary"))))),
      tabPanel("Demografische Typologie", value="typology",
        div(class="panel-card",h3("Standardisierte demografische Clusterprofile"),p(class="app-note","z < 0: unter dem Durchschnitt aller Kreise · z > 0: über dem Durchschnitt"),plotly::plotlyOutput("profile_plot",height="350px"),uiOutput("cluster_warning")),
        div(class="panel-card",h3("Profile auf Originalskala"),div(class="small-table",tableOutput("profile_table"))),
        div(class="panel-card",h3("Stabilität und Aufspaltung der Typologie"),p(class="app-note",strong("k=3: Referenz für Sensitivitätsvergleich")),plotly::plotlyOutput("stability_plot",height="390px"),div(class="small-table",tableOutput("stability_summary")),uiOutput("stability_note")),
        div(class="panel-card",h3("Demografische Verteilungen"),p(class="app-note","Die drei Indikatoren werden auf getrennten Skalen dargestellt."),div(class="distribution-panel",plotly::plotlyOutput("demographic_distribution_plot",height="650px")))),
      tabPanel("Infektionsgeschehen", value="epidemiology", div(class="panel-card",uiOutput("period_heading")),
        tabsetPanel(id="epidemiology_view",
          tabPanel("Zeitverlauf",plotly::plotlyOutput("incidence_plot",height="440px"),p(class="app-note","Ansicht eingrenzen: Im unteren Zeitbalken kann der sichtbare Ausschnitt verschoben oder vergrößert werden. Dies verändert nur die Darstellung, nicht den gewählten Analysezeitraum."),uiOutput("incidence_warning")),
          tabPanel("Verteilung der Kreise",plotly::plotlyOutput("period_distribution_plot",height="440px")),
          tabPanel("Wöchentliche Clusterunterschiede",
            tabsetPanel(id="weekly_comparison_view",
              tabPanel("Relative Aktivität",h4("Relative Aktivität gegenüber allen Kreisen"),plotly::plotlyOutput("relative_heatmap",height="330px")),
              tabPanel("Paarweise Vergleiche",p(class="app-note","A − B zeigt die Differenz der wöchentlichen medianen Kreisinzidenz zwischen beiden Clustern. Positive Werte bedeuten höhere Werte in A, negative Werte höhere Werte in B."),plotly::plotlyOutput("pairwise_heatmap",height="420px")))),
          tabPanel("Kreis × Woche",p(class="app-note","Rohinzidenz; fehlende Werte sind grau und nicht null."),plotly::plotlyOutput("district_heatmap",height="760px")))),
      tabPanel("Methodik & Daten", value="methods",div(class="panel-card",uiOutput("provenance")))
    )))
)
