initial_period_choices <- regionalepi:::.shiny_period_choices("Influenza, saisonal")
initial_period_values <- unlist(initial_period_choices, use.names = FALSE)

ui <- fluidPage(
  tags$head(tags$title("regionalepi – Regionale Infektionsepidemiologie"),
    tags$style(HTML("
      body { font-size:15px; background:#f7f8fa; }
      .app-note { color:#4b5563; font-size:0.92rem; }
      .status-error { color:#9b1c1c; font-weight:600; }
      .status-ok { color:#166534; font-weight:600; }
      .panel-card { border:1px solid #d7dde5; border-radius:7px;
                    padding:14px; margin:12px 0; background:#fff; }
      .leaflet { min-height:520px; }
      .leaflet-container { background:#f3f5f7; }
      .small-table table { font-size:12px; }
      @media (max-width:900px) { .leaflet { min-height:420px; } }
    "))),
  titlePanel("regionalepi – Regionale Infektionsepidemiologie im demographischen Kontext"),
  sidebarLayout(
    sidebarPanel(
      selectInput("pathogen", "Erreger", c("Influenza" = "Influenza, saisonal", "COVID-19" = "COVID-19")),
      selectInput("period_id", "Epidemiologischer Zeitraum", choices = initial_period_choices,
        selected = tail(initial_period_values, 1L)),
      selectInput("typology_mode", "Typologie", c(
        "Dynamische demographische Typologie" = "dynamic",
        "Dissertation-Referenz" = "dissertation"), selected = "dynamic"),
      conditionalPanel("input.typology_mode == 'dynamic'",
        selectInput("demographic_period", "Demographischer Referenzzeitraum",
          choices = c("2022–2024", "2017–2020"), selected = "2022–2024"),
        selectInput("k", "Anzahl Cluster (explorativ)", choices = 2:5, selected = 3),
        p(class = "app-note", "C01, C02 usw. sind fit-lokale neutrale IDs.")),
      conditionalPanel("input.typology_mode == 'dissertation'",
        p(class = "app-note", strong("Historische Referenzreproduktion der Dissertation (2017–2020)"),
          br(), "k = 3 und Indikatorsatz sind fest vorgegeben.")),
      tags$details(tags$summary("Erweiterte Einstellungen"),
        selectInput("demographic_source", "Demographische Daten", c(
          "Geprüfter Snapshot (empfohlen)" = "snapshot",
          "Live-Abruf Regionaldatenbank" = "live"), selected = "snapshot"),
        p(class = "app-note", "Live-Abrufe gelten nur für diese Sitzung.")),
      actionButton("load_analysis", "Analyse laden / aktualisieren", class = "btn-primary"),
      br(), br(), uiOutput("load_status"), uiOutput("demography_status"),
      p(class = "app-note", "Navigation und Kartenauswahl lösen keinen Datenabruf aus.")),
    mainPanel(tabsetPanel(id = "analysis_section",
      tabPanel("Übersicht", value = "overview",
        div(class = "panel-card", uiOutput("analysis_heading"),
          leaflet::leafletOutput("district_map", height = "540px"), uiOutput("map_attribution")),
        fluidRow(column(5, div(class = "panel-card", h3("Ausgewählter Kreis"), uiOutput("district_detail"))),
          column(7, div(class = "panel-card", h3("Clusterübersicht"), tableOutput("cluster_summary"))))),
      tabPanel("Typologie", value = "typology",
        div(class = "panel-card", h3("Standardisierte Clusterprofile"),
          plotOutput("profile_plot", height = "330px"), uiOutput("cluster_warning")),
        div(class = "panel-card", h3("Profile auf Originalskala"),
          div(class = "small-table", tableOutput("profile_table"))),
        div(class = "panel-card", h3("Demographische Verteilungen"),
          plotOutput("demographic_distribution_plot", height = "520px"))),
      tabPanel("Infektionsgeschehen", value = "epidemiology",
        div(class = "panel-card", uiOutput("period_heading"),
          plotOutput("incidence_plot", height = "390px"), uiOutput("incidence_warning")),
        div(class = "panel-card", h3("Verteilung der kreisspezifischen Periodenmediane"),
          p(class = "app-note", "Median der wöchentlichen Kreisinzidenz im ausgewählten Zeitraum; ein ungewichteter Wert je Kreis."),
          plotOutput("period_distribution_plot", height = "390px"))),
      tabPanel("Methodik & Daten", value = "methods",
        div(class = "panel-card", uiOutput("provenance")))
    ))
  )
)
