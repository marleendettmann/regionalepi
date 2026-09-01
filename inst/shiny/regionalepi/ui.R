initial_period_choices <- regionalepi:::.shiny_period_choices(
  "Influenza, saisonal"
)

ui <- fluidPage(
  tags$head(
    tags$title("regionalepi – Regionale Infektionsepidemiologie im demographischen Kontext"),
    tags$style(HTML("
      body { font-size: 15px; }
      .app-note { color:#4b5563; font-size:0.92rem; }
      .status-error { color:#9b1c1c; font-weight:600; }
      .status-ok { color:#166534; font-weight:600; }
      .panel-card { border:1px solid #d7dde5; border-radius:6px;
                    padding:14px; margin-bottom:14px; background:#fff; }
      .leaflet { min-height:520px; }
      .leaflet-container { background:#f3f5f7; }
      .small-table table { font-size:12px; }
      @media (max-width: 900px) { .leaflet { min-height:420px; } }
    "))
  ),
  titlePanel("regionalepi – Regionale Infektionsepidemiologie im demographischen Kontext"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "pathogen", "Erreger", c(
          "Influenza" = "Influenza, saisonal", "COVID-19" = "COVID-19"
        )
      ),
      selectInput(
        "period_id", "Epidemiologischer Zeitraum",
        choices = initial_period_choices,
        selected = unname(initial_period_choices[[length(initial_period_choices)]])
      ),
      selectInput(
        "demographic_period", "Demographischer Referenzzeitraum",
        choices = c("2022–2024", "2017–2020"), selected = "2022–2024"
      ),
      selectInput(
        "typology_mode", "Typologie", choices =
          c("Dynamische demographische Typologie" = "dynamic")
      ),
      tags$details(
        tags$summary("Erweiterte Einstellungen"),
        selectInput(
          "k", "Anzahl Cluster (explorativ)", choices = 2:5, selected = 3
        ),
        p(class = "app-note",
          "Eine Änderung von k erzeugt eine neue explorative Partition. ",
          "C01, C02 usw. sind fit-spezifisch und keine dauerhaft festgelegten Sachkategorien.")
      ),
      actionButton("load_analysis", "Analyse laden / aktualisieren",
                   class = "btn-primary"),
      br(), br(),
      uiOutput("load_status"),
      tags$hr(),
      p(class = "app-note",
        "Live-Daten werden erst nach Betätigung des Buttons abgerufen. ",
        "Innerhalb der Sitzung werden bereits geladene Quellen wiederverwendet.")
    ),
    mainPanel(
      div(class = "panel-card",
          h3("Kreiskarte"), leaflet::leafletOutput("district_map", height = "540px"),
          uiOutput("map_attribution")),
      fluidRow(
        column(7, div(class = "panel-card",
          h3("Demographische Clusterprofile"),
          plotOutput("profile_plot", height = "310px"),
          uiOutput("cluster_warning"),
          div(class = "small-table", tableOutput("profile_table")))),
        column(5, div(class = "panel-card",
          h3("Ausgewählter Kreis"), uiOutput("district_detail")))
      ),
      div(class = "panel-card",
          h3("Wöchentliche mediane Kreisinzidenz nach Cluster"),
          plotOutput("incidence_plot", height = "360px"),
          uiOutput("incidence_warning")),
      tags$details(
        class = "panel-card", tags$summary("Methoden und Provenienz"),
        uiOutput("provenance")
      )
    )
  )
)
