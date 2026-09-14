initial_window_choices <- regionalepi:::.shiny_window_choices("Influenza, saisonal")
pathogen_choices <- regionalepi:::.shiny_pathogen_choices()
ui <- fluidPage(
  tags$head(tags$title("regionalepi – Regionale Infektionsepidemiologie"), tags$style(HTML("
    body{font-family:Lato,'Helvetica Neue',Arial,sans-serif;font-size:15px;line-height:1.48;color:rgba(0,0,0,.87);background:#fff}
    a{color:#4183C4}.container-fluid>h2{font-size:26px;line-height:1.25;margin-bottom:16px}.app-note{color:#4f5963;font-size:12.5px;line-height:1.5}
    .status-error{color:#8b1e1e;background:#fff2f2;border-left:3px solid #b42318;padding:7px 10px;font-weight:600}
    .status-ok{color:#315d78;background:#f4f8fb;border-left:3px solid #7da6c2;padding:7px 10px}
    .status-quality{color:#725415;background:#fff8e6;border-left:3px solid #d6a72c;padding:7px 10px}
    .panel-card{border:1px solid rgba(34,36,38,.15);border-radius:5px;padding:16px 18px;margin:16px 0 22px;background:#fff;box-shadow:0 1px 2px rgba(34,36,38,.04)}
    .panel-card h3{font-size:22px;line-height:1.3;margin-top:2px}.panel-card h4{font-size:18px;line-height:1.35;margin-top:2px}.distribution-panel{border-top:1px solid #E8E8E8;padding-top:12px;margin-top:14px}
    .nav-tabs{border-bottom:1px solid #E8E8E8}.nav-tabs>li>a{color:#4d5963;border-radius:4px 4px 0 0}
    .app-main>.tabbable>.nav-tabs>li.active>a,.app-main>.tabbable>.nav-tabs>li.active>a:focus{color:#9a5200;border-top:3px solid #E08625;background:#fff}
    .nested-tabs>.tabbable>.nav-tabs{margin:8px 0 16px}.nested-tabs>.tabbable>.nav-tabs>li>a{padding:8px 13px;font-size:14.5px}
    .nested-tabs>.tabbable>.nav-tabs>li.active>a,.nested-tabs>.tabbable>.nav-tabs>li.active>a:focus{color:#8a4a06;background:#fff8ef;border-color:#E8E8E8;border-bottom-color:#E08625;box-shadow:inset 0 -2px 0 #E08625}
    .btn-primary{background:#F39C12;border-color:#d98200;color:#2f2108;font-weight:700}.btn-primary:hover,.btn-primary:focus{background:#E08625;border-color:#bd6f12;color:#211605}.btn-primary[disabled]{background:#E8E8E8;border-color:#d2d2d2;color:#777}
    .sidebar-section{border-top:1px solid #E8E8E8;margin-top:14px;padding-top:12px}.info-state{background:#f4f8fb;border-left:3px solid #7da6c2;padding:12px 14px;margin:12px 0}
    .method-grid h3{font-size:22px;line-height:1.3}.method-grid h4{font-size:18px;line-height:1.35;color:#3f4c56;margin-top:18px}.credential-example{background:#f5f6f7;border:1px solid #E8E8E8;padding:10px 12px;color:#27313a}
    .leaflet{min-height:570px}.leaflet-container{background:#F3F5F7}.small-table table{font-size:12.5px}.status-ok,.status-quality{font-size:13px;line-height:1.45}
    @media(max-width:900px){.leaflet{min-height:430px}}
  "))),
  titlePanel("regionalepi – Regionale Infektionsepidemiologie im demografischen Kontext"),
  sidebarLayout(sidebarPanel(
    selectInput("pathogen", "Erreger", pathogen_choices),
    selectInput("window_id", "Beobachtungszeitraum", initial_window_choices,
      selected=tail(unname(initial_window_choices),1L)),
    radioButtons("range_mode", "Analysezeitraum", c("Gesamter Beobachtungszeitraum"="window",
      "RKI-definierter/geprüfter Zeitraum"="reviewed", "Benutzerdefinierter Zeitraum"="custom"), "window"),
    uiOutput("period_availability_note"),
    conditionalPanel("input.range_mode == 'reviewed'", selectInput("reviewed_period_id", "Geprüfter analytischer Zeitraum", character()), uiOutput("reviewed_period_note")),
    conditionalPanel("input.range_mode == 'custom'", dateRangeInput("custom_range", "Benutzerdefinierter Analysezeitraum")),
    div(class="sidebar-section",
    selectInput("typology_mode", "Typologie", c("Dynamische demografische Typologie"="dynamic", "Historische Referenztypologie (2017–2020)"="dissertation"), "dynamic"),
    conditionalPanel("input.typology_mode == 'dynamic'",
      selectInput("demographic_period", "Demografischer Referenzzeitraum", c("2022–2024", "2017–2020"), "2022–2024"),
      selectInput("k", "Anzahl Cluster (explorativ)", 2:5, 3),
      p(class="app-note", "Dynamische Clusterfarben werden aus der demografischen Profilausrichtung zugewiesen; Cluster-IDs bleiben fit-spezifisch.")),
    conditionalPanel("input.typology_mode == 'dissertation'", p(class="app-note", strong("Historische Referenztypologie (2017–2020)"), br(), "Reproduktion der in der Dissertation verwendeten Drei-Cluster-Typologie; k, Indikatorsatz und Definitionen sind fest vorgegeben."))),
    tags$details(tags$summary("Erweiterte Einstellungen"),
      uiOutput("demographic_source_control"),
      checkboxInput("district_overlay", "Ausgewählten Kreis im Zeitverlauf anzeigen", TRUE),
      p(class="app-note", "Live-Abrufe gelten nur für diese Sitzung.")),
    actionButton("load_analysis", "Analyse laden / aktualisieren", class="btn-primary"), br(), br(),
    uiOutput("load_status"), uiOutput("analysis_wide_status"),
    uiOutput("range_status"), uiOutput("demography_status"),
    p(class="app-note", "Bereich, Zoom, Tabs und Auswahl lösen keinen neuen Datenabruf aus.")),
    mainPanel(class="app-main",tabsetPanel(id="analysis_section",
      tabPanel("Übersicht", value="overview",
        div(class="panel-card", uiOutput("analysis_heading"), leaflet::leafletOutput("district_map", height="590px"), uiOutput("map_attribution")),
        fluidRow(column(5,div(class="panel-card",h3("Ausgewählter Kreis"),uiOutput("district_detail"))),
          column(7,div(class="panel-card",h3("Clusterübersicht"),tableOutput("cluster_summary"))))),
      tabPanel("Demografische Typologie", value="typology",
        div(class="nested-tabs",tabsetPanel(id="typology_view",
          tabPanel("Clusterprofile",value="profiles",
            div(class="panel-card",h3("Wie unterscheiden sich die Cluster demografisch?"),h4("Standardisierte demografische Clusterprofile"),p(class="app-note","z < 0: unter dem Durchschnitt aller Kreise · z > 0: über dem Durchschnitt"),plotly::plotlyOutput("profile_plot",height="350px"),uiOutput("cluster_warning")),
            div(class="panel-card",h4("Profile auf Originalskala"),p(class="app-note","Die Originalwerte zeigen die drei Indikatoren in ihren jeweiligen Einheiten."),div(class="small-table",tableOutput("profile_table")))),
          tabPanel("Stabilität der Typologie",value="stability",
            conditionalPanel("input.typology_mode == 'dynamic'",div(class="panel-card",h3("Wie verändert sich die Drei-Cluster-Referenz bei anderer Clusterzahl?"),p(class="app-note",strong("k=3 ist die geprüfte interpretative Referenz.")),plotly::plotlyOutput("stability_plot",height="650px"),div(class="small-table",tableOutput("stability_summary")),uiOutput("stability_note"))),
            conditionalPanel("input.typology_mode == 'dissertation'",div(class="info-state",strong("Historische Referenztypologie (2017–2020)"),p("Für die historische Referenztypologie ist die Drei-Cluster-Lösung fest definiert. Vergleiche mit dynamisch angepassten k=4- oder k=5-Lösungen gehören nicht zu dieser Referenzdefinition.")))),
          tabPanel("Verteilungen der Kreise",value="distributions",
            div(class="panel-card",h3("Verteilungen der Kreise"),p(class="app-note","Bevölkerungsdichte, Durchschnittsalter und Jugendquotient werden auf getrennten Skalen dargestellt."),radioButtons("density_scale","Skalierung Bevölkerungsdichte",c("Original"="original","Logarithmisch"="log10"),"original",inline=TRUE),p(class="app-note","Die Skalierung betrifft ausschließlich die Bevölkerungsdichte. Durchschnittsalter und Jugendquotient werden stets auf der Originalskala dargestellt."),div(class="distribution-panel",plotly::plotlyOutput("demographic_distribution_plot",height="650px"))))))),
      tabPanel("Infektionsgeschehen", value="epidemiology", div(class="panel-card",uiOutput("period_heading")),
        tabsetPanel(id="epidemiology_view",
          tabPanel("Zeitverlauf",plotly::plotlyOutput("incidence_plot",height="440px"),p(class="app-note","Ansicht eingrenzen: Im unteren Zeitbalken kann der sichtbare Ausschnitt verschoben oder vergrößert werden. Dies verändert nur die Darstellung, nicht den gewählten Analysezeitraum."),uiOutput("incidence_warning")),
          tabPanel("Verteilung der Kreise",plotly::plotlyOutput("period_distribution_plot",height="440px")),
          tabPanel("Wöchentliche Clusterunterschiede",
            tabsetPanel(id="weekly_comparison_view",
              tabPanel("Relative Aktivität",h4("Relative Aktivität gegenüber allen Kreisen Deutschlands"),plotly::plotlyOutput("relative_heatmap",height="330px")),
              tabPanel("Paarweise Clusterunterschiede",p(class="app-note","A − B zeigt die Differenz der wöchentlichen medianen Kreisinzidenz zwischen beiden Clustern. Positive Werte bedeuten höhere Werte in A, negative Werte höhere Werte in B."),plotly::plotlyOutput("pairwise_heatmap",height="420px")))),
          tabPanel("Kreis × Woche",p(class="app-note","Rohinzidenz; fehlende Werte sind grau und nicht null."),plotly::plotlyOutput("district_heatmap",height="760px")))),
      tabPanel("Regionale Analyse", value="regional",
        div(class="panel-card",
          fluidRow(
            column(4,radioButtons("regional_level","Räumliche Vergleichsebene",
              c("Großregionen"="grossregion","Regionengruppen aus Ländern"="aggregated_state","Bundesländer"="state"),"grossregion")),
            column(4,uiOutput("regional_focal_control")),
            column(4,uiOutput("regional_comparison_control"))
          ),
          p(class="app-note","Deutschland wird automatisch als Referenz gezeigt. Die regionale Auswahl verändert weder die bundesweite Typologie noch Datenabrufe oder Clusterzuordnungen."),
          uiOutput("regional_analysis_context")),
        tabsetPanel(id="regional_view",
          tabPanel("Demografische Struktur",
            div(class="panel-card",uiOutput("regional_demographic_heading"),plotly::plotlyOutput("regional_composition_plot",height="260px")),
            div(class="panel-card",h4("Demografische Verteilungen in der ausgewählten Region"),radioButtons("regional_density_scale","Skalierung Bevölkerungsdichte",c("Original"="original","Logarithmisch"="log10"),"original",inline=TRUE),p(class="app-note","Die Skalierung betrifft ausschließlich die Bevölkerungsdichte. Durchschnittsalter und Jugendquotient werden stets auf der Originalskala dargestellt."),p(class="app-note","Die Darstellung passt sich an die Zahl verfügbarer Kreise je Regionaltyp an: Bei kleinen Gruppen werden Einzelwerte sowie gegebenenfalls Median oder Boxplot gezeigt; Violinplots erst ab mindestens 10 verfügbaren Kreisen."),plotly::plotlyOutput("regional_demographic_distribution_plot",height="650px"))),
          tabPanel("Zeitverlauf",
            div(class="panel-card",uiOutput("regional_cluster_heading"),plotly::plotlyOutput("regional_cluster_time_plot",height="440px"),uiOutput("regional_cluster_quality")),
            div(class="panel-card",h4("Relative Aktivität innerhalb der ausgewählten Region"),p(class="app-note","Wöchentlicher Median der Kreisinzidenzen eines demografischen Regionaltyps minus Median aller beobachteten Kreise der ausgewählten Region."),plotly::plotlyOutput("regional_relative_activity_plot",height="390px"),p(class="app-note","Positive Werte zeigen eine relativ zum regionalen Wochenmedian höhere, negative Werte eine niedrigere Aktivität. Die Darstellung definiert keinen formalen Wellenbeginn und keine Ausbreitungsrichtung.")),
            div(class="panel-card",h4("Regionaler Kontext"),plotly::plotlyOutput("regional_context_time_plot",height="390px"),p(class="app-note","Die dargestellten regionalen Kurven sind deskriptive Mediane der beobachteten Kreisinzidenzen und keine amtlichen Inzidenzen der Länder oder Großregionen."),uiOutput("regional_context_quality"))),
          tabPanel("Kreise",
            div(class="panel-card",uiOutput("regional_district_heading"),plotly::plotlyOutput("regional_district_plot",height="520px"),p(class="app-note","Innerhalb der ausgewählten Region werden die periodenbezogenen medianen Kreisinzidenzen nach bundesweit bestimmten demografischen Regionaltypen deskriptiv gegenübergestellt. Jeder Punkt entspricht einem Kreis."),p(class="app-note","Die Darstellung beschreibt regionale Unterschiede, erlaubt aber keine kausalen Aussagen über demografische oder administrative Effekte."))))),
      tabPanel("Methodik und Daten", value="methods",
        div(class="nested-tabs",tabsetPanel(id="methods_view",
          tabPanel("Methodik",value="methods_method",uiOutput("methods_methodology")),
          tabPanel("Datenquellen",value="methods_sources",uiOutput("methods_sources")),
          tabPanel("Datenstand und Definitionen",value="methods_status",uiOutput("methods_status")),
          tabPanel("Reproduzierbarkeit",value="methods_reproducibility",uiOutput("methods_reproducibility")))))
    )))
)
