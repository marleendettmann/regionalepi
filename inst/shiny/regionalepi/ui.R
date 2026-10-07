initial_window_choices <- regionalepi:::.shiny_window_choices("Influenza, saisonal")
pathogen_choices <- regionalepi:::.shiny_pathogen_choices()
ui <- fluidPage(
  tags$head(tags$title("regionalepi – Infektionsepidemiologie und demografische Regionaltypologien"), tags$style(HTML("
    :root{--re-primary:#5F627B;--re-primary-hover:#4E5066;--re-primary-soft:#ECECF2;--re-action:#DD7F02;--re-action-hover:#C46F00;--re-action-text:#181D22;--re-action-soft:#FFF3E8;--re-success:#556B13;--re-success-soft:#F4F7E6;--re-warning:#8A4A06;--re-warning-soft:#FFF4E5;--re-error:#8B1E1E;--re-error-soft:#FFF2F2;--re-border:#D9DAE2;--re-bg:#FFFFFF;--re-bg-muted:#F6F6F8;--re-text:#27313A;--re-text-muted:#58616B;--re-focus:#5F627B}
    body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,'Helvetica Neue',Arial,sans-serif;font-size:15px;line-height:1.48;color:var(--re-text);background:var(--re-bg)}
    a{color:var(--re-primary)}a:hover,a:focus{color:var(--re-primary-hover)}.container-fluid>h2{font-size:26px;line-height:1.25;margin-bottom:16px}.app-note{color:var(--re-text-muted);font-size:12.5px;line-height:1.5}
    input[type='radio'],input[type='checkbox']{accent-color:var(--re-primary)}
    .form-control:focus,.selectize-input.focus,.selectize-input.input-active{border-color:var(--re-primary);box-shadow:0 0 0 3px rgba(95,98,123,.22);outline:0}.selectize-dropdown .active{color:var(--re-text);background:var(--re-primary-soft)}.selectize-dropdown .option.disabled{color:#666;background:var(--re-bg-muted);cursor:not-allowed}.progress-bar{background-color:var(--re-primary)}
    a:focus-visible,button:focus-visible,.btn:focus-visible,summary:focus-visible{outline:3px solid rgba(95,98,123,.45);outline-offset:2px}
    .status-error{color:var(--re-error);background:var(--re-error-soft);border-left:3px solid var(--re-error);padding:7px 10px;font-weight:600}
    .status-ok{color:var(--re-success);background:var(--re-success-soft);border-left:3px solid var(--re-success);padding:7px 10px}
    .status-quality{color:var(--re-warning);background:var(--re-warning-soft);border-left:3px solid var(--re-warning);padding:7px 10px}
    .panel-card{border:1px solid var(--re-border);border-radius:6px;padding:16px 18px;margin:16px 0 22px;background:var(--re-bg);box-shadow:0 1px 2px rgba(34,36,38,.04)}
    .panel-card h3{font-size:22px;line-height:1.3;margin-top:2px}.panel-card h4{font-size:18px;line-height:1.35;margin-top:2px}.distribution-panel{border-top:1px solid #E8E8E8;padding-top:12px;margin-top:14px}
    .nav-tabs{border-bottom:1px solid var(--re-border)}.nav-tabs>li>a{color:var(--re-text-muted);border-radius:4px 4px 0 0}.nav-tabs>li>a:hover,.nav-tabs>li>a:focus{color:var(--re-primary-hover);background:var(--re-bg-muted);border-color:var(--re-border)}
    .app-main>.tabbable>.nav-tabs>li.active>a,.app-main>.tabbable>.nav-tabs>li.active>a:hover,.app-main>.tabbable>.nav-tabs>li.active>a:focus{color:var(--re-primary-hover);border-top:3px solid var(--re-primary);background:var(--re-bg)}
    .nested-tabs>.tabbable>.nav-tabs{margin:8px 0 16px}.nested-tabs>.tabbable>.nav-tabs>li>a{padding:8px 13px;font-size:14.5px}
    .nested-tabs>.tabbable>.nav-tabs>li.active>a,.nested-tabs>.tabbable>.nav-tabs>li.active>a:hover,.nested-tabs>.tabbable>.nav-tabs>li.active>a:focus{color:var(--re-primary-hover);background:var(--re-primary-soft);border-color:var(--re-border);border-bottom-color:var(--re-primary);box-shadow:inset 0 -2px 0 var(--re-primary)}
    .btn-primary{background:var(--re-action);border-color:var(--re-action);color:var(--re-action-text);font-weight:700}.btn-primary:hover,.btn-primary:focus,.btn-primary:active,.btn-primary:active:hover{background:var(--re-action-hover);border-color:var(--re-action-hover);color:var(--re-action-text)}.btn-primary:focus{box-shadow:0 0 0 3px rgba(95,98,123,.32)}.btn-primary[disabled]{background:#E8E8E8;border-color:#D2D2D2;color:#666}
    .sidebar-section{border-top:1px solid var(--re-border);margin-top:16px;padding-top:12px}.sidebar-section:first-child{margin-top:0}.sidebar-heading{color:var(--re-primary-hover);font-size:13px;font-weight:600;letter-spacing:.02em;margin:0 0 11px}.sidebar-section details{margin:3px 0 14px}.sidebar-section summary{display:block;position:relative;width:100%;padding:8px 34px 8px 10px;border:1px solid var(--re-border);border-radius:5px;background:var(--re-primary-soft);color:var(--re-text);font-weight:600;cursor:pointer;list-style:none}.sidebar-section summary::-webkit-details-marker{display:none}.sidebar-section summary::after{content:'\\25B8';position:absolute;right:11px;color:var(--re-primary)}.sidebar-section details[open]>summary::after{content:'\\25BE'}.sidebar-section details[open]>summary{margin-bottom:12px}.info-state{background:var(--re-primary-soft);border-left:3px solid var(--re-primary);padding:12px 14px;margin:12px 0}
    .method-grid h3{font-size:22px;line-height:1.3}.method-grid h4{font-size:18px;line-height:1.35;color:#3f4c56;margin-top:18px}.credential-example{background:var(--re-bg-muted);border:1px solid var(--re-border);padding:10px 12px;color:var(--re-text)}
    .leaflet{min-height:570px}.leaflet-container{background:#F3F5F7}.small-table table{font-size:12.5px}.status-ok,.status-quality{font-size:13px;line-height:1.45}
    @media(max-width:991px){.container-fluid>h2{font-size:22px}.panel-card{padding:14px}.leaflet{min-height:430px}}
    @media(max-width:767px){.btn-primary{width:100%}.nav-tabs{display:flex;flex-wrap:nowrap;overflow-x:auto;overflow-y:hidden}.nav-tabs>li{float:none;flex:0 0 auto}.nav-tabs>li>a{white-space:nowrap}.leaflet{min-height:400px}}
  "))),
  titlePanel("regionalepi – Infektionsepidemiologie und demografische Regionaltypologien"),
  sidebarLayout(sidebarPanel(
    div(class="sidebar-section",
    div(class="sidebar-heading", "Infektionsgeschehen"),
    selectInput("pathogen", "Erreger", pathogen_choices),
    selectInput("window_id", "Beobachtungszeitraum", initial_window_choices,
      selected=tail(unname(initial_window_choices),1L)),
    radioButtons("range_mode", "Analysezeitraum", c("Gesamter Beobachtungszeitraum"="window",
      "Vordefinierter epidemiologischer Zeitraum"="reviewed", "Benutzerdefinierter Zeitraum"="custom"), "window"),
    uiOutput("range_mode_note"),
    uiOutput("period_availability_note"),
    conditionalPanel("input.range_mode == 'reviewed'", selectInput("reviewed_period_id", "Vordefinierter epidemiologischer Zeitraum", character()), uiOutput("reviewed_period_note")),
    conditionalPanel("input.range_mode == 'custom'",
      selectInput("custom_start_week", "Start-KW", character()),
      selectInput("custom_end_week", "End-KW", character()))),
    div(class="sidebar-section",
    div(class="sidebar-heading", "Demografische Typologie"),
    selectInput("typology_mode", "Typologie", c("Aktualisierte demografische Typologie"="dynamic", "Historische Referenztypologie (2017–2020)"="dissertation"), "dynamic"),
    conditionalPanel("input.typology_mode == 'dynamic'",
      selectInput("demographic_period", "Demografischer Referenzzeitraum", c("2022–2025", "2017–2020"), "2022–2025"),
      selectInput("k", "Anzahl Cluster (explorativ)", 2:5, 3),
      p(class="app-note", "Für den gewählten demografischen Referenzzeitraum wird mit k-Means eine aktualisierte demografische Typologie mit der gewählten Anzahl an Clustern ermittelt."),
      p(class="app-note", "Der demografische Referenzzeitraum bestimmt die Datenbasis der Typologie und ist vom epidemiologischen Beobachtungs- und Analysezeitraum getrennt. 2017–2020 liegt der historischen Referenztypologie zugrunde; 2022–2025 ist der aktuell geprüfte Snapshot-Zeitraum auf Basis des Zensus 2022."),
      p(class="app-note", "Die Farben der Cluster orientieren sich an ihren demografischen Profilen; die Cluster-IDs gelten nur für die jeweilige Berechnung.")),
    conditionalPanel("input.typology_mode == 'dissertation'", p(class="app-note", strong("Historische Referenztypologie (2017–2020)"), br(), "Reproduziert die festgelegte historische Drei-Cluster-Lösung."))),
    div(class="sidebar-section",
    div(class="sidebar-heading", "Daten & Aktualisierung"),
    tags$details(tags$summary("Erweiterte Einstellungen"),
      uiOutput("demographic_source_control"),
      checkboxInput("district_overlay", "Ausgewählten Kreis im Zeitverlauf anzeigen", TRUE),
      p(class="app-note", "Live-Abrufe gelten nur für diese Sitzung.")),
    actionButton("load_analysis", "Analyse laden / aktualisieren", class="btn-primary"), br(), br(),
    uiOutput("load_status"), uiOutput("analysis_wide_status"),
    uiOutput("range_status"), uiOutput("demography_status"),
    p(class="app-note", "Bereich, Zoom, Tabs und Auswahl lösen keinen neuen Datenabruf aus."))),
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
          tabPanel("Zeitverlauf",plotly::plotlyOutput("incidence_plot",height="440px"),uiOutput("incidence_early_window_note"),uiOutput("incidence_warning"),p(class="app-note","Ansicht eingrenzen: Im unteren Zeitbalken kann der sichtbare Ausschnitt verschoben oder vergrößert werden. Dies verändert nur die Darstellung, nicht den gewählten Analysezeitraum.")),
          tabPanel("Verteilung der Kreise",plotly::plotlyOutput("period_distribution_plot",height="440px")),
          tabPanel("Wöchentliche Clusterunterschiede",
            tabsetPanel(id="weekly_comparison_view",
              tabPanel("Relative Aktivität",h4("Relative Aktivität im bundesweiten Vergleich"),p(class="app-note","Abweichung des wöchentlichen Medians eines demografischen Regionaltyps vom Median aller beobachteten Kreise Deutschlands. Positive Werte zeigen eine höhere, negative Werte eine niedrigere mediane Inzidenz."),plotly::plotlyOutput("relative_heatmap",height="330px")),
              tabPanel("Paarweise Clusterunterschiede",p(class="app-note","A − B bezeichnet den wöchentlichen Median der Kreisinzidenzen von Regionaltyp A minus den entsprechenden Median von Regionaltyp B. Positive Werte bedeuten höhere Werte in A, negative Werte höhere Werte in B."),plotly::plotlyOutput("pairwise_heatmap",height="420px")))),
          tabPanel("Kreis × Woche",p(class="app-note","Zeigt die wöchentliche Inzidenz jedes Kreises im Analysezeitraum. Zeilen entsprechen Kreisen, Spalten den Wochen. Die Kreise sind nach demografischem Regionaltyp gruppiert; die Farbintensität zeigt die Höhe der Inzidenz. Fehlende Werte sind grau und werden nicht als Null interpretiert."),plotly::plotlyOutput("district_heatmap",height="760px")))),
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
            div(class="panel-card",h4("Relative Aktivität innerhalb der ausgewählten Region"),p(class="app-note","Die regionale relative Aktivität zeigt, wie stark der wöchentliche Median eines demografischen Regionaltyps vom Median aller beobachteten Kreise der ausgewählten Region abweicht. Positive Werte liegen über, negative Werte unter dem regionalen Median."),plotly::plotlyOutput("regional_relative_activity_plot",height="390px"),p(class="app-note","Die Darstellung definiert keinen formalen Wellenbeginn und keine Ausbreitungsrichtung.")),
            div(class="panel-card",h4("Regionaler Kontext"),plotly::plotlyOutput("regional_context_time_plot",height="390px"),p(class="app-note","Die dargestellten regionalen Kurven zeigen Mediane der beobachteten Kreisinzidenzen; sie entsprechen nicht den Inzidenzen der Länder oder Großregionen."),uiOutput("regional_context_quality"))),
          tabPanel("Kreise",
            div(class="panel-card",uiOutput("regional_district_heading"),plotly::plotlyOutput("regional_district_plot",height="520px"),p(class="app-note","Innerhalb der ausgewählten Region werden die über den Analysezeitraum zusammengefassten Kreisinzidenzen nach den bundesweit bestimmten demografischen Regionaltypen gegenübergestellt. Jeder Punkt entspricht einem Kreis; dargestellt wird der Median seiner beobachteten Wocheninzidenzen im Analysezeitraum."),p(class="app-note","Die Darstellung dient dem deskriptiven Vergleich innerhalb der ausgewählten Region. Unterschiede zwischen den Regionaltypen sind nicht als kausale Effekte demografischer oder administrativer Merkmale zu interpretieren."))))),
      tabPanel("Methodik und Daten", value="methods",
        div(class="nested-tabs",tabsetPanel(id="methods_view",
          tabPanel("Methodik",value="methods_method",uiOutput("methods_methodology")),
          tabPanel("Datenquellen",value="methods_sources",uiOutput("methods_sources")),
          tabPanel("Datenstand und Definitionen",value="methods_status",uiOutput("methods_status")),
          tabPanel("Reproduzierbarkeit",value="methods_reproducibility",uiOutput("methods_reproducibility")))))
    )))
)
