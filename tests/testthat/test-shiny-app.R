test_that("Shiny PoC defaults and reviewed period choices are stable", {
  periods <- regionalepi:::.shiny_demographic_periods()
  expect_identical(names(periods), c("2022–2024", "2017–2020"))
  expect_identical(periods[[1L]], 2022:2024)

  influenza <- regionalepi:::.shiny_period_choices("Influenza, saisonal")
  expect_named(influenza, "RKI-geprüfte Influenzawellen")
  influenza_flat <- influenza[[1L]]
  expect_false(any(grepl("2020/21", names(influenza_flat), fixed = TRUE)))
  expect_identical(sum(grepl("2022/23", names(influenza_flat), fixed = TRUE)), 2L)

  covid <- regionalepi:::.shiny_period_choices("COVID-19")
  expect_identical(names(covid), c(
    "Dissertation / RKI-Pandemieperioden", "RKI-geprüfte Aktivitätswellen"))
  expect_identical(length(covid[[1L]]), 7L)
  expect_identical(length(covid[[2L]]), 2L)
  registry <- regionalepi:::.shiny_pathogen_registry()
  expect_identical(names(registry), c(
    "Influenza, saisonal", "COVID-19", "Norovirus-Gastroenteritis"
  ))
  expect_identical(
    registry[["Norovirus-Gastroenteritis"]]$survstat_member,
    "Norovirus-Gastroenteritis"
  )
  expect_length(registry[["Norovirus-Gastroenteritis"]]$period_factories, 0L)
})

test_that("historical pandemic frame exposes all reviewed dissertation periods", {
  window <- regionalepi:::.shiny_selected_window(
    "COVID-19", "covid19_pandemic_2020_22")
  periods <- regionalepi:::.shiny_periods_in_window(window)
  dissertation <- periods[periods$period_system ==
    "Dissertation / RKI-Pandemieperioden", ]
  expect_identical(nrow(dissertation), 7L)
  expect_identical(dissertation$period_id, paste0("covid_wave_", c(
    "1", "2", "3", "4a", "4b", "5a", "5b")))
  expect_true(all(dissertation$start_date >= window$start_date))
  expect_true(all(dissertation$end_date <= window$end_date))
  choices <- regionalepi:::.shiny_period_choices_in_window(window)
  expect_identical(names(choices), c(
    "Dissertation / RKI-Pandemieperioden", "RKI-geprüfte Aktivitätswellen")[1L])
  expect_identical(length(choices[[1L]]), 7L)
  for (mode in c("dissertation", "dynamic")) {
    expect_identical(regionalepi:::.shiny_periods_in_window(window)$period_id,
      periods$period_id)
  }
})

test_that("period context is metadata driven and makes typology explicit", {
  window <- regionalepi:::.shiny_selected_window(
    "COVID-19", "covid19_pandemic_2020_22")
  period <- regionalepi:::.shiny_periods_in_window(window)
  period <- period[period$period_id == "covid_wave_3", ]
  range <- regionalepi:::.shiny_select_analysis_range(window, "reviewed", period)
  dynamic <- regionalepi:::.shiny_period_context(
    "COVID-19", range, period, "dynamic", 2017:2020, 4L)
  expect_identical(dynamic$title, "COVID-19 · 3. Welle / Alpha")
  expect_identical(dynamic$subtitle,
    "01.03.2021–13.06.2021 · 2021-KW09–KW23")
  expect_identical(dynamic$typology,
    "Typologie: Dynamisch · Referenzzeitraum 2017–2020 · k=4")
  reference <- regionalepi:::.shiny_period_context(
    "COVID-19", range, period, "dissertation", 2017:2020, 3L)
  expect_identical(reference$typology,
    "Typologie: Historische Referenztypologie (2017–2020)")
})

test_that("Shiny cache keys respect reactive source boundaries", {
  demographic <- regionalepi:::.shiny_demographic_cache_key("2022–2024")
  expect_identical(demographic, "demography:snapshot:2022-2023-2024")
  expect_identical(
    regionalepi:::.shiny_demographic_cache_key("2022–2024"), demographic
  )
  expect_false(grepl("Influenza|COVID|k", demographic))
  expect_false(identical(demographic,
    regionalepi:::.shiny_demographic_cache_key("2022–2024", "live")))

  surveillance <- regionalepi:::.shiny_surveillance_cache_key(
    "Influenza, saisonal", c(2023, 2022)
  )
  expect_identical(
    surveillance,
    regionalepi:::.shiny_surveillance_cache_key("Influenza, saisonal", 2022:2023)
  )
  expect_false(grepl("2017–2020|2022–2024|k", surveillance))

  first <- regionalepi:::.shiny_summary_cache_key(surveillance, "wave-1", "fit-a")
  second <- regionalepi:::.shiny_summary_cache_key(surveillance, "wave-2", "fit-a")
  expect_false(identical(first, second))

  index <- list(
    broad = list(key = "broad", pathogen = "COVID-19",
                 reporting_years = 2020:2022),
    narrow = list(key = "narrow", pathogen = "COVID-19",
                  reporting_years = 2021:2022)
  )
  expect_identical(regionalepi:::.shiny_surveillance_cache_match(
    index, "COVID-19", 2021
  ), "narrow")
  expect_null(regionalepi:::.shiny_surveillance_cache_match(
    index, "Influenza, saisonal", 2021
  ))
  bundle <- regionalepi:::.shiny_surveillance_bundle_cache_key(
    "Influenza, saisonal", 2025:2026)
  expect_match(bundle, "survstat-incidence-counts", fixed = TRUE)
  expect_match(bundle, "reference-definition", fixed = TRUE)
})

test_that("typology modes retain distinct identities and palettes", {
  reference <- regionalepi:::.shiny_typology_config("dissertation")
  expect_identical(reference$years, 2017:2020)
  expect_identical(reference$k, 3L)
  expect_identical(regionalepi:::.shiny_app_cluster_colours(
    c("ClD", "ClJ", "ClA"), "dissertation"),
    c(ClD = "#bc5e21", ClJ = "#748c61", ClA = "#274f66"))
  dynamic <- regionalepi:::.shiny_app_cluster_colours(sprintf("C%02d", 1:5))
  expect_identical(length(unique(dynamic)), 5L)
  expect_false(any(dynamic %in% c("#bc5e21", "#748c61", "#274f66")))
  expect_identical(regionalepi:::.shiny_typology_cache_key(
    list(data = data.frame()), 3L, "dissertation"),
    "typology:dissertation_v1:historical_reference")
})

test_that("profile-aligned k3 colours depend on profiles rather than C IDs", {
  ids <- c("population_density", "mean_age", "youth_dependency_ratio")
  reference <- data.frame(display_cluster_id=rep(c("ClD","ClJ","ClA"),each=3),
    indicator_id=rep(ids,3),standardized_center=c(2,0,-1,-1,-1,2,0,2,-1))
  dynamic <- data.frame(display_cluster_id=rep(c("C01","C02","C03"),each=3),
    indicator_id=rep(ids,3),standardized_center=c(0,2,-1,2,0,-1,-1,-1,2))
  alignment <- regionalepi:::.shiny_profile_palette_alignment(dynamic,reference)
  expect_identical(alignment$mapping,c(C01="ClA",C02="ClD",C03="ClJ"))
  expect_false(alignment$ambiguous)
  expect_gt(alignment$margin,0)
  expect_identical(regionalepi:::.shiny_app_cluster_colours(
    c("C01","C02","C03"),"dynamic","profile_aligned",alignment),
    c(C01="#274f66",C02="#bc5e21",C03="#748c61"))
  ambiguous <- dynamic; ambiguous$standardized_center <- 0
  expect_error(regionalepi:::.shiny_profile_palette_alignment(ambiguous,reference),
    "ambiguous")
})

test_that("current dynamic colours follow profile continuity for k3 to k5", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2024)$summary
  reference <- regionalepi:::.shiny_fit_typology(
    regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary,
    "dissertation",3L)
  anchor <- regionalepi:::.shiny_fit_typology(summary,"dynamic",3L)
  expected <- list(
    `3`=c(C01="#274f66",C02="#748c61",C03="#bc5e21"),
    `4`=c(C01="#274f66",C02="#748c61",C03="#7B61A8",C04="#bc5e21"),
    `5`=c(C01="#7B61A8",C02="#274f66",C03="#748c61",
          C04="#A94F74",C05="#bc5e21"))
  for(k in 3:5) {
    fit <- regionalepi:::.shiny_fit_typology(summary,"dynamic",k)
    policy <- regionalepi:::.shiny_dynamic_colour_policy(fit,reference,anchor)
    expect_identical(policy$colours,expected[[as.character(k)]])
    expect_identical(length(unique(policy$colours)),k)
  }
})

test_that("k2 assigns a dense anchor and an explicit mixed-profile colour", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2024)$summary
  historical <- regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary
  reference <- regionalepi:::.shiny_fit_typology(historical,"dissertation",3L)
  anchor <- regionalepi:::.shiny_fit_typology(summary,"dynamic",3L)
  fit <- regionalepi:::.shiny_fit_typology(summary,"dynamic",2L)
  policy <- regionalepi:::.shiny_dynamic_colour_policy(fit,reference,anchor)
  expect_identical(policy$continuation,c(C03="C02"))
  expect_identical(policy$colours,c(C01="#7B61A8",C02="#bc5e21"))
  expect_gt(policy$target_anchor_fraction["C03","C02"],.97)
  expect_lt(policy$center_distances["C03","C02"],.06)
  expect_lt(max(policy$target_anchor_fraction[,"C01"]),.8)
  metadata <- regionalepi:::.shiny_cluster_display_metadata(
    fit,"dynamic","profile_aligned",policy,
    regionalepi_map_geometry()$features$geo_id)
  expect_identical(metadata$profile_anchor,c(NA_character_,"dense"))
  expect_identical(metadata$display_colour,c("#7B61A8","#bc5e21"))
  expect_identical(length(unique(metadata$display_colour)),2L)
})

test_that("custom ranges handle exact boundaries one week and empty subsets", {
  window <- regionalepi:::.shiny_selected_window("COVID-19","covid19_2020_21")
  exact <- regionalepi:::.shiny_select_analysis_range(window,"custom",
    custom_dates=as.Date(c("2020-05-11","2021-05-23")))
  expect_identical(exact$start_date,window$start_date)
  expect_identical(exact$end_date,window$end_date)
  expect_identical(length(seq(exact$start_date,exact$end_date,by="week")),54L)
  one_week <- regionalepi:::.shiny_select_analysis_range(window,"custom",
    custom_dates=as.Date(c("2020-05-11","2020-05-11")))
  expect_identical(one_week$start_date,one_week$end_date)
  expect_error(regionalepi:::.shiny_select_analysis_range(window,"custom",
    custom_dates=as.Date(c("2020-05-04","2020-05-11"))),"au\u00dferhalb")

  fit <- list(assignments=data.frame(geo_id=c("00001","00002"),
    display_cluster_id=c("C01","C02")),provenance=list(fit_id="fit"),
    indicator_set=list(definition_version="v1"))
  observations <- data.frame(geo_id=rep(c("00001","00002"),2),
    geo_name=rep(c("A","B"),2),date=rep(as.Date(c("2020-05-11","2020-05-18")),each=2),
    reporting_year=2020L,reporting_week=rep(c(20L,21L),each=2),
    incidence=c(0,NA,2,3),cases=c(0,NA,2,3),stringsAsFactors=FALSE)
  bundle <- list(data=observations)
  attached <- regionalepi:::.shiny_attach_typology_range(bundle,fit,one_week)
  expect_identical(nrow(attached),2L)
  expect_identical(attached$incidence,c(0,NA_real_))
  expect_no_error(regionalepi:::.shiny_exploration_summaries(bundle,fit,one_week))
  empty_range <- list(start_date=as.Date("2020-06-01"),end_date=as.Date("2020-06-08"))
  empty <- regionalepi:::.shiny_attach_typology_range(bundle,fit,empty_range)
  expect_identical(nrow(empty),0L)
  expect_true(all(c("cluster_id","typology_id","typology_definition_version")%in%names(empty)))
  expect_error(regionalepi:::.shiny_exploration_summaries(bundle,fit,empty_range),
    "keine darstellbaren Beobachtungen")
})

test_that("one display metadata contract fixes identity order labels and colours", {
  current <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2024)$summary
  historical <- regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary
  reference <- regionalepi:::.shiny_fit_typology(historical, "dissertation", 3L)
  map_ids <- regionalepi_map_geometry()$features$geo_id
  reference_metadata <- regionalepi:::.shiny_cluster_display_metadata(
    reference, "dissertation", display_geo_ids = map_ids)
  expect_identical(reference_metadata$display_cluster_id, c("ClD", "ClJ", "ClA"))
  expect_identical(reference_metadata$display_colour,
    c("#bc5e21", "#748c61", "#274f66"))
  expect_identical(reference_metadata$display_label, c(
    "ClD \u00b7 dichte Regionen", "ClJ \u00b7 familiengepr\u00e4gte Regionen",
    "ClA \u00b7 \u00e4ltere, l\u00e4ndliche Regionen"))
  expect_identical(sum(reference_metadata$n_districts), 400L)
  map <- regionalepi_map_geometry()
  joined <- regionalepi:::.shiny_map_assignments(map, reference)$data
  widget <- regionalepi:::.shiny_app_leaflet_geojson(
    map$browser_geojson, regionalepi:::.shiny_app_map_bounds(map), joined,
    "dissertation", regionalepi_state_boundaries()$browser_geojson,
    regionalepi:::.shiny_germany_outline_geojson(),
    display_metadata = reference_metadata)
  map_display <- widget$jsHooks$render[[1L]]$data
  representatives <- c(ClD="01001",ClJ="01003",ClA="01004")
  expect_identical(unlist(map_display$clusters[representatives],use.names=FALSE),
    names(representatives))
  expect_identical(unlist(map_display$colours[names(representatives)],use.names=FALSE),
    c("#bc5e21", "#748c61", "#274f66"))

  for (years_summary in list(current, historical)) {
    anchor <- regionalepi:::.shiny_fit_typology(years_summary, "dynamic", 3L)
    policy <- regionalepi:::.shiny_dynamic_colour_policy(anchor, reference, anchor)
    metadata <- regionalepi:::.shiny_cluster_display_metadata(
      anchor, "dynamic", "profile_aligned", policy, map_ids)
    expect_identical(metadata$display_cluster_id, c("C01", "C02", "C03"))
    expect_identical(metadata$profile_anchor,
      c("older_low_density", "family_youth", "dense"))
    expect_identical(metadata$display_colour,
      c("#274f66", "#748c61", "#bc5e21"))
    applied <- regionalepi:::.shiny_apply_display_metadata(
      data.frame(cluster_id = rev(metadata$display_cluster_id)), metadata)
    expect_identical(levels(applied$cluster_id), metadata$display_cluster_id)
    expect_identical(as.character(applied$display_colour),
      metadata$display_colour[match(as.character(applied$cluster_id),
                                    metadata$display_cluster_id)])
  }
})

test_that("dynamic profile descriptions lead with the strongest feature", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2024)$summary
  display <- regionalepi:::.shiny_app_indicator_display()
  for (k in 2:5) {
    fit <- regionalepi:::.shiny_fit_typology(summary, "dynamic", k)
    descriptions <- regionalepi:::.shiny_profile_descriptions(fit$profiles)
    for (cluster in unique(fit$profiles$display_cluster_id)) {
      profile <- fit$profiles[fit$profiles$display_cluster_id == cluster, ]
      strongest <- profile$indicator_id[
        order(-abs(profile$standardized_center),
              match(profile$indicator_id, display$indicator_id))[[1L]]]
      if (max(abs(profile$standardized_center)) >= 0.5) {
        first_label <- display$label[match(strongest, display$indicator_id)]
        description <- descriptions$profile_description[
          descriptions$display_cluster_id == cluster]
        expect_match(description, first_label, fixed = TRUE)
        expect_lt(regexpr(first_label, description, fixed = TRUE)[[1L]],
                  regexpr(",", paste0(description, ","), fixed = TRUE)[[1L]])
      }
    }
  }
  k3 <- regionalepi:::.shiny_fit_typology(summary, "dynamic", 3L)
  k3_description <- regionalepi:::.shiny_profile_descriptions(k3$profiles)
  expect_match(k3_description$profile_description[
    k3_description$display_cluster_id == "C03"], "Bev\u00f6lkerungsdichte",
    fixed = TRUE)
})

test_that("partition comparison reports ARI overlap and center distances", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2024)$summary
  fits <- lapply(3:5,function(k)regionalepi:::.shiny_fit_typology(summary,"dynamic",k))
  three_four <- regionalepi:::.shiny_compare_partitions(fits[[1L]],fits[[2L]])
  four_five <- regionalepi:::.shiny_compare_partitions(fits[[2L]],fits[[3L]])
  expect_equal(three_four$ari,0.9391039,tolerance=1e-7)
  expect_equal(four_five$ari,0.4711732,tolerance=1e-7)
  expect_identical(sum(three_four$contingency),400L)
  expect_identical(dim(three_four$center_distances),c(3L,4L))
  expect_named(three_four$transition,c("source_k","source_cluster","target_k",
    "target_cluster","n_shared","source_fraction","target_fraction",
    "source_center_distance"))
})

test_that("typology stability contract uses membership and conserves fractions", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2024)$summary
  fits <- lapply(2:5,function(k)
    regionalepi:::.shiny_fit_typology(summary,"dynamic",k))
  stability <- regionalepi:::.shiny_typology_stability(fits)
  expect_identical(stability$reference_k,3L)
  expect_identical(stability$summary$target_k,c(2L,4L,5L))
  expect_equal(stability$summary$ari,
    c(0.3879296,0.9391039,0.4334176),tolerance=1e-7)
  required <- c("reference_fit_id","reference_cluster","target_fit_id",
    "target_cluster","n_shared","reference_n","target_n",
    "reference_fraction","target_fraction","reference_center_distance",
    "target_k")
  expect_named(stability$transitions,required)
  for (target_k in c(2L,4L,5L)) {
    x <- stability$transitions[stability$transitions$target_k==target_k,]
    expect_equal(tapply(x$n_shared,x$reference_cluster,sum),
      tapply(x$reference_n,x$reference_cluster,unique))
    expect_equal(as.vector(tapply(x$reference_fraction,x$reference_cluster,sum)),
      rep(1,3))
    expect_equal(as.vector(tapply(x$target_fraction,x$target_cluster,sum)),
      rep(1,target_k))
  }
  expect_match(regionalepi:::.shiny_stability_interpretation(stability),
    "grobe Zweiteilung",fixed=TRUE)
  broken <- fits[[2L]]
  broken$assignments <- rbind(broken$assignments,broken$assignments[1L,])
  expect_error(regionalepi:::.shiny_typology_transition(broken,fits[[1L]]),
    "unique geo_id")
})

test_that("stability and pairwise sections use reviewed visual hierarchy", {
  ui_text <- paste(readLines(file.path(regionalepi:::.shiny_app_dir(), "ui.R"),
    warn=FALSE),collapse="\n")
  expect_match(ui_text,"Wie verändert sich die Drei-Cluster-Referenz",fixed=TRUE)
  expect_match(ui_text,'tabPanel("Paarweise Clusterunterschiede"',fixed=TRUE)
  expect_match(ui_text,"A − B zeigt die Differenz",fixed=TRUE)
  body <- paste(deparse(body(regionalepi:::.shiny_app_leaflet_geojson)),collapse="\n")
  expect_match(body,'color = "#F7F7F3"',fixed=TRUE)
  expect_match(body,"weight = 1.2",fixed=TRUE)
  expect_match(body,"opacity = 0.98",fixed=TRUE)
  expect_match(body,'color = "#27313A"',fixed=TRUE)
  expect_match(body,"weight = 2.1",fixed=TRUE)
})

test_that("stability alluvials compare k2 k4 and k5 independently with k3", {
  skip_if_not_installed("plotly")
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2024)$summary
  fits <- lapply(2:5,function(k)
    regionalepi:::.shiny_fit_typology(summary,"dynamic",k))
  stability <- regionalepi:::.shiny_typology_stability(fits)
  metadata <- regionalepi:::.shiny_cluster_display_metadata(fits[[2L]])
  widget <- regionalepi:::.shiny_stability_alluvial_widget(stability,metadata)
  built <- plotly::plotly_build(widget)
  expect_identical(length(built$x$data),3L)
  expect_true(all(vapply(built$x$data,function(x)x$type,character(1L))=="sankey"))
  expect_identical(vapply(built$x$data,function(x)sum(x$link$value),integer(1L)),
    c(400L,400L,400L))
  expect_identical(vapply(built$x$data,function(x)as.integer(sub("k=([245]).*","\\1",
    tail(x$node$label,1L))),integer(1L)),c(2L,4L,5L))
  expect_true(all(vapply(built$x$data,function(x)
    all(grepl("^k=3 ",head(x$node$label,3L))),logical(1L))))
  expect_match(paste(vapply(built$x$layout$annotations,`[[`,character(1L),"text"),
    collapse=" "),"k=3 → k=2 k=3 → k=4 k=3 → k=5",fixed=TRUE)
})

test_that("population-density scaling is display-only and log-safe", {
  ui_text <- paste(readLines(file.path(regionalepi:::.shiny_app_dir(),"ui.R"),
    warn=FALSE),collapse="\n")
  server_text <- paste(readLines(file.path(regionalepi:::.shiny_app_dir(),
    "server.R"),warn=FALSE),collapse="\n")
  expect_match(ui_text,"Skalierung Bevölkerungsdichte",fixed=TRUE)
  expect_match(ui_text,'c("Original"="original","Logarithmisch"="log10")',
    fixed=TRUE)
  expect_match(ui_text,paste("Die Skalierung betrifft ausschließlich die",
    "Bevölkerungsdichte. Durchschnittsalter und Jugendquotient werden stets",
    "auf der Originalskala dargestellt."),fixed=TRUE)
  expect_match(server_text,"scale_y_log10",fixed=TRUE)
  expect_match(server_text,"positive finite values",fixed=TRUE)
  expect_match(server_text,"Darstellung: logarithmische Achse",fixed=TRUE)
  expect_false(grepl("density_scale",paste(c(
    deparse(body(regionalepi:::.shiny_demographic_cache_key)),
    deparse(body(regionalepi:::.shiny_typology_cache_key)),
    deparse(body(regionalepi:::.shiny_surveillance_cache_key))),collapse="\n")))
  for(i in seq_along(list(2017:2020,2022:2024))) {
    years<-list(2017:2020,2022:2024)[[i]]
    x<-regionalepi:::.shiny_fetch_snapshot_demography(years)$summary$data
    x<-x$indicator_value[x$indicator_id=="population_density"]
    expect_length(x,c(401L,400L)[[i]])
    expect_true(all(is.finite(x)))
    expect_true(all(x>0))
  }
})

test_that("finite-n display geometry follows the reviewed thresholds", {
  expected <- c(
    `0`="unavailable", `1`="point_only", `2`="points_and_median",
    `4`="points_and_median", `5`="points_and_boxplot",
    `9`="points_and_boxplot", `10`="points_boxplot_and_violin"
  )
  for (n in as.integer(names(expected))) {
    values <- if (n) c(seq_len(n) - 1, NA_real_, NaN, Inf, -Inf) else
      c(NA_real_, NaN, Inf, -Inf)
    rule <- regionalepi:::.shiny_finite_n_display(values)
    expect_identical(rule$n_finite, n)
    expect_identical(rule$display_rule, unname(expected[as.character(n)]))
    expect_identical(rule$show_points, n > 0L)
    expect_identical(rule$show_median, n >= 2L && n <= 4L)
    expect_identical(rule$show_boxplot, n >= 5L)
    expect_identical(rule$show_violin, n >= 10L)
  }

  sizes <- c(A=1L,B=2L,C=4L,D=5L,E=9L,F=10L)
  data <- do.call(rbind,lapply(names(sizes),function(id)data.frame(
    cluster_id=id,value=seq_len(sizes[[id]])-1,
    geo_id=sprintf("%s%02d",id,seq_len(sizes[[id]])),
    stringsAsFactors=FALSE)))
  data <- rbind(data,data.frame(cluster_id="F",value=NA_real_,geo_id="FNA"))
  original <- data
  layers <- regionalepi:::.shiny_adaptive_distribution_layers(
    data,"cluster_id","value")
  expect_identical(data,original)
  expect_identical(stats::setNames(layers$rules$n_finite,
    layers$rules$cluster_id),sizes)
  expect_setequal(unique(layers$violins$cluster_id),"F")
  expect_setequal(unique(layers$boxplots$cluster_id),c("D","E","F"))
  expect_setequal(layers$medians$cluster_id,c("B","C"))
  expect_identical(layers$medians$.display_median,c(.5,1.5))
  expect_identical(nrow(layers$points),sum(sizes))
  expect_false(any(!is.finite(layers$points$value)))
})

test_that("final distribution and weekly heatmap grammar is explicit", {
  expect_identical(regionalepi:::.shiny_weekly_heatmap_xgap(0L),1)
  expect_identical(regionalepi:::.shiny_weekly_heatmap_xgap(60L),1)
  expect_identical(regionalepi:::.shiny_weekly_heatmap_xgap(61L),0)
  expect_error(regionalepi:::.shiny_weekly_heatmap_xgap(1.5),"whole number")
  grid <- regionalepi:::.shiny_heatmap_grid(data.frame(
    cluster_id=c("C01","C01","C02"),
    date=as.Date(c("2024-01-01","2024-01-08","2024-01-01")),
    value=c(0,NA_real_,50),stringsAsFactors=FALSE),
    "cluster_id","date","value")
  expect_identical(grid$z[1L,1L],0)
  expect_true(is.na(grid$z[1L,2L]))

  app <- regionalepi:::.shiny_app_dir()
  ui_text <- paste(readLines(file.path(app,"ui.R"),warn=FALSE),collapse="\n")
  server_text <- paste(readLines(file.path(app,"server.R"),warn=FALSE),
    collapse="\n")
  expect_match(ui_text,"Violinplots erst ab mindestens 10 verfügbaren Kreisen",
    fixed=TRUE)
  expect_match(server_text,"Deutschlandreferenz<br>Nationaler Regionaltyp",
    fixed=TRUE)
  expect_match(server_text,"shape=23,size=3.5,fill=\"white\"",fixed=TRUE)
  expect_match(server_text,".shiny_weekly_heatmap_xgap(length(g$columns))",
    fixed=TRUE)
  expect_match(server_text,'c("#2166AC","#F7F7F7","#B2182B")',fixed=TRUE)
  expect_match(server_text,'zmin=-limit,zmax=limit,zmid=0',fixed=TRUE)
  expect_match(server_text,'plot_bgcolor="#D1D5DB"',fixed=TRUE)
  expect_false(grepl("shape=95",server_text,fixed=TRUE))
})

test_that("canonical pair keys respect display order and remain unique for k2 to k5", {
  dates <- as.Date("2020-09-28") + 7*0:3
  for (k in 2:5) {
    ids <- sprintf("C%02d",seq_len(k))
    weekly <- expand.grid(date=dates,cluster_id=ids,KEEP.OUT.ATTRS=FALSE,
      stringsAsFactors=FALSE)
    weekly$median_incidence <- seq_len(nrow(weekly))
    pairs <- regionalepi:::.shiny_pairwise_differences(weekly,ids)
    expect_equal(nrow(pairs),length(dates)*choose(k,2L))
    expect_identical(anyDuplicated(paste(pairs$date,pairs$pair_key)),0L)
    expect_true(all(table(pairs$date)==choose(k,2L)))
    expect_equal(pairs$difference,pairs$median_a-pairs$median_b)
  }

  ids <- c("ClD","ClJ","ClA")
  weekly <- expand.grid(date=dates,cluster_id=rev(ids),KEEP.OUT.ATTRS=FALSE,
    stringsAsFactors=FALSE)
  weekly$median_incidence <- match(weekly$cluster_id,ids)*10+
    match(weekly$date,dates)
  pairs <- regionalepi:::.shiny_pairwise_differences(weekly,ids)
  expect_identical(unique(pairs$pair_label),
    c("ClD \u2212 ClJ","ClD \u2212 ClA","ClJ \u2212 ClA"))
  expect_identical(unique(pairs$pair_key),
    c("ClD\rClJ","ClD\rClA","ClJ\rClA"))
  expect_identical(nrow(pairs),12L)
  expect_identical(anyDuplicated(paste(pairs$date,pairs$pair_key)),0L)
  expect_true(all(pairs$difference==pairs$median_a-pairs$median_b))
  grid <- regionalepi:::.shiny_heatmap_grid(
    pairs,"pair_key","date","difference","pair_label")
  widget <- plotly::plotly_build(plotly::plot_ly(x=grid$columns,
    y=unique(pairs$pair_label),z=grid$z,type="heatmap",zmid=0,
    colors=c("#2166AC","#F7F7F7","#B2182B"),text=grid$text,
    hoverinfo="text",connectgaps=FALSE))
  expect_identical(length(widget$x$data[[1L]]$y),3L)
  expect_identical(dim(widget$x$data[[1L]]$z),c(3L,4L))
})

test_that("advanced pairwise UI and server retain a live Plotly output contract", {
  skip_if_not_installed("plotly")
  app <- regionalepi:::.shiny_app_dir()
  ui_text <- paste(readLines(file.path(app,"ui.R"),warn=FALSE),collapse="\n")
  server_text <- paste(readLines(file.path(app,"server.R"),warn=FALSE),collapse="\n")
  expect_match(ui_text,'tabsetPanel(id="weekly_comparison_view"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Relative Aktivität"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Paarweise Clusterunterschiede"',fixed=TRUE)
  expect_false(grepl('tags$summary("Erweiterte paarweise Vergleiche")',
    ui_text,fixed=TRUE))
  expect_match(ui_text,'plotly::plotlyOutput("pairwise_heatmap"',fixed=TRUE)
  expect_match(ui_text,"A − B zeigt die Differenz",fixed=TRUE)
  expect_match(server_text,"output$pairwise_heatmap <- plotly::renderPlotly",fixed=TRUE)
  expect_match(server_text,"length(metadata$display_cluster_id)>=2L",fixed=TRUE)
  expect_false(grepl('outputOptions(output,"pairwise_heatmap"',server_text,
    fixed=TRUE))

  dates <- as.Date("2020-09-28") + 7L * 0:21
  for (k in 2:5) {
    ids <- if (k == 3L) c("ClD","ClJ","ClA") else sprintf("C%02d",seq_len(k))
    weekly <- expand.grid(date=dates,cluster_id=ids,KEEP.OUT.ATTRS=FALSE,
      stringsAsFactors=FALSE)
    weekly$median_incidence <- seq_len(nrow(weekly))/10
    pairwise <- regionalepi:::.shiny_pairwise_differences(weekly,ids)
    metadata <- data.frame(display_cluster_id=ids,stringsAsFactors=FALSE)
    widget <- regionalepi:::.shiny_pairwise_heatmap_widget(pairwise,metadata)
    built <- plotly::plotly_build(widget)
    expect_s3_class(widget,"plotly")
    expect_identical(nrow(pairwise),as.integer(22L*choose(k,2L)))
    expect_identical(anyDuplicated(paste(pairwise$date,pairwise$pair_key)),0L)
    expect_identical(dim(built$x$data[[1L]]$z),c(as.integer(choose(k,2L)),22L))
    expect_identical(built$x$data[[1L]]$xgap,1)
    expect_equal(pairwise$difference,pairwise$median_a-pairwise$median_b)
  }
  long <- expand.grid(date=as.Date("2020-01-06")+7L*0:60,
    cluster_id=c("C01","C02"),stringsAsFactors=FALSE)
  long$median_incidence<-seq_len(nrow(long))
  long_widget<-plotly::plotly_build(regionalepi:::.shiny_pairwise_heatmap_widget(
    regionalepi:::.shiny_pairwise_differences(long,c("C01","C02")),
    data.frame(display_cluster_id=c("C01","C02"))))
  expect_identical(long_widget$x$data[[1L]]$xgap,0)
  period <- data.frame(start_date = min(dates), end_date = max(dates))
  bounded <- plotly::plotly_build(regionalepi:::.shiny_pairwise_heatmap_widget(
    pairwise, metadata, period))
  expect_identical(length(bounded$x$layout$shapes), 2L)
  expect_identical(vapply(bounded$x$layout$shapes, `[[`, character(1L), "x0"),
    format(c(period$start_date, period$end_date)))
})

test_that("final weekly widget hides every IQR legend trace", {
  skip_if_not_installed("plotly");skip_if_not_installed("ggplot2")
  x<-data.frame(date=rep(as.Date("2025-01-06")+0:2,3),
    cluster=rep(c("C01","C02","C03"),each=3),median=1:9,q1=0:8,q3=2:10)
  graph<-ggplot2::ggplot(x,ggplot2::aes(date,median,colour=cluster,fill=cluster,
    group=cluster))+ggplot2::geom_ribbon(ggplot2::aes(ymin=q1,ymax=q3),alpha=.15,
      colour=NA,show.legend=FALSE)+ggplot2::geom_line()+ggplot2::geom_line(
        data=x[x$cluster=="C01",],ggplot2::aes(date,median,group=1),
        inherit.aes=FALSE,colour="#111111")
  widget<-regionalepi:::.shiny_finalize_incidence_legend(plotly::ggplotly(graph))
  ribbons<-vapply(widget$x$data,function(trace)identical(trace$fill,"toself"),logical(1L))
  visible<-vapply(widget$x$data,function(trace)isTRUE(trace$showlegend),logical(1L))
  expect_true(all(!visible[ribbons]))
  expect_setequal(vapply(widget$x$data[visible],`[[`,character(1L),"name"),
    c("C01","C02","C03","Ausgewählter Kreis"))
})

test_that("pathogen selection reconciliation never exposes a stale window", {
  covid <- regionalepi:::.shiny_reconcile_selection(
    "COVID-19","influenza_2025_26","reviewed")
  expect_identical(covid$window_id,"covid19_2025_26")
  expect_identical(covid$window$pathogen,"COVID-19")
  expect_identical(covid$range_mode,"window")
  expect_identical(nrow(covid$periods),0L)
  influenza <- regionalepi:::.shiny_reconcile_selection(
    "Influenza, saisonal",covid$window_id,"window")
  expect_identical(influenza$window$pathogen,"Influenza, saisonal")
  expect_true(influenza$window_id %in% unname(influenza$choices))
})

test_that("navigation is display-only and absent from analytical cache keys", {
  keys <- c(
    regionalepi:::.shiny_demographic_cache_key("2022–2024"),
    regionalepi:::.shiny_surveillance_cache_key("COVID-19", 2024),
    regionalepi:::.shiny_summary_cache_key("source", "period", "fit")
  )
  expect_false(any(grepl("overview|typology|epidemiology|methods", keys)))
  ui_text <- paste(readLines(file.path(regionalepi:::.shiny_app_dir(), "ui.R"), warn = FALSE), collapse = "\n")
  expect_match(ui_text, 'id="analysis_section"', fixed = TRUE)
})

test_that("map join is identifier-only and handles reviewed historical difference", {
  map <- regionalepi_map_geometry()
  assignments <- data.frame(
    fit_id = "fit", geo_id = map$features$geo_id, raw_cluster = 1L,
    display_cluster_id = "C01", stringsAsFactors = FALSE
  )
  fit <- list(
    assignments = assignments,
    provenance = list(fit_id = "fit"),
    indicator_set = demographic_structure_spec()
  )
  joined <- regionalepi:::.shiny_map_assignments(map, fit)
  expect_identical(nrow(joined$data), 400L)
  expect_identical(joined$data$geo_id, map$features$geo_id)
  expect_length(joined$typology_only_geo_ids, 0L)

  fit$assignments <- rbind(assignments, data.frame(
    fit_id = "fit", geo_id = "16056", raw_cluster = 1L,
    display_cluster_id = "C01", stringsAsFactors = FALSE
  ))
  historical <- regionalepi:::.shiny_map_assignments(map, fit)
  expect_identical(historical$typology_only_geo_ids, "16056")
  current <- regionalepi:::.shiny_current_display_assignments(historical)
  expect_identical(names(current), c("geo_id", "cluster_id"))
  expect_identical(nrow(current), 400L)
  expect_identical(current$geo_id, map$features$geo_id)
  compatibility <- regionalepi:::.shiny_dynamic_compatibility(
    fit, map$features$geo_id
  )
  expect_identical(compatibility$expected_typology_only_geo_ids, "16056")
  expect_identical(compatibility$expected_surveillance_count, 400L)
})

test_that("launcher finds the packaged app and dependency failures are clear", {
  expect_identical(regionalepi:::.shiny_missing_dependencies(), character())
  fake <- function(package, quietly = FALSE) package != "leaflet"
  expect_identical(regionalepi:::.shiny_missing_dependencies(fake), "leaflet")

  directory <- regionalepi:::.shiny_app_dir()
  expect_true(nzchar(directory))
  expect_true(file.exists(file.path(directory, "app.R")))
  ui_environment <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(directory, "ui.R"), envir = ui_environment)
  expect_identical(tail(unname(ui_environment$initial_window_choices), 1L),
                   "influenza_2025_26")
  expect_match(paste(deparse(body(run_regionalepi_app)), collapse = "\n"),
               "optional package")
})

test_that("server error callback resolves its formatter from the package namespace", {
  skip_if_not_installed("shiny")
  app <- regionalepi:::.shiny_app_dir()
  server_environment <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(app, "server.R"), envir = server_environment)

  testthat::local_mocked_bindings(
    .shiny_regional_credentials_available = function(...) TRUE,
    .shiny_fetch_demography = function(years) {
      stop("synthetic Regionaldatenbank failure", call. = FALSE)
    },
    .package = "regionalepi"
  )
  shiny::testServer(server_environment$server, {
    session$setInputs(
      pathogen = "Influenza, saisonal",
      window_id = "influenza_2023_24", range_mode = "window",
      custom_range = as.Date(c("2023-10-02", "2024-05-19")),
      demographic_period = "2022–2024", demographic_source = "live",
      typology_mode = "dynamic",
      k = "3", load_analysis = 1
    )
    session$flushReact()
    status <- paste(as.character(output$load_status), collapse = "")
    expect_match(status, "demographischen Daten konnten nicht geladen werden",
                 fixed = TRUE)
    expect_false(grepl("synthetic Regionaldatenbank failure", status, fixed = TRUE))
    expect_false(grepl("app_format_error", status, fixed = TRUE))
    expect_false(state$busy)
    expect_match(state$technical_error, "synthetic Regionaldatenbank failure",
                 fixed = TRUE)
  })
})

test_that("Shiny user errors hide authentication and source internals", {
  missing <- simpleError(paste(
    "Regionaldatenbank retrieval failed: Invalid Regionaldatenbank",
    "authentication: required environment variables are missing or empty"
  ))
  text <- regionalepi:::.shiny_app_format_error(missing, "Die Live-Analyse")
  expect_match(text, "fehlen die Zugangsdaten", fixed = TRUE)
  expect_false(grepl("environment|authentication", text, ignore.case = TRUE))

  survstat <- regionalepi:::.shiny_app_format_error(
    simpleError("SurvStat@RKI retrieval failed: <SOAP secret>"),
    "Die Live-Analyse"
  )
  expect_match(survstat, "SurvStat-Daten konnten nicht geladen werden",
               fixed = TRUE)
  expect_false(grepl("SOAP secret", survstat, fixed = TRUE))
})

test_that("installed app files do not rely on unqualified app helpers", {
  app <- regionalepi:::.shiny_app_dir()
  files <- file.path(app, c("app.R", "server.R", "ui.R"))
  expect_true(all(file.exists(files)))
  text <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
  unqualified <- gregexpr("(?<![:.])\\bapp_[A-Za-z0-9_]+", text, perl = TRUE)
  matches <- regmatches(text, unqualified)[[1L]]
  expect_identical(matches, character())
  expect_false(file.exists(file.path(app, "helpers.R")))
})

test_that("Shiny provenance formats POSIXct source statuses safely", {
  status <- as.POSIXct("2026-08-28 08:20:20", tz = "UTC")
  source <- list(population = list(
    diagnostics = list(data_status = status), provenance = list()
  ))
  queries <- list(
    kreis = list(data_status = status), berlin = list(data_status = status)
  )
  expect_identical(
    regionalepi:::.shiny_app_source_status(source),
    "2026-08-28 08:20:20"
  )
  expect_identical(
    unname(regionalepi:::.shiny_app_query_status(queries)),
    rep("2026-08-28 08:20:20", 2L)
  )
})

test_that("map and fitting provenance required by the PoC are complete", {
  map <- regionalepi_map_geometry()
  expect_true(all(c(
    "source_organization", "source_product", "source_vintage", "license",
    "license_url", "attribution", "change_notice"
  ) %in% names(map$provenance)))
  expect_true(nzchar(map$provenance$attribution))

  indicator_set <- demographic_structure_spec()
  fitting <- dynamic_kmeans_spec()
  expect_identical(indicator_set$indicator_set_id, "demographic_structure_v1")
  expect_identical(fitting$fitting_specification_id, "dynamic_kmeans_v1")
  expect_identical(fitting$seed, 20241231L)

  bounds <- regionalepi:::.shiny_app_map_bounds(map)
  expect_named(bounds, c("lng1", "lat1", "lng2", "lat2"))
  expect_true(all(is.finite(bounds)))
  expect_lt(bounds[["lng1"]], bounds[["lng2"]])
  expect_lt(bounds[["lat1"]], bounds[["lat2"]])

  assignments <- data.frame(
    geo_id = map$features$geo_id,
    display_cluster_id = rep("C01", nrow(map$features)),
    stringsAsFactors = FALSE
  )
  widget <- regionalepi:::.shiny_app_leaflet_map(map, assignments)
  calls <- vapply(widget$x$calls, `[[`, character(1L), "method")
  expect_false("addProviderTiles" %in% calls)
  expect_identical(sum(calls == "addGeoJSON"), 2L)
  expect_length(widget$x$fitBounds, 5L)
  expect_equal(unlist(widget$x$fitBounds[1:4]),
    unname(bounds[c("lat1", "lng1", "lat2", "lng2")]))
  hook <- widget$jsHooks$render[[1L]]$code
  expect_match(hook, "color:'#F4F4F0', weight:0.45", fixed = TRUE)
  expect_match(hook, "fillOpacity:0.88", fixed = TRUE)
  expect_match(hook, "weight: p.geo_id === id ? 3.25 : 0.45", fixed = TRUE)
  expect_match(hook, "fillOpacity: p.geo_id === id ? 0.94 : 0.88", fixed = TRUE)
  expect_match(hook, "layer.bindTooltip", fixed = TRUE)
  expect_match(hook, "p.state_name", fixed = TRUE)
  exterior <- jsonlite::fromJSON(
    regionalepi:::.shiny_germany_outline_geojson(), simplifyVector = FALSE)
  expect_identical(length(exterior$features), 1L)
  expect_identical(exterior$features[[1L]]$geometry$type, "MultiPolygon")
  expect_identical(exterior$features[[1L]]$properties$role,
    "Germany exterior outline")
})

test_that("district heatmap grouping derives boundaries for every k", {
  for(k in 2:5) {
    x <- data.frame(geo_id=sprintf("%05d",1:(k*2)),geo_name=LETTERS[1:(k*2)],
      cluster_id=rep(sprintf("C%02d",1:k),each=2),stringsAsFactors=FALSE)
    layout <- regionalepi:::.shiny_district_heatmap_layout(x,"00003")
    expect_identical(length(layout$boundaries),k-1L)
    expect_identical(layout$selected_position,3L)
    expect_identical(layout$rows$cluster_id,
      rep(sprintf("C%02d",1:k),each=2))
    source<-x[rep(seq_len(nrow(x)),each=2L),,drop=FALSE]
    source$date<-rep(as.Date("2025-01-06")+c(0,7),times=nrow(x))
    source$incidence<-seq_len(nrow(source));source$incidence[[2L]]<-NA_real_
    source$row_position<-layout$rows$row_position[
      match(source$geo_id,layout$rows$geo_id)]
    grid<-regionalepi:::.shiny_heatmap_grid(source,"row_position","date","incidence")
    expect_identical(as.integer(grid$rows),layout$rows$row_position)
    expect_true(anyNA(grid$z))
  }
  expect_identical(regionalepi:::.shiny_format_number(9453),"9.453")
  expect_identical(regionalepi:::.shiny_format_number(16.28,2L),"16,28")
})

test_that("final district-week traces preserve every synthetic cell coordinate", {
  skip_if_not_installed("plotly")
  ids <- sprintf("%05d", 1:5)
  assignments <- data.frame(geo_id = ids, raw_cluster = c(1L,1L,2L,2L,2L),
    display_cluster_id = c("C01","C01","C02","C02","C02"))
  fit <- list(mode = "dynamic", provenance = list(fit_id = "synthetic"),
    assignments = assignments,
    profiles = data.frame(display_cluster_id=rep(c("C01","C02"),each=3),
      indicator_id=rep(c("population_density","mean_age",
        "youth_dependency_ratio"),2),standardized_center=c(-1,1,0,1,-1,0)))
  metadata <- regionalepi:::.shiny_cluster_display_metadata(fit,"dynamic")
  dates <- as.Date("2025-01-06") + 7*0:2
  data <- expand.grid(geo_id=ids,date=dates,KEEP.OUT.ATTRS=FALSE,
    stringsAsFactors=FALSE)
  data$geo_name <- paste0("District ", data$geo_id)
  data$cluster_id <- assignments$display_cluster_id[
    match(data$geo_id, assignments$geo_id)]
  data$incidence <- seq_len(nrow(data)); data$incidence[[6L]] <- 0
  data$incidence[c(3L,12L)] <- NA_real_
  data$cases <- 100 + seq_len(nrow(data)); data$cases[[12L]] <- NA_real_
  data$reporting_year <- 2025L
  data$reporting_week <- rep(1:3, each=length(ids))
  audit <- regionalepi:::.shiny_district_heatmap_data(data,metadata,"00003")
  widget <- plotly::plotly_build(
    regionalepi:::.shiny_district_heatmap_widget(audit))
  traces <- widget$x$data
  expect_identical(vapply(traces, `[[`, character(1L), "type"),
    c("scatter","heatmap","heatmap"))
  expect_identical(dim(traces[[2L]]$z), c(5L,3L))
  expect_identical(traces[[2L]]$xgap, 1)
  expect_identical(traces[[2L]]$ygap, 0)
  expect_equal(traces[[2L]]$z, audit$incidence, ignore_attr = TRUE)
  expect_equal(traces[[2L]]$customdata, audit$customdata, ignore_attr = TRUE)
  expect_identical(is.na(traces[[3L]]$z), !audit$missing)
  expect_equal(unname(traces[[1L]]$marker$color),
    audit$districts$display_colour, ignore_attr = TRUE)
  for (i in seq_len(nrow(data))) {
    row <- match(data$geo_id[[i]], audit$districts$geo_id)
    column <- match(data$date[[i]], audit$weeks$date)
    expect_equal(traces[[2L]]$z[row,column],data$incidence[[i]])
    expect_equal(audit$cases[row,column],data$cases[[i]])
    expect_identical(audit$missing[row,column],is.na(data$incidence[[i]]))
    expect_identical(traces[[2L]]$customdata[row,column],
      paste(data$geo_id[[i]],data$date[[i]],sep="|"))
  }
  expect_identical(audit$selected_position,3L)
  expect_identical(audit$boundaries,2.5)
  expect_true(all(audit$blocks$last_row-audit$blocks$first_row+1L==audit$blocks$n))
})

test_that("actual server path switches pathogens without an invalid window", {
  skip_if_not_installed("shiny")
  app <- regionalepi:::.shiny_app_dir()
  env <- new.env(parent=asNamespace("shiny"))
  sys.source(file.path(app,"server.R"),envir=env)
  shiny::testServer(env$server, {
    session$setInputs(pathogen="Influenza, saisonal",
      window_id="influenza_2025_26",range_mode="reviewed")
    session$flushReact()
    expect_identical(selected_window()$pathogen,"Influenza, saisonal")
    session$setInputs(pathogen="COVID-19")
    session$flushReact()
    expect_identical(selected_window()$pathogen,"COVID-19")
    expect_identical(selected_window()$observation_window_id,"covid19_2025_26")
    expect_identical(nrow(reviewed_periods()),0L)
    expect_no_error(analysis_range())
    expect_identical(analysis_range()$mode,"window")
    session$setInputs(window_id="covid19_2024_25")
    session$flushReact()
    expect_identical(selected_window()$observation_window_id,"covid19_2024_25")
    expect_gt(nrow(reviewed_periods()),0L)
    session$setInputs(pathogen="Norovirus-Gastroenteritis",range_mode="reviewed")
    session$flushReact()
    expect_identical(selected_window()$pathogen,"Norovirus-Gastroenteritis")
    expect_identical(selected_window()$observation_window_id,"norovirus_2025_26")
    expect_identical(nrow(reviewed_periods()),0L)
    expect_identical(analysis_range()$mode,"window")
    session$setInputs(pathogen="Influenza, saisonal")
    session$flushReact()
    expect_identical(selected_window()$pathogen,"Influenza, saisonal")
    expect_true(selected_window()$observation_window_id %in%
      unname(regionalepi:::.shiny_window_choices("Influenza, saisonal")))
    expect_null(state$error)
    expect_identical(state$retrieval_count,0L)
  })
})

test_that("Shiny indicator presentation uses German labels and units", {
  display <- regionalepi:::.shiny_app_indicator_display()
  expect_identical(display$indicator_id, c(
    "population_density", "mean_age", "youth_dependency_ratio"
  ))
  expect_identical(display$label, c(
    "Bevölkerungsdichte", "Durchschnittsalter", "Jugendquotient"
  ))
  expect_identical(display$unit, c(
    "Einwohner je km²", "Jahre",
    "Unter-20-Jährige je 100 Personen im Alter 20–64"
  ))
})

test_that("Shiny app exposes staged progress and caches fitted map widgets", {
  app <- regionalepi:::.shiny_app_dir()
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE),
                       collapse = "\n")
  ui_text <- paste(readLines(file.path(app, "ui.R"), warn = FALSE),
                   collapse = "\n")
  expect_match(server_text, ".shiny_fetch_demography", fixed = TRUE)
  expect_match(server_text, "SurvStat-Inzidenzen und gemeldete Fälle", fixed = TRUE)
  expect_match(server_text, ".shiny_fetch_surveillance_bundle", fixed = TRUE)
  expect_match(server_text, "mkey <- paste0(\"map:\"", fixed = TRUE)
  expect_true(grepl("fetch_surveillance_bundle", server_text, fixed = TRUE))
  expect_match(server_text, ".shiny_surveillance_cache_match", fixed = TRUE)
  expect_match(ui_text, "Erweiterte Einstellungen", fixed = TRUE)
  expect_match(ui_text,
    "Regionale Infektionsepidemiologie im demografischen Kontext",
    fixed = TRUE)
  expect_match(server_text, "show.legend=FALSE", fixed = TRUE)
  expect_match(ui_text, "Darstellung, nicht den gewählten Analysezeitraum",
    fixed = TRUE)
  expect_match(ui_text, "Demografische Typologie", fixed = TRUE)
  expect_match(ui_text, "Standardisierte demografische Clusterprofile",
    fixed = TRUE)
  expect_identical(
    unname(regionalepi:::.shiny_pathogen_choices()[["Norovirus"]]),
    "Norovirus-Gastroenteritis"
  )
  expect_match(ui_text, "Regionale Analyse", fixed = TRUE)
  expect_match(ui_text, "Räumliche Vergleichsebene", fixed = TRUE)
  expect_match(ui_text, '"Großregionen"="grossregion"', fixed = TRUE)
  expect_match(ui_text, '"grossregion"', fixed = TRUE)
  expect_false(grepl("Regionaltyp im regionalen Vergleich",ui_text,fixed=TRUE))
  expect_match(server_text,
    "Die Prozentwerte beziehen sich auf Kreise, nicht auf Bevölkerungsanteile.",
    fixed = TRUE)
  expect_match(ui_text, "keine kausalen Aussagen", fixed = TRUE)
  expect_match(server_text, "Infektionsgeschehen nach demografischem Regionaltyp", fixed = TRUE)
  expect_match(server_text, ".summarize_weekly_regional_incidence", fixed = TRUE)
  expect_match(ui_text, "keine amtlichen Inzidenzen", fixed = TRUE)
  expect_match(server_text,
    "Für Norovirus ist kein geprüfter nationaler Wellenzeitraum hinterlegt.",
    fixed = TRUE)
  expect_match(server_text, "summarize_state_cluster_incidence", fixed = TRUE)
})

test_that("Shiny demographic snapshot is default and live mode is explicit", {
  app <- regionalepi:::.shiny_app_dir()
  ui_environment <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(app, "ui.R"), envir = ui_environment)
  html <- as.character(ui_environment$ui)
  expect_match(html, 'id="demographic_source_control"', fixed = TRUE)

  testthat::local_mocked_bindings(
    .shiny_fetch_demography = function(...) stop("live retrieval used"),
    .package = "regionalepi"
  )
  current <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2024)
  historical <- regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)
  expect_identical(current$source_mode, "snapshot")
  expect_identical(historical$source_mode, "snapshot")
  expect_match(regionalepi:::.shiny_demography_status(current),
               "Geprüfter Snapshot", fixed = TRUE)
  expect_identical(nrow(current$summary$data), 1200L)
  expect_identical(nrow(historical$summary$data), 1203L)
})

test_that("Regionaldatenbank availability is non-secret and requires both credentials", {
  available <- function(user, password) {
    values <- c(REGIONALSTATISTIK_USER=user,
      REGIONALSTATISTIK_PASSWORD=password)
    regionalepi:::.shiny_regional_credentials_available(
      function(name, unset="") if(name %in% names(values)) values[[name]] else unset)
  }
  expect_true(available("user","password"))
  expect_false(available("","password"))
  expect_false(available("user",""))
  expect_false(available("",""))
  expect_false(available("   ","password"))
  expect_false(available("user","\t"))
  server_text<-paste(readLines(file.path(regionalepi:::.shiny_app_dir(),
    "server.R"),warn=FALSE),collapse="\n")
  expect_match(server_text,"Live-Abruf Regionaldatenbank",fixed=TRUE)
  expect_match(server_text,"aria-disabled','true'",fixed=TRUE)
  expect_match(server_text,"if(v==='live')s.setValue('snapshot')",fixed=TRUE)
  expect_match(server_text,"Geprüfter Snapshot (empfohlen)",fixed=TRUE)
  expect_match(server_text,"REGIONALSTATISTIK_USER und REGIONALSTATISTIK_PASSWORD",
    fixed=TRUE)
  expect_false(grepl("synthetic-user|synthetic-password",server_text,fixed=TRUE))
})

test_that("reviewed small-n regional display geometry remains unchanged", {
  summary <- regionalepi:::.shiny_fetch_snapshot_demography(2022:2024)$summary
  fit <- regionalepi:::.shiny_fit_typology(summary, "dynamic", 3L)
  assignments <- regionalepi:::.shiny_current_display_assignments(
    regionalepi:::.shiny_map_assignments(regionalepi_map_geometry(), fit))
  crosswalk <- regionalepi_district_state_crosswalk()
  assignments <- merge(assignments, crosswalk[c("geo_id", "state_id")],
    by = "geo_id", sort = FALSE)
  expected <- list(
    `01` = c(C01 = 5L, C02 = 6L, C03 = 4L),
    `16` = c(C01 = 17L, C02 = 3L, C03 = 2L)
  )
  expected_rules <- list(
    `01` = c(C01 = "points_and_boxplot", C02 = "points_and_boxplot",
      C03 = "points_and_median"),
    `16` = c(C01 = "points_boxplot_and_violin", C02 = "points_and_median",
      C03 = "points_and_median")
  )
  for (state_id in names(expected)) {
    counts <- table(factor(assignments$cluster_id[
      assignments$state_id == state_id], levels = names(expected[[state_id]])))
    expect_identical(stats::setNames(as.integer(counts), names(counts)),
      expected[[state_id]])
    rules <- vapply(as.integer(counts), function(n)
      regionalepi:::.shiny_finite_n_display(seq_len(n))$display_rule,
      character(1L))
    expect_identical(stats::setNames(rules, names(counts)),
      expected_rules[[state_id]])
  }
})

test_that("regional relative-activity and legend UI semantics are explicit", {
  app<-regionalepi:::.shiny_app_dir()
  ui_text<-paste(readLines(file.path(app,"ui.R"),warn=FALSE),collapse="\n")
  server_text<-paste(readLines(file.path(app,"server.R"),warn=FALSE),collapse="\n")
  expect_match(ui_text,"Relative Aktivität innerhalb der ausgewählten Region",fixed=TRUE)
  expect_match(ui_text,"minus Median aller beobachteten Kreise",fixed=TRUE)
  expect_match(ui_text,"keinen formalen Wellenbeginn",fixed=TRUE)
  expect_match(ui_text,"RKI-definierter/geprüfter Zeitraum",fixed=TRUE)
  expect_false(grepl("Geprüfter epidemiologischer Zeitraum",ui_text,fixed=TRUE))
  expect_match(server_text,".regional_relative_activity",fixed=TRUE)
  expect_match(server_text,'type="heatmap"',fixed=TRUE)
  expect_match(server_text,"plot_bgcolor=\"#D1D5DB\"",fixed=TRUE)
  expect_false(grepl("normalized_activity",server_text,fixed=TRUE))
  expect_match(server_text,"show.legend=FALSE",fixed=TRUE)
  expect_false(grepl("peak-week|cross-correlation|centre of mass",server_text))
})

test_that("Pass-B nested information architecture and theme are explicit", {
  app <- regionalepi:::.shiny_app_dir()
  ui_text <- paste(readLines(file.path(app,"ui.R"),warn=FALSE),collapse="\n")
  server_text <- paste(readLines(file.path(app,"server.R"),warn=FALSE),
    collapse="\n")
  expect_match(ui_text,'tabsetPanel(id="typology_view"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Clusterprofile"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Stabilität der Typologie"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Verteilungen der Kreise"',fixed=TRUE)
  expect_match(ui_text,"Für die historische Referenztypologie ist die Drei-Cluster-Lösung fest definiert",fixed=TRUE)
  expect_match(ui_text,'tabsetPanel(id="methods_view"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Methodik"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Datenquellen"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Methodik und Daten"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Datenstand und Definitionen"',fixed=TRUE)
  expect_match(ui_text,'tabPanel("Reproduzierbarkeit"',fixed=TRUE)
  expect_match(ui_text,"Relative Aktivität gegenüber allen Kreisen Deutschlands",fixed=TRUE)
  expect_match(ui_text,"Regionengruppen aus Ländern",fixed=TRUE)
  expect_match(ui_text,"#E08625",fixed=TRUE)
  expect_match(ui_text,"#F39C12",fixed=TRUE)
  expect_match(ui_text,"#4183C4",fixed=TRUE)
  expect_match(ui_text,"Lato,'Helvetica Neue',Arial,sans-serif",fixed=TRUE)
  expect_match(server_text,"output$methods_methodology",fixed=TRUE)
  expect_match(server_text,"output$methods_sources",fixed=TRUE)
  expect_match(server_text,"output$methods_status",fixed=TRUE)
  expect_match(server_text,"output$methods_reproducibility",fixed=TRUE)
  expect_match(server_text,"Die methodische Konzeption basiert auf der von ",fixed=TRUE)
  expect_match(server_text,"Reproduktion der von ",fixed=TRUE)
  expect_match(server_text,"verwendeten Drei-Cluster-Typologie. Clusterzahl, Indikatorsatz und Definitionen sind fest vorgegeben.",fixed=TRUE)
  expect_gte(length(gregexpr("https://doi.org/10.17169/refubium-51449",
    server_text,fixed=TRUE)[[1L]]),3L)
  expect_false(grepl("Paper 1",server_text,fixed=TRUE))
  expect_match(server_text,"REGIONALSTATISTIK_USER=...",fixed=TRUE)
  expect_match(server_text,"REGIONALSTATISTIK_PASSWORD=...",fixed=TRUE)
  expect_match(server_text,"Methodik und Daten → Datenquellen → Regionaldatenbank",fixed=TRUE)
  expect_false(grepl("Methodik & Daten",paste(ui_text,server_text),fixed=TRUE))
  expect_match(server_text,"weder demografische Kausalität noch formale Wellenbeginne",fixed=TRUE)
  expect_false(grepl('uiOutput("provenance")',ui_text,fixed=TRUE))
})

test_that("regional relative-activity heatmap grid is complete ordered and value preserving", {
  dates<-as.Date("2024-01-01")+7L*0:2
  x<-expand.grid(date=dates,cluster_id=c("C01","C02"),stringsAsFactors=FALSE)
  x$median_incidence<-c(0,2,4,NA,0,5)
  x$regional_reference_median<-c(1,1,1,2,2,2)
  x$regional_relative_activity<-c(-1,0,3,NA,-2,3)
  x$observed_districts<-c(3L,3L,3L,0L,3L,3L)
  x$expected_districts<-3L;x$missing_districts<-3L-x$observed_districts
  x$completeness<-x$observed_districts/3
  x$regional_observed_districts<-c(3L,3L,3L,2L,2L,2L)
  x$regional_expected_districts<-3L
  x$regional_missing_districts<-3L-x$regional_observed_districts
  x$regional_completeness<-x$regional_observed_districts/3
  metadata<-data.frame(display_cluster_id=c("C02","C01"),
    display_label=c("C02","C01"),profile_description=c("Profil 2","Profil 1"),
    stringsAsFactors=FALSE)
  grid<-regionalepi:::.shiny_regional_temporal_heatmap_data(x,metadata)
  expect_identical(grid$rows,c("C02","C01"))
  expect_identical(grid$columns,dates)
  expect_identical(nrow(grid$data),6L)
  expect_setequal(as.numeric(grid$z),x$regional_relative_activity)
  expect_true(any(is.na(grid$z)))
  expect_true(any(grid$z==0,na.rm=TRUE))
  expect_match(paste(grid$text,collapse=" "),"Cluster-Wochenmedian",fixed=TRUE)
  expect_match(paste(grid$text,collapse=" "),"Region gesamt",fixed=TRUE)
  expect_match(paste(grid$text,collapse=" "),"Keine beobachtbare relative Aktivität",fixed=TRUE)
  expect_error(regionalepi:::.shiny_regional_temporal_heatmap_data(
    rbind(x,x[1L,]),metadata),"complete unique")
  expect_error(regionalepi:::.shiny_regional_temporal_heatmap_data(
    x[-1L,],metadata),"complete unique")
})

test_that("regional relative-activity heatmap retains k2 to k5 and historical order", {
  for(ids in list(sprintf("C%02d",1:2),sprintf("C%02d",1:3),
                  sprintf("C%02d",1:4),sprintf("C%02d",1:5),
                  c("ClD","ClJ","ClA"))) {
    x<-expand.grid(date=as.Date("2024-01-01")+7L*0:1,
      cluster_id=ids,stringsAsFactors=FALSE)
    x$median_incidence<-rep(c(0,2),length(ids))
    x$regional_reference_median<-1
    x$regional_relative_activity<-rep(c(-1,1),length(ids))
    x$observed_districts<-2L;x$expected_districts<-2L
    x$missing_districts<-0L;x$completeness<-1
    x$regional_observed_districts<-2L;x$regional_expected_districts<-2L
    x$regional_missing_districts<-0L;x$regional_completeness<-1
    metadata<-data.frame(display_cluster_id=ids,display_label=ids,
      profile_description=paste("Profil",ids),stringsAsFactors=FALSE)
    grid<-regionalepi:::.shiny_regional_temporal_heatmap_data(x,metadata)
    expect_identical(grid$rows,ids)
    expect_identical(dim(grid$z),c(length(ids),2L))
  }
})

test_that("regional context styles are deterministic and role-distinct", {
  names<-c("Süden","Mitte (West)","Norden (West)","Osten","Deutschland")
  first<-regionalepi:::.shiny_regional_context_styles(names,"Süden")
  second<-regionalepi:::.shiny_regional_context_styles(names,"Süden")
  expect_identical(first,second)
  expect_identical(first$comparison_name,names)
  expect_identical(first$linetype[first$comparison_name=="Süden"],"solid")
  expect_identical(first$linetype[first$comparison_name=="Deutschland"],"longdash")
  comparisons<-first[!first$comparison_name%in%c("Süden","Deutschland"),]
  expect_identical(length(unique(comparisons$colour)),3L)
  expect_identical(length(unique(comparisons$linetype)),3L)
  expect_gt(first$linewidth[first$comparison_name=="Süden"],max(comparisons$linewidth))
  expect_error(regionalepi:::.shiny_regional_context_styles(
    c(names,"Zusatz"),"Süden"),"at most three")
})

test_that("named Plotly legend cleanup removes technical tuple suffixes", {
  skip_if_not_installed("plotly");skip_if_not_installed("ggplot2")
  x<-data.frame(date=rep(as.Date("2024-01-01")+0:1,2),value=1:4,
    region=rep(c("Süden","Deutschland"),each=2),
    role=rep(c("Fokal","Referenz"),each=2))
  graph<-ggplot2::ggplot(x,ggplot2::aes(date,value,colour=region,
    linetype=role,group=region))+ggplot2::geom_line()
  widget<-regionalepi:::.shiny_clean_named_legend(
    plotly::ggplotly(graph),c("Süden","Deutschland"))
  visible<-vapply(widget$x$data,function(trace)!identical(trace$showlegend,FALSE),logical(1))
  names<-vapply(widget$x$data[visible],function(trace)
    if(is.null(trace$name))"" else trace$name,character(1))
  expect_setequal(names,c("Süden","Deutschland"))
  expect_false(any(grepl("[(),]",names)))
})

test_that("absolute regional IQR legend has one clean entry per cluster", {
  skip_if_not_installed("plotly");skip_if_not_installed("ggplot2")
  x<-expand.grid(date=as.Date("2024-01-01")+0:2,
    cluster_id=c("C01","C02","C03"),stringsAsFactors=FALSE)
  x$median<-seq_len(nrow(x));x$q1<-x$median-1;x$q3<-x$median+1
  graph<-ggplot2::ggplot(x,ggplot2::aes(date,median,colour=cluster_id,
    fill=cluster_id,group=cluster_id))+ggplot2::geom_ribbon(
      ggplot2::aes(ymin=q1,ymax=q3),colour=NA,show.legend=FALSE)+
    ggplot2::geom_line()
  widget<-regionalepi:::.shiny_finalize_incidence_legend(plotly::ggplotly(graph))
  visible<-vapply(widget$x$data,function(trace)!identical(trace$showlegend,FALSE),logical(1))
  names<-vapply(widget$x$data[visible],function(trace)
    if(is.null(trace$name))"" else trace$name,character(1))
  expect_identical(sort(unique(names)),c("C01","C02","C03"))
  expect_identical(length(names),3L)
  expect_false(any(grepl("[(),]",names)))
})

test_that("Shiny period orchestration removes explicitly unassigned rows", {
  source("helper-examples.R", local = TRUE)
  surveillance <- surveillance_incidence_example()[rep(1L, 4L), , drop = FALSE]
  surveillance$geo_id <- rep(c("01001", "02000"), each = 2L)
  surveillance$geo_name <- rep(c("Flensburg", "Hamburg"), each = 2L)
  surveillance$geo_level <- "district"
  surveillance$date <- rep(as.Date(c("2024-01-01", "2024-02-05")), 2L)
  surveillance$pathogen <- "Influenza, saisonal"
  surveillance$source <- "SurvStat@RKI"
  surveillance$incidence <- c(1, 2, 3, 4)
  surveillance$reporting_year <- 2024L
  surveillance$reporting_week <- rep(c(1L, 6L), 2L)
  surveillance$query_id <- "synthetic"
  periods <- data.frame(
    period_set_id = "test", period_id = "selected", pathogen = "Influenza, saisonal",
    season_id = "test", period_type = "influenza_wave", label = "Test",
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-01-07"),
    definition_version = "test_v1", variant_context = NA_character_,
    historical_context = "synthetic", evidence_class = "REVIEWED_TEST_PERIOD",
    source_reference = "synthetic",
    review_status = "reviewed", note = NA_character_, stringsAsFactors = FALSE
  )
  fit <- list(
    assignments = data.frame(
      geo_id = c("01001", "02000"), display_cluster_id = c("C01", "C02"),
      stringsAsFactors = FALSE
    ),
    provenance = list(fit_id = "fit"),
    indicator_set = demographic_structure_spec()
  )
  selected <- list(resource = periods, row = periods)
  result <- regionalepi:::.shiny_analyse_period(
    list(data = surveillance), selected, fit
  )
  expect_false(anyNA(result$attached$data$geo_id))
  expect_true(all(result$attached$data$period_id == "selected"))
  expect_identical(nrow(result$attached$data), 2L)
})

test_that("linked display state reuses one source bundle and one typology fit", {
  skip_if_not_installed("shiny")
  app <- regionalepi:::.shiny_app_dir()
  server_environment <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(app, "server.R"), envir = server_environment)
  map <- regionalepi_map_geometry()
  dates <- as.Date(c("2025-10-06", "2025-10-13"))
  grid <- expand.grid(geo_id = map$features$geo_id, date = dates,
                      stringsAsFactors = FALSE)
  grid$geo_name <- map$features$geo_name[match(grid$geo_id, map$features$geo_id)]
  grid$reporting_year <- 2025L
  grid$reporting_week <- as.integer(format(grid$date, "%V"))
  grid$incidence <- seq_len(nrow(grid)) / 100
  grid$cases <- as.numeric(seq_len(nrow(grid)) %% 7L)
  calls <- 0L
  fake_bundle <- function(...) {
    calls <<- calls + 1L
    list(data = grid, diagnostics = list(exact_key_equality = TRUE),
         provenance = list(incidence = list(), counts = list()), timings = numeric())
  }
  testthat::local_mocked_bindings(
    .shiny_fetch_surveillance_bundle = fake_bundle,
    .package = "regionalepi"
  )
  shiny::testServer(server_environment$server, {
    session$setInputs(pathogen="Influenza, saisonal",window_id="influenza_2025_26",
      range_mode="window",custom_range=as.Date(c("2025-09-29","2026-05-17")),
      typology_mode="dynamic",demographic_period="2022–2024",
      k="3",density_scale="original",district_overlay=TRUE,
      regional_level="grossregion",load_analysis=1)
    session$flushReact()
    expect_null(state$error)
    expect_match(state$result$cache_keys$demographic,"snapshot",fixed=TRUE)
    expect_identical(calls, 1L)
    fit_id <- state$result$fit$provenance$fit_id
    expect_no_error(output$methods_methodology)
    expect_no_error(output$methods_sources)
    expect_no_error(output$methods_status)
    expect_no_error(output$methods_reproducibility)
    original_distribution <- output$demographic_distribution_plot
    session$setInputs(density_scale="log10")
    session$flushReact()
    expect_false(identical(output$demographic_distribution_plot,
      original_distribution))
    expect_identical(calls,1L)
    expect_identical(state$result$fit$provenance$fit_id,fit_id)
    state$selected_geo_id <- "01001"
    session$setInputs(range_mode="custom",
      custom_range=as.Date(c("2025-10-06","2025-10-13")),analysis_section="typology")
    session$flushReact()
    expect_identical(calls, 1L)
    expect_identical(state$result$fit$provenance$fit_id, fit_id)
    expect_identical(state$selected_geo_id, "01001")
    expect_identical(nrow(exploration()$data), 800L)
    session$setInputs(regional_focal="south",regional_comparisons=c("east"))
    session$flushReact()
    expect_no_error(output$regional_composition_plot)
    expect_no_error(output$regional_demographic_distribution_plot)
    expect_no_error(output$regional_cluster_time_plot)
    expect_no_error(output$regional_relative_activity_plot)
    expect_no_error(output$regional_context_time_plot)
    expect_no_error(output$regional_district_plot)
    expect_identical(calls, 1L)
    expect_identical(state$result$fit$provenance$fit_id, fit_id)
    relative<-regional_relative_activity()
    expect_equal(relative$regional_relative_activity,
      relative$median_incidence-relative$regional_reference_median)
    expect_identical(relative$observed_districts,
      regional_cluster_weekly()$observed_districts)
    expect_true(all(relative$regional_expected_districts>=
      relative$regional_observed_districts))
    session$setInputs(regional_view="Kreise",regional_density_scale="log10")
    session$flushReact()
    expect_no_error(output$regional_demographic_distribution_plot)
    expect_identical(calls,1L)
    expect_identical(state$result$fit$provenance$fit_id,fit_id)
    session$setInputs(regional_level="state",regional_focal="09")
    session$flushReact()
    expect_no_error(output$regional_composition_plot)
    expect_identical(calls, 1L)
    expect_identical(state$result$fit$provenance$fit_id, fit_id)
    session$setInputs(regional_level="aggregated_state")
    session$flushReact()
    expect_no_error(output$regional_composition_plot)
    expect_no_error(output$regional_context_time_plot)
    session$setInputs(regional_level="grossregion")
    session$flushReact()
    expect_no_error(output$regional_composition_plot)
    expect_no_error(output$regional_context_time_plot)
    expect_identical(calls, 1L)
    expect_identical(state$result$fit$provenance$fit_id, fit_id)

    session$setInputs(typology_mode="dissertation",load_analysis=2)
    session$flushReact()
    historical_fit_id <- state$result$fit$provenance$fit_id
    expect_identical(historical_fit_id,
      "dissertation_v1_historical_reference")
    expect_identical(nrow(state$result$fit$assignments),401L)
    expect_identical(nrow(state$result$map_join$data),400L)
    expect_identical(state$result$map_join$typology_only_geo_ids,"16056")
    expect_no_error(output$regional_composition_plot)
    expect_no_error(output$regional_demographic_distribution_plot)
    expect_no_error(output$regional_cluster_time_plot)
    expect_no_error(output$regional_relative_activity_plot)
    expect_no_error(output$regional_context_time_plot)
    expect_no_error(output$regional_district_plot)
    expect_identical(calls,1L)

    session$setInputs(typology_mode="dynamic",k="3",load_analysis=3)
    session$flushReact()
    expect_identical(state$result$fit$provenance$fit_id,fit_id)
    expect_true(all(grepl("^C0[1-3]$",
      state$result$map_join$data$display_cluster_id)))
    expect_no_error(output$regional_composition_plot)
    expect_identical(calls,1L)

    session$setInputs(typology_mode="dissertation",load_analysis=4)
    session$flushReact()
    expect_identical(state$result$fit$provenance$fit_id,historical_fit_id)
    expect_setequal(unique(state$result$map_join$data$display_cluster_id),
      c("ClD","ClJ","ClA"))
    expect_no_error(output$regional_composition_plot)
    expect_identical(calls,1L)

    session$setInputs(pathogen="COVID-19",window_id="covid19_2020_21",
      range_mode="custom",custom_range=as.Date(c("2020-05-11","2021-05-23")))
    session$flushReact()
    expect_null(state$result)
    expect_identical(calls,1L)
  })
})
