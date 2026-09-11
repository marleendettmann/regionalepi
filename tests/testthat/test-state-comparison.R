test_that("reviewed district-to-state crosswalk is exact", {
  x <- regionalepi_district_state_crosswalk()
  expect_invisible(validate_district_state_crosswalk(x))
  expect_identical(names(x), c(
    "geo_id", "state_id", "state_name", "reference_date", "source",
    "definition_version"
  ))
  expect_identical(nrow(x), 400L)
  expect_identical(length(unique(x$state_id)), 16L)
  expect_identical(sum(x$state_name == "Berlin"), 1L)
  expect_identical(sum(x$state_name == "Hamburg"), 1L)
  expect_identical(sum(x$state_name == "Bremen"), 2L)
  expect_true(all(x$reference_date == as.Date("2024-12-31")))
  expect_error(regionalepi_district_state_crosswalk("latest"), "Unsupported")

  bad <- x; bad$state_id[[1L]] <- "99"
  expect_error(validate_district_state_crosswalk(bad), "inconsistent")
  bad <- x[-1L, ]
  expect_error(validate_district_state_crosswalk(bad), "exactly 400")
})

test_that("reviewed 16-to-12 comparison mapping is exact", {
  groups <- regionalepi_state_comparison_groups()
  expect_invisible(validate_state_comparison_groups(groups))
  expect_identical(nrow(groups), 16L)
  expect_identical(length(unique(groups$comparison_group_id)), 12L)
  expected <- c(`11`="BB_BE", `12`="BB_BE", `04`="NI_HB", `03`="NI_HB",
                `02`="SH_HH", `01`="SH_HH", `10`="RP_SL", `07`="RP_SL")
  expect_identical(
    unname(groups$comparison_group_id[match(names(expected), groups$state_id)]),
    unname(expected)
  )
  unchanged <- setdiff(groups$state_id, names(expected))
  expect_identical(length(unique(groups$comparison_group_id[
    groups$state_id %in% unchanged])), 8L)
  membership <- regionalepi:::.regional_comparison_membership(
    regionalepi_district_state_crosswalk(), "aggregated_states", groups)
  expect_identical(nrow(membership), 400L)
  expect_identical(anyDuplicated(membership$geo_id), 0L)
  expect_false(anyNA(membership$comparison_id))
  expect_identical(unique(membership$comparison_name[
    membership$state_name == "Berlin"]), "Brandenburg/Berlin")
  expect_identical(unique(membership$comparison_name[
    membership$state_name == "Bremen"]), "Niedersachsen/Bremen")
  expect_identical(unique(membership$comparison_name[
    membership$state_name == "Hamburg"]), "Schleswig-Holstein/Hamburg")
  expect_identical(unique(membership$comparison_name[
    membership$state_name == "Saarland"]), "Rheinland-Pfalz/Saarland")
})

test_that("state composition uses district fractions and explicit absences", {
  crosswalk <- regionalepi_district_state_crosswalk()
  assignments <- data.frame(
    geo_id = crosswalk$geo_id,
    cluster_id = ifelse(seq_len(nrow(crosswalk)) %% 3L == 0L, "C03",
      ifelse(seq_len(nrow(crosswalk)) %% 2L == 0L, "C02", "C01")),
    stringsAsFactors = FALSE
  )
  # Make the one-district city states explicit single-cluster compositions.
  assignments$cluster_id[crosswalk$state_id %in% c("02", "11")] <- "C03"
  result <- summarize_cluster_composition_by_state(assignments, crosswalk)
  expect_identical(nrow(result), 48L)
  expect_equal(as.numeric(tapply(
    result$district_fraction, result$state_id, sum
  )), rep(1, 16L))
  expect_identical(sum(result$n_districts), 400L)
  expect_true(any(result$n_districts == 0L))
  berlin <- result[result$state_name == "Berlin", ]
  hamburg <- result[result$state_name == "Hamburg", ]
  bremen <- result[result$state_name == "Bremen", ]
  expect_identical(unique(berlin$state_total_districts), 1L)
  expect_identical(unique(hamburg$state_total_districts), 1L)
  expect_identical(unique(bremen$state_total_districts), 2L)
  expect_identical(sum(berlin$n_districts), 1L)

  expect_error(
    summarize_cluster_composition_by_state(assignments[-1L, ], crosswalk),
    "exactly the reviewed 400"
  )
})

test_that("state incidence comparison retains district-level estimand", {
  crosswalk <- regionalepi_district_state_crosswalk()
  ids <- crosswalk$geo_id[1:3]
  dates <- as.Date(c("2024-01-01", "2024-01-08", "2024-01-15"))
  data <- expand.grid(date = dates, geo_id = ids, stringsAsFactors = FALSE)
  data$geo_name <- paste("District", data$geo_id)
  data$cluster_id <- rep(c("C01", "C01", "C02"), each = length(dates))
  data$incidence <- c(1, 3, 5, 2, NA, 6, 4, 8, 12)
  data$cases <- c(1, 2, 3, 4, NA, 6, NA, NA, NA)
  result <- summarize_state_cluster_incidence(data, crosswalk)
  expect_identical(nrow(result), 3L)
  first <- result[result$geo_id == ids[[1L]], ]
  expect_identical(first$period_median_incidence, 3)
  expect_identical(first$cumulative_reported_cases, 6)
  second <- result[result$geo_id == ids[[2L]], ]
  expect_identical(second$observed_weeks, 2L)
  expect_identical(second$missing_weeks, 1L)
  third <- result[result$geo_id == ids[[3L]], ]
  expect_true(is.na(third$cumulative_reported_cases))
  expect_false(any(grepl("state_incidence|weighted", names(result))))

  duplicate <- rbind(data, data[1L, ])
  expect_error(summarize_state_cluster_incidence(duplicate, crosswalk),
               "unique")
})

test_that("state comparison display rules are deterministic", {
  crosswalk <- regionalepi_district_state_crosswalk()
  state_ids <- c(rep("01", 5L), rep("03", 4L), rep("04", 2L), "11")
  ids <- unlist(lapply(split(state_ids, state_ids), function(id) {
    head(crosswalk$geo_id[crosswalk$state_id == id[[1L]]], length(id))
  }), use.names = FALSE)
  rows <- crosswalk[match(ids, crosswalk$geo_id), ]
  summary <- data.frame(
    geo_id = rows$geo_id, geo_name = paste("District", rows$geo_id),
    state_id = rows$state_id, state_name = rows$state_name,
    cluster_id = "C01", period_median_incidence = seq_along(ids),
    observed_weeks = 3L, expected_weeks = 4L, missing_weeks = 1L,
    cumulative_reported_cases = seq_along(ids), stringsAsFactors = FALSE
  )
  display <- regionalepi:::.shiny_state_comparison_display(
    summary, "C01", crosswalk
  )
  rule <- stats::setNames(display$groups$display_rule,
                          display$groups$state_id)
  expect_identical(rule[["01"]], "points_and_boxplot")
  expect_identical(rule[["03"]], "points_and_median")
  expect_identical(rule[["04"]], "points_and_median")
  expect_identical(rule[["11"]], "point_only")
  expect_identical(rule[["02"]], "unavailable")
})

test_that("state composition supports every current typology k and reference", {
  crosswalk <- regionalepi_district_state_crosswalk()
  map_ids <- regionalepi_map_geometry()$features$geo_id
  for (years in list(2017:2020, 2022:2024)) {
    summary <- regionalepi:::.shiny_fetch_snapshot_demography(years)$summary
    for (k in 2:5) {
      fit <- fit_dynamic_typology(summary, k = k)
      assignments <- fit$assignments[fit$assignments$geo_id %in% map_ids,
        c("geo_id", "display_cluster_id")]
      names(assignments)[[2L]] <- "cluster_id"
      composition <- summarize_cluster_composition_by_state(assignments, crosswalk)
      expect_equal(as.numeric(tapply(
        composition$district_fraction, composition$state_id, sum
      )), rep(1, 16L))
    }
  }
  historical <- regionalepi:::.shiny_fit_typology(
    regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary,
    "dissertation", 3L
  )
  assignments <- historical$assignments[
    historical$assignments$geo_id %in% map_ids,
    c("geo_id", "display_cluster_id")
  ]
  names(assignments)[[2L]] <- "cluster_id"
  expect_identical(nrow(
    summarize_cluster_composition_by_state(assignments, crosswalk)
  ), 48L)
})

test_that("historical regional composition uses the reviewed current display universe", {
  crosswalk <- regionalepi_district_state_crosswalk()
  historical <- regionalepi:::.shiny_fit_typology(
    regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)$summary,
    "dissertation", 3L
  )
  raw <- historical$assignments[c("geo_id", "display_cluster_id")]
  names(raw)[[2L]] <- "cluster_id"
  expect_identical(nrow(raw), 401L)
  expect_error(summarize_cluster_composition_by_state(raw, crosswalk),
               "exactly the reviewed 400 districts")

  map_join <- regionalepi:::.shiny_map_assignments(
    regionalepi_map_geometry(), historical
  )
  current <- regionalepi:::.shiny_current_display_assignments(map_join)
  expect_identical(nrow(current), 400L)
  expect_identical(anyDuplicated(current$geo_id), 0L)
  expect_setequal(current$geo_id, crosswalk$geo_id)
  expect_false("16056" %in% current$geo_id)
  expect_true("16056" %in% raw$geo_id)
  expect_identical(as.integer(table(factor(raw$cluster_id,
    levels = c("ClD", "ClJ", "ClA")))), c(63L, 213L, 125L))
  expect_identical(raw$cluster_id[raw$geo_id == "16056"], "ClA")
  expect_identical(current$cluster_id[current$geo_id == "16063"], "ClA")
  expect_identical(current$cluster_id[current$geo_id == "11000"], "ClD")
  expect_identical(as.integer(table(factor(current$cluster_id,
    levels = c("ClA", "ClD", "ClJ")))), c(124L, 63L, 213L))

  gross <- summarize_cluster_composition_by_region(
    current, "grossregion", crosswalk
  )
  expected_gross <- data.frame(
    comparison_id = rep(c("middle_west", "north", "east", "south"), each = 3L),
    cluster_id = rep(c("ClA", "ClD", "ClJ"), 4L),
    n_districts = c(26L, 28L, 67L, 16L, 7L, 40L,
                    60L, 7L, 9L, 22L, 21L, 97L),
    stringsAsFactors = FALSE
  )
  observed_gross <- gross[c("comparison_id", "cluster_id", "n_districts")]
  observed_gross <- observed_gross[order(observed_gross$comparison_id,
    observed_gross$cluster_id), ]
  expected_gross <- expected_gross[order(expected_gross$comparison_id,
    expected_gross$cluster_id), ]
  rownames(observed_gross) <- rownames(expected_gross) <- NULL
  expect_identical(observed_gross, expected_gross)

  aggregated <- summarize_cluster_composition_by_region(
    current, "aggregated_state", crosswalk
  )
  states <- summarize_cluster_composition_by_region(current, "state", crosswalk)
  expect_identical(sum(gross$n_districts), 400L)
  expect_identical(sum(aggregated$n_districts), 400L)
  expect_identical(sum(states$n_districts), 400L)
  expect_identical(length(unique(gross$comparison_id)), 4L)
  expect_identical(length(unique(aggregated$comparison_id)), 12L)
  expect_identical(length(unique(states$comparison_id)), 16L)
  expect_identical(aggregated$n_districts[
    aggregated$comparison_id == "BB_BE" & aggregated$cluster_id == "ClA"], 15L)
  expect_identical(aggregated$n_districts[
    aggregated$comparison_id == "BB_BE" & aggregated$cluster_id == "ClD"], 1L)
  expect_identical(aggregated$n_districts[
    aggregated$comparison_id == "BB_BE" & aggregated$cluster_id == "ClJ"], 3L)
  expect_identical(states$n_districts[
    states$comparison_id == "16" & states$cluster_id == "ClA"], 17L)
  expect_identical(states$n_districts[
    states$comparison_id == "16" & states$cluster_id == "ClD"], 1L)
  expect_identical(states$n_districts[
    states$comparison_id == "16" & states$cluster_id == "ClJ"], 4L)
})

test_that("regional composition supports both reviewed comparison levels", {
  crosswalk <- regionalepi_district_state_crosswalk()
  fit <- regionalepi:::.shiny_fit_typology(
    regionalepi:::.shiny_fetch_snapshot_demography(2022:2024)$summary,
    "dynamic", 5L)
  assignments <- fit$assignments[c("geo_id", "display_cluster_id")]
  names(assignments)[[2L]] <- "cluster_id"
  states <- summarize_cluster_composition_by_region(
    assignments, "states", crosswalk)
  aggregated <- summarize_cluster_composition_by_region(
    assignments, "aggregated_states", crosswalk)
  expect_identical(nrow(states), 80L)
  expect_identical(nrow(aggregated), 60L)
  expect_identical(sum(states$n_districts), 400L)
  expect_identical(sum(aggregated$n_districts), 400L)
  expect_equal(as.numeric(tapply(states$district_fraction,
    states$comparison_id, sum)), rep(1, 16L))
  expect_equal(as.numeric(tapply(aggregated$district_fraction,
    aggregated$comparison_id, sum)), rep(1, 12L))
})

test_that("generic comparison contract covers reviewed 4/12/16 levels", {
  groups <- regionalepi:::.regional_comparison_groups()
  expect_invisible(regionalepi:::.validate_regional_comparison_groups(groups))
  expect_identical(as.integer(table(groups$comparison_level)),c(16L,16L,16L))
  expect_identical(vapply(split(groups$comparison_group_id,
    groups$comparison_level),function(x)length(unique(x)),integer(1)),
    c(aggregated_state=12L,grossregion=4L,state=16L))
  expected <- c(`01`="north",`02`="north",`03`="north",`04`="north",
    `05`="middle_west",`06`="middle_west",`07`="middle_west",
    `08`="south",`09`="south",`10`="middle_west",`11`="east",
    `12`="east",`13`="east",`14`="east",`15`="east",`16`="east")
  gross <- groups[groups$comparison_level=="grossregion",]
  expect_identical(stats::setNames(gross$comparison_group_id,gross$state_id),expected)
  for(level in c("grossregion","aggregated_state","state")) {
    membership <- regionalepi:::.regional_comparison_membership(
      regionalepi_district_state_crosswalk(),level,groups)
    expect_identical(nrow(membership),400L)
    expect_identical(anyDuplicated(membership$geo_id),0L)
    expect_false(anyNA(membership$comparison_id))
  }
  bad<-groups[-1L,]
  expect_error(regionalepi:::.validate_regional_comparison_groups(bad),
    "map all 16")
})

test_that("regional composition supports Großregionen with explicit zero cells", {
  crosswalk<-regionalepi_district_state_crosswalk()
  assignments<-data.frame(geo_id=crosswalk$geo_id,
    cluster_id=ifelse(seq_len(nrow(crosswalk))%%2L,"C01","C02"))
  result<-summarize_cluster_composition_by_region(assignments,"grossregion",crosswalk)
  expect_identical(nrow(result),8L)
  expect_identical(sum(result$n_districts),400L)
  expect_equal(as.numeric(tapply(result$district_fraction,result$comparison_id,sum)),rep(1,4L))
})

test_that("regional weekly summaries retain NA zero and completeness semantics", {
  dates<-as.Date(c("2024-01-01","2024-01-08"));ids<-c("01001","01002","01003")
  data<-expand.grid(geo_id=ids,date=dates,stringsAsFactors=FALSE)
  data$cluster_id<-c(C01="C01",C02="C01",C03="C02")[match(data$geo_id,ids)]
  data$incidence<-c(0,NA,8,2,4,NA)
  membership<-data.frame(geo_id=ids,comparison_id="north",
    comparison_name="Norden (West)",stringsAsFactors=FALSE)
  by_cluster<-regionalepi:::.summarize_weekly_regional_incidence(
    data,membership,"north",TRUE)
  c01_first<-by_cluster[by_cluster$date==dates[[1L]]&by_cluster$cluster_id=="C01",]
  expect_identical(c01_first$median_incidence,0)
  expect_identical(c01_first$observed_districts,1L)
  expect_identical(c01_first$expected_districts,2L)
  expect_identical(c01_first$missing_districts,1L)
  expect_identical(c01_first$completeness,.5)
  c02_second<-by_cluster[by_cluster$date==dates[[2L]]&by_cluster$cluster_id=="C02",]
  expect_true(is.na(c02_second$median_incidence))
  expect_identical(c02_second$observed_districts,0L)
  overall<-regionalepi:::.summarize_weekly_regional_incidence(
    data,membership,"north",FALSE)
  expect_identical(nrow(overall),2L)
  expect_identical(overall$expected_districts,c(3L,3L))
})

test_that("regional relative activity uses the contemporaneous focal-region median", {
  dates<-as.Date(c("2024-01-01","2024-01-08"));ids<-c("01001","01002","01003")
  data<-expand.grid(geo_id=ids,date=dates,stringsAsFactors=FALSE)
  data$cluster_id<-c("C01","C01","C02")[match(data$geo_id,ids)]
  data$incidence<-c(0,NA,8,2,4,NA)
  membership<-data.frame(geo_id=ids,comparison_id="north",
    comparison_name="Norden (West)",stringsAsFactors=FALSE)
  out<-regionalepi:::.regional_relative_activity(data,membership,"north")
  expect_identical(out$regional_reference_median,c(4,4,3,3))
  expect_identical(out$regional_relative_activity,c(-4,4,0,NA))
  expect_identical(out$regional_observed_districts,rep(2L,4L))
  expect_identical(out$regional_expected_districts,rep(3L,4L))
  expect_identical(out$regional_missing_districts,rep(1L,4L))
  expect_equal(out$regional_completeness,rep(2/3,4L))
  expect_identical(out$observed_districts,c(1L,1L,2L,0L))
  expect_true(is.na(out$regional_relative_activity[[4L]]))
})

test_that("regional comparison reconciliation applies level-specific limits", {
  ids<-c("north","south","east","middle_west")
  expect_identical(regionalepi:::.regional_comparison_limit("grossregion"),3L)
  expect_identical(regionalepi:::.regional_comparison_limit("aggregated_state"),2L)
  expect_identical(regionalepi:::.regional_comparison_limit("state"),2L)
  expect_identical(regionalepi:::.reconcile_regional_comparisons(
    "grossregion","south",c("north","east","middle_west"),ids),
    c("north","east","middle_west"))
  expect_identical(regionalepi:::.reconcile_regional_comparisons(
    "grossregion","north",c("north","east","east","south","middle_west"),ids),
    c("east","south","middle_west"))
  expect_identical(regionalepi:::.reconcile_regional_comparisons(
    "aggregated_state","NW",c("HE","BY","TH"),c("NW","HE","BY","TH")),
    c("HE","BY"))
  expect_identical(regionalepi:::.reconcile_regional_comparisons(
    "state","09",c("01","02","03"),sprintf("%02d",1:16)),c("01","02"))
  expect_error(regionalepi:::.reconcile_regional_comparisons(
    "state","99",character(),sprintf("%02d",1:16)),"not valid")
})

test_that("regional relative activity retains supported cluster naming schemes", {
  for(ids in list(sprintf("C%02d",1:2),sprintf("C%02d",1:3),
                  sprintf("C%02d",1:4),sprintf("C%02d",1:5),
                  c("ClD","ClJ","ClA"))) {
    geography<-sprintf("%05d",seq_along(ids))
    x<-expand.grid(geo_id=geography,date=as.Date("2024-01-01")+7L*0:2,
      stringsAsFactors=FALSE)
    x$cluster_id<-ids[match(x$geo_id,geography)]
    x$incidence<-rep(c(0,2,4),each=length(ids))
    membership<-data.frame(geo_id=geography,comparison_id="region",
      comparison_name="Region",stringsAsFactors=FALSE)
    out<-regionalepi:::.regional_relative_activity(x,membership,"region")
    expect_setequal(unique(as.character(out$cluster_id)),ids)
    expect_identical(nrow(out),3L*length(ids))
    expect_true(all(out$regional_relative_activity==0))
  }
})

test_that("regional-type labels derive from authoritative display metadata", {
  metadata <- data.frame(
    display_cluster_id=c("C01", "C02"),
    display_label=c("C01", "C02"),
    profile_description=c("hohe Bevölkerungsdichte, niedrigeres Alter",
                          "höheres Alter; geringe Dichte"),
    mode="dynamic", stringsAsFactors=FALSE)
  choices <- regionalepi:::.shiny_regional_type_choices(metadata)
  expect_identical(names(choices), c(
    "C01 – hohe Bevölkerungsdichte", "C02 – höheres Alter"))
  metadata$mode <- "dissertation"
  metadata$display_label <- c("ClD · dichte Regionen", "ClA · ältere Regionen")
  expect_identical(names(regionalepi:::.shiny_regional_type_choices(metadata)),
    metadata$display_label)
})
