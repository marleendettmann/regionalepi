transition_fixture <- function() {
  from_demography<-regionalepi:::.shiny_fetch_snapshot_demography(2017:2020)
  to_demography<-regionalepi:::.shiny_fetch_snapshot_demography(2022:2025)
  from_fit<-fit_dynamic_typology(from_demography$summary,k=3L)
  to_fit<-fit_dynamic_typology(to_demography$summary,k=3L)
  ids<-regionalepi_map_geometry()$features$geo_id
  list(from_fit=from_fit,to_fit=to_fit,ids=ids,
    result=compare_dynamic_typology_transitions(from_fit,to_fit,ids))
}

test_that("reviewed dynamic transition regression is exact", {
  x<-transition_fixture()$result
  expected<-matrix(c(113L,8L,3L,14L,193L,6L,0L,0L,63L),
    nrow=3L,byrow=TRUE,dimnames=list(
      from=c("older","family","dense"),to=c("older","family","dense")))
  expect_identical(unclass(x$count_matrix),unclass(as.table(expected)))
  expect_equal(rowSums(x$count_matrix),c(older=124,family=213,dense=63))
  expect_equal(colSums(x$count_matrix),c(older=127,family=201,dense=72))
  expect_equal(unname(rowSums(x$row_percentage_matrix)),rep(100,3L),
    tolerance=1e-12)
  expect_identical(x$diagnostics$compared_districts,400L)
  expect_identical(x$diagnostics$unchanged_districts,369L)
  expect_identical(x$diagnostics$changed_districts,31L)
  expect_equal(x$diagnostics$changed_percentage,7.75)
  expect_identical(x$diagnostics$from_fit_only_geo_ids,"16056")
  expect_length(x$diagnostics$to_fit_only_geo_ids,0L)
  expect_identical(nrow(x$assignments),400L)
  expect_identical(length(unique(x$assignments$geo_id)),400L)
  expect_identical(unique(x$assignments$alignment_version),
    "demographic_profile_v1")
})

test_that("profile alignment is semantic and permutation invariant", {
  fixture<-transition_fixture()
  expect_identical(fixture$result$profile_alignment$from$profile_class,
    c("older","family","dense"))
  expect_identical(fixture$result$profile_alignment$to$profile_class,
    c("older","family","dense"))
  permute_ids<-function(fit){
    mapping<-c(C01="C03",C02="C01",C03="C02")
    fit$assignments$display_cluster_id<-unname(mapping[
      fit$assignments$display_cluster_id])
    fit$profiles$display_cluster_id<-unname(mapping[
      fit$profiles$display_cluster_id])
    fit$cluster_diagnostics$display_cluster_id<-unname(mapping[
      fit$cluster_diagnostics$display_cluster_id])
    fit
  }
  permuted<-compare_dynamic_typology_transitions(
    permute_ids(fixture$from_fit),permute_ids(fixture$to_fit),fixture$ids)
  expect_identical(permuted$count_matrix,fixture$result$count_matrix)
  expect_identical(permuted$assignments$from_profile_class,
    fixture$result$assignments$from_profile_class)
  expect_identical(permuted$assignments$to_profile_class,
    fixture$result$assignments$to_profile_class)
})

test_that("transition comparison rejects ambiguous or invalid inputs", {
  fixture<-transition_fixture()
  ambiguous<-fixture$from_fit
  rows<-ambiguous$profiles$indicator_id=="mean_age"
  maximum<-max(ambiguous$profiles$standardized_center[rows])
  candidates<-which(rows)
  ambiguous$profiles$standardized_center[candidates[1:2]]<-maximum
  expect_error(compare_dynamic_typology_transitions(
    ambiguous,fixture$to_fit,fixture$ids),"unique maxima")
  expect_error(compare_dynamic_typology_transitions(fixture$from_fit,
    fixture$to_fit,c(fixture$ids[[1L]],fixture$ids[[1L]])),"unique complete")
  expect_error(compare_dynamic_typology_transitions(fixture$from_fit,
    fixture$to_fit,c(fixture$ids[-1L],"99999")),"occur in both fits")
  duplicate<-fixture$from_fit
  duplicate$assignments<-rbind(duplicate$assignments,
    duplicate$assignments[1L,,drop=FALSE])
  expect_error(compare_dynamic_typology_transitions(duplicate,
    fixture$to_fit,fixture$ids),"unique")
})

test_that("transition comparison preserves fits and supplies indicator changes", {
  fixture<-transition_fixture()
  from_before<-serialize(fixture$from_fit,NULL)
  to_before<-serialize(fixture$to_fit,NULL)
  result<-compare_dynamic_typology_transitions(
    fixture$from_fit,fixture$to_fit,fixture$ids)
  expect_identical(serialize(fixture$from_fit,NULL),from_before)
  expect_identical(serialize(fixture$to_fit,NULL),to_before)
  expect_identical(nrow(result$indicator_changes),1200L)
  expect_setequal(unique(result$indicator_changes$indicator_id),c(
    "population_density","mean_age","youth_dependency_ratio"))
  expect_equal(result$indicator_changes$absolute_change,
    result$indicator_changes$to_value-result$indicator_changes$from_value)
  expect_true(all(is.finite(result$indicator_changes$from_value)))
  expect_true(all(is.finite(result$indicator_changes$to_value)))
})

test_that("transition views consume one unchanged result object", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("leaflet")
  fixture<-transition_fixture()
  result_before<-serialize(fixture$result,NULL)
  sankey<-regionalepi:::.shiny_transition_sankey_widget(fixture$result)
  built<-plotly::plotly_build(sankey)
  expect_identical(sum(built$x$data[[1L]]$link$value),400L)
  expect_setequal(built$x$data[[1L]]$link$value,
    as.integer(fixture$result$count_matrix[fixture$result$count_matrix>0L]))
  map<-regionalepi_map_geometry()
  widget<-regionalepi:::.shiny_transition_map_widget(fixture$result,map,
    regionalepi:::.shiny_app_map_bounds(map),regionalepi_state_boundaries(),
    regionalepi:::.shiny_germany_outline_geojson(),"all")
  expect_s3_class(widget,"leaflet")
  legend_calls<-Filter(function(call)
    identical(call$method,"addLegend"),widget$x$calls)
  expect_length(legend_calls,1L)
  expect_identical(legend_calls[[1L]]$args[[1L]]$title,
    "Typologie\u00fcbergang")
  legend_labels<-unlist(legend_calls[[1L]]$args[[1L]]$labels,
    use.names=FALSE)
  expect_true("Profil unver\u00e4ndert"%in%legend_labels)
  expect_true(any(grepl("\u00c4lter/l\u00e4ndlich",legend_labels,fixed=TRUE)))
  expect_true(any(grepl("Familie/Jugend",legend_labels,fixed=TRUE)))
  expect_true(any(grepl("Dicht",legend_labels,fixed=TRUE)))
  expect_identical(serialize(fixture$result,NULL),result_before)
})
