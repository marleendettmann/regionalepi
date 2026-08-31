app_cluster_colours <- function(ids) {
  palette <- c("#3B6FB6", "#D17C28", "#3F8F6B", "#8A64A8", "#B64E5A")
  levels <- sort(unique(ids))
  stats::setNames(palette[seq_along(levels)], levels)
}

app_map_geojson <- function(map, assignments) {
  colours <- app_cluster_colours(assignments$display_cluster_id)
  position <- match(map$features$geo_id, assignments$geo_id)
  features <- lapply(seq_len(nrow(map$features)), function(index) {
    cluster <- assignments$display_cluster_id[position[[index]]]
    list(
      type = "Feature",
      id = map$features$geo_id[[index]],
      properties = list(
        geo_id = map$features$geo_id[[index]],
        geo_name = map$features$geo_name[[index]],
        cluster = cluster,
        fillColor = unname(colours[[cluster]])
      ),
      geometry = map$features$geometry[[index]]
    )
  })
  jsonlite::toJSON(
    list(type = "FeatureCollection", features = features),
    auto_unbox = TRUE, null = "null", digits = 10
  )
}

app_leaflet_map <- function(map, assignments) {
  geojson <- app_map_geojson(map, assignments)
  widget <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 4))
  widget <- leaflet::addProviderTiles(widget, "CartoDB.Positron")
  widget <- leaflet::addGeoJSON(widget, geojson)
  htmlwidgets::onRender(widget, "
    function(el, x) {
      var map = this;
      map.eachLayer(function(layer) {
        if (!layer.feature || !layer.feature.properties) return;
        var p = layer.feature.properties;
        layer.setStyle({color:'#ffffff', weight:0.7, fillColor:p.fillColor,
                        fillOpacity:0.82});
        layer.bindTooltip('<strong>' + p.geo_name + '</strong><br>' +
                          p.geo_id + ' · ' + p.cluster);
        layer.on('click', function() {
          Shiny.setInputValue('selected_geo_id', p.geo_id,
                              {priority:'event'});
        });
      });
      map.fitBounds([[47.2, 5.5], [55.2, 15.5]]);
    }")
}

app_source_status <- function(source) {
  statuses <- unlist(lapply(source, function(x) {
    candidates <- c(x$diagnostics$data_status, x$provenance$data_status)
    candidates[!vapply(candidates, is.null, logical(1L))]
  }), use.names = FALSE)
  unique(as.character(statuses))
}

app_format_error <- function(error, source) {
  paste0(source, " konnte nicht geladen werden. ", conditionMessage(error),
         " Bitte Einstellungen und Dienstverfügbarkeit prüfen und erneut laden.")
}
