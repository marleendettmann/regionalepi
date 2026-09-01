test_that("map contract preserves character AGS and GeoJSON geometry", {
  x <- map_resource_example()
  expect_identical(validate_map_geometry_resource(x), x)
  expect_type(x$features$geo_id, "character")
  expect_identical(x$features$geo_id, c("01001", "02000"))
  expect_identical(x$features$geometry[[1L]]$type, "MultiPolygon")
  expect_false(any(vapply(x$features$geometry, inherits, logical(1L), "sfc")))
})

test_that("map feature identity and vintage validation is strict", {
  x <- map_resource_example(); x$features$geo_id[[2L]] <- "01001"
  expect_error(validate_map_geometry_resource(x), "exactly one")
  x <- map_resource_example(); x$features$geo_id[[1L]] <- NA_character_
  expect_error(validate_map_geometry_resource(x), "must not contain NA")
  x <- map_resource_example(); x$features$geo_id[[1L]] <- "1001"
  expect_error(validate_map_geometry_resource(x), "five-character")
  x <- map_resource_example(); x$features$geo_vintage[[1L]] <- as.Date(NA)
  expect_error(validate_map_geometry_resource(x), "must not contain NA")
})

test_that("empty and malformed geometry fail without sf", {
  x <- map_resource_example(); x$features$geometry[[1L]]$coordinates <- list()
  expect_error(validate_map_geometry_resource(x), "non-empty MultiPolygon")
  x <- map_resource_example(); x$features$geometry[[1L]]$type <- "Polygon"
  expect_error(validate_map_geometry_resource(x), "MultiPolygon")
  x <- map_resource_example()
  x$features$geometry[[1L]]$coordinates[[1L]][[1L]][[4L]] <- c(8, 54)
  expect_error(validate_map_geometry_resource(x), "closed")
})

test_that("map provenance and reviewed discrepancies are enforced", {
  x <- map_resource_example(); x$provenance$output_crs <- ""
  expect_error(validate_map_geometry_resource(x), "output_crs")
  x <- map_resource_example(); x$provenance$output_feature_count <- 3L
  expect_error(validate_map_geometry_resource(x), "contradict")
  x <- map_resource_example(); x$provenance$output_crs <- "EPSG:25832"
  expect_error(validate_map_geometry_resource(x), "EPSG:4326")
  x <- map_resource_example(); x$discrepancies$canonical_geo_name <- "Source"
  expect_error(validate_map_geometry_resource(x), "discrepancies")
  x <- map_resource_example(); x$discrepancies$geo_id <- "99999"
  expect_error(validate_map_geometry_resource(x), "discrepancies")
})

test_that("reviewed 2024 map resource has exact package integrity", {
  resource <- regionalepi_map_geometry()
  geography <- regionalepi_geography_resources_2024$bkg_districts
  expect_identical(validate_map_geometry_resource(resource), resource)
  expect_identical(resource$features$geo_id, geography$geo_id)
  expect_identical(resource$features$geo_name, geography$geo_name)
  expect_identical(nrow(resource$features), 400L)
  expect_identical(unique(resource$features$geo_vintage), as.Date("2024-12-31"))
  expect_identical(resource$provenance$source_vertex_count, 60707L)
  expect_identical(resource$provenance$output_vertex_count, 60707L)
  expect_identical(resource$provenance$simplification, "none")
  expect_identical(resource$provenance$output_crs, "EPSG:4326")
  expect_identical(nchar(resource$browser_geojson, type = "bytes"),
                   resource$provenance$browser_geojson_bytes)
  expect_identical(
    regionalepi:::.map_browser_geojson_md5(resource$browser_geojson),
    resource$provenance$browser_geojson_md5
  )
  expect_identical(resource$discrepancies$geo_id, c("03403", "10046", "12071"))
  expect_identical(sum(resource$features$geo_id == "11000"), 1L)
  expect_identical(sum(resource$features$geo_id == "02000"), 1L)
  expect_false("16056" %in% resource$features$geo_id)
  expect_false(any(grepl("^survstat:", resource$features$geo_id)))
})

test_that("browser-ready map avoids runtime geometry serialization", {
  resource <- regionalepi_map_geometry()
  assignments <- data.frame(
    geo_id = resource$features$geo_id,
    display_cluster_id = "C01", stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    .shiny_app_map_geojson = function(...) stop("runtime serialization used"),
    .package = "regionalepi"
  )
  widget <- regionalepi:::.shiny_app_leaflet_map(resource, assignments)
  expect_s3_class(widget, "leaflet")
})

test_that("map accessor rejects unsupported vintages", {
  expect_error(regionalepi_map_geometry(as.Date("2025-12-31")), "unsupported")
  expect_error(regionalepi_map_geometry(20241231), "Date")
})
