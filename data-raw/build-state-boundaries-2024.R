# Build reviewed browser-ready VG2500 Bundesland boundaries for display only.
source_file <- "data-raw/local/bkg/vg2500_2024-12-31/vg2500/DE_VG2500.gpkg"
source_layer <- "vg2500_lan"
expected_sha256 <- "85905ab68a49e1f7ce8e2425716212cfd5a614a2e431144998823db95c62cb01"
if (!requireNamespace("sf", quietly = TRUE)) stop("Builder requires suggested package `sf`.")
sha <- unname(strsplit(system2(Sys.which("shasum"), c("-a", "256", source_file), stdout = TRUE), " ")[[1L]][1L])
stopifnot(identical(sha, expected_sha256))
x <- sf::st_read(source_file, layer = source_layer, quiet = TRUE)
x <- x[x$GF == 9, ]
stopifnot(nrow(x) == 16L, !anyDuplicated(x$AGS), identical(sf::st_crs(x)$epsg, 25832L),
          all(sf::st_geometry_type(x) == "MULTIPOLYGON"), all(sf::st_is_valid(x)))
x <- x[order(x$AGS), ]
y <- sf::st_transform(x, 4326)
positions <- function(m) lapply(seq_len(nrow(m)), function(i) as.numeric(m[i, 1:2]))
geometry <- lapply(sf::st_geometry(y), function(feature) list(
  type = "MultiPolygon",
  coordinates = lapply(unclass(feature), function(poly) lapply(unclass(poly), positions))
))
features <- data.frame(geo_id = x$AGS, geo_name = x$GEN,
  source_feature_id = x$OBJID, stringsAsFactors = FALSE)
features$geometry <- I(geometry)
json_features <- lapply(seq_len(nrow(features)), function(i) list(
  type = "Feature", id = features$geo_id[[i]],
  properties = list(geo_id = features$geo_id[[i]], geo_name = features$geo_name[[i]]),
  geometry = geometry[[i]]))
regionalepi_state_boundaries_2024 <- list(
  features = features,
  browser_geojson = jsonlite::toJSON(list(type = "FeatureCollection", features = json_features),
    auto_unbox = TRUE, null = "null", digits = 10),
  provenance = list(resource_id = "regionalepi_state_boundaries_2024",
    resource_version = "vg2500-2024.12.31-state-boundaries-v1",
    source_organization = "Bundesamt für Kartographie und Geodäsie (BKG)",
    source_product = "Verwaltungsgebiete 1 : 2 500 000 (VG2500)",
    source_vintage = as.Date("2024-12-31"), source_file = "DE_VG2500.gpkg",
    source_layer = source_layer, source_crs = "EPSG:25832", output_crs = "EPSG:4326",
    selection = "GF = 9 (land territory); exactly 16 Länder; no simplification",
    source_sha256 = sha, feature_count = 16L,
    license = "Datenlizenz Deutschland - Namensnennung - Version 2.0",
    license_url = "https://www.govdata.de/dl-de/by-2-0",
    attribution = paste("© BKG 2025 dl-de/by-2-0, Datenquellen:",
      "https://sgx.geodatenzentrum.de/web_public/gdz/datenquellen/datenquellen_vg_nuts.pdf"),
    builder = "data-raw/build-state-boundaries-2024.R"))
validation_environment <- new.env(parent = globalenv())
for (file in sort(list.files("R", pattern = "[.]R$", full.names = TRUE)))
  sys.source(file, envir = validation_environment)
validation_environment$validate_state_boundaries_resource(regionalepi_state_boundaries_2024)
save(regionalepi_state_boundaries_2024,
  file = "data/regionalepi_state_boundaries_2024.rda", compress = "xz", version = 3L)
