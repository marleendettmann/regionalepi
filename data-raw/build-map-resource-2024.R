# Build the reviewed browser-ready VG2500 district map resource.
#
# Run from the package root. The official local GeoPackage is read only and is
# never copied into package data. The output geometry is EPSG:4326 and is not
# simplified.

source_file <- paste0(
  "data-raw/local/bkg/vg2500_2024-12-31/vg2500/",
  "DE_VG2500.gpkg"
)
source_layer <- "vg2500_krs"
source_vintage <- as.Date("2024-12-31")
expected_sha256 <- "85905ab68a49e1f7ce8e2425716212cfd5a614a2e431144998823db95c62cb01"

if (!file.exists(source_file)) {
  stop("Missing local VG2500 source: ", source_file, call. = FALSE)
}
if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Builder requires suggested package `sf`.", call. = FALSE)
}

sha256_file <- function(path) {
  command <- if (nzchar(Sys.which("shasum"))) {
    c(Sys.which("shasum"), "-a", "256", path)
  } else if (nzchar(Sys.which("sha256sum"))) {
    c(Sys.which("sha256sum"), path)
  } else {
    stop("A `shasum` or `sha256sum` executable is required.", call. = FALSE)
  }
  output <- system2(command[[1L]], command[-1L], stdout = TRUE)
  strsplit(output[[1L]], "[[:space:]]+")[[1L]][[1L]]
}

source_sha256 <- sha256_file(source_file)
if (!identical(source_sha256, expected_sha256)) {
  stop("Local VG2500 checksum does not match the reviewed source.", call. = FALSE)
}

load("data/regionalepi_geography_resources_2024.rda")
canonical <- regionalepi_geography_resources_2024$bkg_districts
source <- sf::st_read(source_file, layer = source_layer, quiet = TRUE)

stopifnot(
  nrow(source) == 400L,
  identical(sf::st_crs(source)$epsg, 25832L),
  identical(as.character(sf::st_geometry_type(source)),
            rep("MULTIPOLYGON", nrow(source))),
  is.character(source$AGS),
  all(grepl("^[0-9]{5}$", source$AGS)),
  !anyNA(source$AGS),
  !anyDuplicated(source$AGS),
  !anyNA(source$OBJID),
  !anyDuplicated(source$OBJID),
  !any(sf::st_is_empty(source)),
  all(sf::st_is_valid(source)),
  setequal(source$AGS, canonical$geo_id)
)

source <- source[match(sort(source$AGS), source$AGS), ]
canonical <- canonical[match(source$AGS, canonical$geo_id), ]
stopifnot(identical(source$AGS, canonical$geo_id))

discrepancies <- utils::read.csv(
  "data-raw/reviewed/vg2500-name-discrepancies-2024.csv",
  colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)
observed <- data.frame(
  geo_id = source$AGS[source$GEN != canonical$geo_name],
  source_geo_name = source$GEN[source$GEN != canonical$geo_name],
  canonical_geo_name = canonical$geo_name[source$GEN != canonical$geo_name],
  stringsAsFactors = FALSE
)
stopifnot(
  nrow(observed) == 3L,
  identical(observed$geo_id, discrepancies$geo_id),
  identical(observed$source_geo_name, discrepancies$source_geo_name),
  identical(observed$canonical_geo_name, discrepancies$canonical_geo_name)
)

vertex_count <- function(geometry) {
  sum(vapply(geometry, function(feature) {
    nrow(sf::st_coordinates(feature))
  }, integer(1L)))
}
source_vertex_count <- vertex_count(sf::st_geometry(source))
output <- sf::st_transform(source, 4326)
stopifnot(
  identical(sf::st_crs(output)$epsg, 4326L),
  !any(sf::st_is_empty(output)),
  all(sf::st_is_valid(output)),
  identical(source$AGS, output$AGS)
)
output_vertex_count <- vertex_count(sf::st_geometry(output))

position_list <- function(matrix) {
  lapply(seq_len(nrow(matrix)), function(i) as.numeric(matrix[i, 1:2]))
}
as_geojson_multipolygon <- function(feature) {
  polygons <- lapply(unclass(feature), function(polygon) {
    lapply(unclass(polygon), position_list)
  })
  list(type = "MultiPolygon", coordinates = polygons)
}
geometry <- lapply(sf::st_geometry(output), as_geojson_multipolygon)

features <- data.frame(
  geo_id = canonical$geo_id,
  geo_name = canonical$geo_name,
  geo_level = rep("district", nrow(output)),
  geo_vintage = rep(source_vintage, nrow(output)),
  source_feature_id = as.character(source$OBJID),
  stringsAsFactors = FALSE
)
features$geometry <- I(geometry)

provenance <- list(
  resource_id = "regionalepi_map_geometry_2024",
  resource_version = "vg2500-2024.12.31-map-v1",
  source_organization = "Bundesamt für Kartographie und Geodäsie (BKG)",
  source_product = "Verwaltungsgebiete 1 : 2 500 000 (VG2500)",
  source_vintage = source_vintage,
  source_file = "DE_VG2500.gpkg",
  source_layer = source_layer,
  source_crs = "EPSG:25832",
  output_crs = "EPSG:4326",
  simplification = "none",
  transformation = paste(
    "Selected district layer; exact AGS join to reviewed canonical register;",
    "canonical names substituted only through that join; reprojected with sf::st_transform."
  ),
  source_feature_count = as.integer(nrow(source)),
  output_feature_count = as.integer(nrow(output)),
  source_vertex_count = as.integer(source_vertex_count),
  output_vertex_count = as.integer(output_vertex_count),
  acquisition_provenance = paste(
    "Official BKG delivery supplied locally for reviewed development;",
    "source file modification metadata dates the delivery to 2025;",
    "no independent download transaction was performed by the builder."
  ),
  source_sha256 = source_sha256,
  builder = "data-raw/build-map-resource-2024.R",
  license = "Datenlizenz Deutschland - Namensnennung - Version 2.0",
  license_url = "https://www.govdata.de/dl-de/by-2-0",
  attribution = paste(
    "© BKG 2025 dl-de/by-2-0, Datenquellen:",
    "https://sgx.geodatenzentrum.de/web_public/gdz/datenquellen/datenquellen_vg_nuts.pdf"
  ),
  source_reference = "https://www.bkg.bund.de",
  change_notice = paste(
    "Derived visualization resource: selected 400 district geometries, removed",
    "unneeded attributes, used reviewed canonical names, transformed EPSG:25832",
    "to EPSG:4326, and performed no simplification or area calculation."
  ),
  package_version = "0.0.0.9000"
)

regionalepi_map_geometry_2024 <- list(
  features = features,
  provenance = provenance,
  discrepancies = discrepancies
)

validation_environment <- new.env(parent = globalenv())
for (file in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) {
  sys.source(file, envir = validation_environment)
}
validation_environment$validate_map_geometry_resource(
  regionalepi_map_geometry_2024
)
stopifnot(
  identical(features$geo_id, canonical$geo_id),
  identical(features$geo_name, canonical$geo_name),
  identical(unique(features$geo_vintage), source_vintage),
  sum(features$geo_id == "11000") == 1L,
  sum(features$geo_id == "02000") == 1L,
  !"16056" %in% features$geo_id,
  !any(grepl("^survstat:", features$geo_id))
)

dir.create("data", showWarnings = FALSE)
save(
  regionalepi_map_geometry_2024,
  file = "data/regionalepi_map_geometry_2024.rda",
  compress = "xz", version = 3L
)

