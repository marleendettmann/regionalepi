# Build the reviewed 2024 geography resource bundle.
#
# Run from the package root after placing the official, unmodified VG-Hist
# GeoPackage at data-raw/local/vg-hist.gpkg. The GeoPackage and geometry are
# never written to package data.

reference_date <- as.Date("2024-12-31")
source_file <- "data-raw/local/vg-hist.gpkg"
expected_sha256 <- "101b557e168fa63be400bbff2ca4c4b9e6fd793db7c3f27453cbb7bdf5226ef4"

if (!file.exists(source_file)) {
  stop("Missing local VG-Hist source: ", source_file, call. = FALSE)
}
for (package in c("DBI", "RSQLite")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Builder requires suggested package `", package, "`.", call. = FALSE)
  }
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
  stop("Local VG-Hist checksum does not match the reviewed source.", call. = FALSE)
}

connection <- DBI::dbConnect(RSQLite::SQLite(), source_file)
on.exit(DBI::dbDisconnect(connection), add = TRUE)
query <- paste(
  'SELECT SKZ, GEN, BEG, "END", VGHID, SNS, STG, VGCODE, TYP',
  'FROM vg_hist',
  "WHERE ADE = 4 AND STG = 'DEU'",
  "AND BEG <= '2024-12-31' AND \"END\" >= '2024-12-31'",
  "ORDER BY SKZ"
)
raw_register <- DBI::dbGetQuery(connection, query)

as_vghist_date <- function(x) as.Date(substr(as.character(x), 1L, 10L))
bkg_districts <- data.frame(
  geo_id = as.character(raw_register$SKZ),
  geo_name = as.character(raw_register$GEN),
  geo_level = rep("district", nrow(raw_register)),
  valid_from = as_vghist_date(raw_register$BEG),
  valid_to = as_vghist_date(raw_register$END),
  source = rep("BKG VG-Hist 2026-01; derived 2024-12-31 register",
               nrow(raw_register)),
  vghid = as.character(raw_register$VGHID),
  sns = as.numeric(raw_register$SNS),
  stg = as.character(raw_register$STG),
  vgcode = as.character(raw_register$VGCODE),
  canonical_type = as.character(raw_register$TYP),
  stringsAsFactors = FALSE
)

read_reviewed <- function(path) {
  utils::read.csv(path, colClasses = "character", check.names = FALSE,
                  stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}
survstat_aliases <- read_reviewed(
  "data-raw/reviewed/survstat-aliases-2024.csv"
)
survstat_aliases$valid_from <- as.Date(survstat_aliases$valid_from)
survstat_aliases$valid_to <- as.Date(survstat_aliases$valid_to)
targets <- match(survstat_aliases$target_geo_id, bkg_districts$geo_id)
survstat_aliases$target_vghid <- bkg_districts$vghid[targets]

survstat_incidence_aliases <- read_reviewed(
  "data-raw/reviewed/survstat-incidence-aliases-2024.csv"
)
survstat_incidence_aliases$valid_from <- as.Date(survstat_incidence_aliases$valid_from)
survstat_incidence_aliases$valid_to <- as.Date(survstat_incidence_aliases$valid_to)
incidence_targets <- match(
  survstat_incidence_aliases$target_geo_id, bkg_districts$geo_id
)
survstat_incidence_aliases$target_vghid <- bkg_districts$vghid[incidence_targets]

survstat_spatial_units <- read_reviewed(
  "data-raw/reviewed/survstat-spatial-units-2024.csv"
)
survstat_spatial_units$valid_from <- as.Date(survstat_spatial_units$valid_from)
survstat_spatial_units$valid_to <- as.Date(survstat_spatial_units$valid_to)
survstat_source_spatial_relations <- read_reviewed(
  "data-raw/reviewed/survstat-source-spatial-relations-2024.csv"
)
survstat_source_spatial_relations$valid_from <- as.Date(
  survstat_source_spatial_relations$valid_from
)
survstat_source_spatial_relations$valid_to <- as.Date(
  survstat_source_spatial_relations$valid_to
)
survstat_incidence_assembly_spec <- read_reviewed(
  "data-raw/reviewed/survstat-incidence-assembly-spec-2024.csv"
)
for (field in c(
  "expected_base_units", "expected_excluded_units",
  "expected_replacement_units", "expected_output_units"
)) {
  survstat_incidence_assembly_spec[[field]] <- as.integer(
    survstat_incidence_assembly_spec[[field]]
  )
}

# Load current source validators without requiring an installed development
# version of regionalepi.
validation_environment <- new.env(parent = globalenv())
for (file in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) {
  sys.source(file, envir = validation_environment)
}
validation_environment$validate_geography(bkg_districts)
validation_environment$validate_geography_aliases(survstat_aliases)
validation_environment$validate_geography_aliases(survstat_incidence_aliases)
validation_environment$.validate_spatial_units(survstat_spatial_units)
validation_environment$validate_source_spatial_relations(
  survstat_source_spatial_relations
)
invisible(validation_environment$.validate_incidence_assembly_spec(
  survstat_incidence_assembly_spec, "reviewed incidence assembly specification"
))

stopifnot(
  nrow(bkg_districts) == 400L,
  !anyDuplicated(bkg_districts$geo_id),
  all(nchar(bkg_districts$geo_id) == 5L),
  all(bkg_districts$valid_from <= reference_date),
  all(bkg_districts$valid_to >= reference_date),
  all(!is.na(bkg_districts$vghid)),
  all(!is.na(bkg_districts$canonical_type)),
  nrow(survstat_aliases) == 19L,
  all(!is.na(targets)),
  nrow(survstat_incidence_aliases) == 1L,
  identical(survstat_incidence_aliases$source_label, "Berlin"),
  identical(survstat_incidence_aliases$source_type, "Bundesland"),
  identical(survstat_incidence_aliases$target_geo_id, "11000"),
  all(!is.na(incidence_targets)),
  nrow(survstat_spatial_units) == 12L,
  !anyDuplicated(survstat_spatial_units$source_geo_id),
  !any(survstat_spatial_units$source_geo_id %in% bkg_districts$geo_id),
  nrow(survstat_source_spatial_relations) == 12L,
  setequal(survstat_source_spatial_relations$from_geo_id,
           survstat_spatial_units$source_geo_id),
  all(survstat_source_spatial_relations$to_geo_id == "11000"),
  all(survstat_source_spatial_relations$from_geo_level ==
        "survstat_berlin_bezirk"),
  nrow(survstat_incidence_assembly_spec) == 12L,
  setequal(survstat_incidence_assembly_spec$exclude_geo_id,
           survstat_spatial_units$source_geo_id),
  all(survstat_incidence_assembly_spec$replacement_target_geo_id == "11000"),
  all(survstat_incidence_assembly_spec$expected_base_units == 411L),
  all(survstat_incidence_assembly_spec$expected_output_units == 400L)
)

regionalepi_geography_resources_2024 <- list(
  bkg_districts = bkg_districts,
  survstat_aliases = survstat_aliases,
  survstat_incidence_aliases = survstat_incidence_aliases,
  survstat_spatial_units = survstat_spatial_units,
  survstat_source_spatial_relations = survstat_source_spatial_relations,
  survstat_incidence_assembly_spec = survstat_incidence_assembly_spec,
  provenance = list(
    resource_id = "regionalepi_geography_resources_2024",
    resource_version = "2024.12.31-v2",
    reference_date = reference_date,
    provider = "Bundesamt für Kartographie und Geodäsie (BKG)",
    product = "Verwaltungsgebiete Historisch (VG-Hist)",
    product_version = "2026-01",
    source_sha256 = source_sha256,
    source_license = "Creative Commons Namensnennung 4.0 International (CC BY 4.0)",
    source_license_url = "https://creativecommons.org/licenses/by/4.0/",
    source_information_url = "https://sgx.geodatenzentrum.de/web_public/gdz/datenquellen/datenquellen_vg-hist.pdf",
    derived_resource = TRUE,
    modification_notice = paste(
      "Derived and modified: selected non-geometric German district records",
      "applicable on 2024-12-31; renamed and reordered attributes; omitted geometry."
    ),
    valid_to_transformation = paste(
      "No sentinel normalization applied. Official documentation defines END",
      "as an inclusive end date but does not explicitly define 9999-12-31",
      "as an open-ended sentinel."
    ),
    reviewed_survstat_source = "SurvStat@RKI",
    reviewed_survstat_source_version = "SurvStat@RKI 2.0",
    reviewed_incidence_assembly = paste(
      "The incidence assembly specification replaces 12 reviewed Berlin Bezirk",
      "source-incidence units with one separately queried Bundesland incidence",
      "without summing or averaging rates."
    ),
    directive_applicability = paste(
      "Alias and spatial-unit mappings were reviewed and established for",
      "2024-12-31; the one-day interval does not assert one-day historical existence."
    ),
    builder = "data-raw/build-geography-resources-2024.R",
    package_version = "0.0.0.9000",
    generated_at = as.POSIXct(format(Sys.time(), tz = "UTC", usetz = TRUE), tz = "UTC")
  )
)

dir.create("data", showWarnings = FALSE)
save(regionalepi_geography_resources_2024,
     file = "data/regionalepi_geography_resources_2024.rda",
     compress = "xz", version = 3L)
