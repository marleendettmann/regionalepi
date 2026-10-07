# Geography resource development inputs

The Git repository contains the reproducible builder and reviewed, versioned
SurvStat alias, spatial-unit, and source-spatial-relation inputs. Run
`Rscript data-raw/build-geography-resources-2024.R` from the package root.

The official BKG VG-Hist GeoPackage must be supplied locally as
`data-raw/local/vg-hist.gpkg`. Everything under `data-raw/local/` is ignored and
must not be committed. The builder checks the reviewed source checksum and
reads only non-geometric attributes through DBI and RSQLite; it does not use
`sf`, download data, or create historical territorial relations. Reviewed
source spatial relations are ordinary version-controlled input data.
The reviewed incidence alias and incidence assembly specification are also
ordinary version-controlled inputs. They describe source-provided rate
replacement and are not additive source spatial relations.

`Rscript data-raw/build-map-resource-2024.R` builds the separate reviewed
visualization resource from the ignored official VG2500 file at
`data-raw/local/bkg/vg2500_2024-12-31/vg2500/DE_VG2500.gpkg`. The builder
requires suggested package `sf`, verifies the reviewed source checksum and
exact canonical AGS set, transforms EPSG:25832 geometry to EPSG:4326, and does
not simplify it. Only the derived GeoJSON-compatible R resource is installed;
the GeoPackage and BKG PDFs remain ignored and excluded from package archives.

The complete `data-raw` directory is excluded from built source archives. The
built and installed package instead contains the generated non-geometric
resource, its R documentation, and `inst/NOTICE` with source attribution.

The alias, spatial-unit, and source-spatial-relation validity interval
2024-12-31 through 2024-12-31 means “reviewed and established for this
reference date.” It does not assert
that a mapping existed for only one day and does not claim unreviewed historical
applicability.

VG-Hist documentation defines `BEG` and `END` as inclusive validity dates but
does not explicitly define `9999-12-31` as an open-ended sentinel. The builder
therefore preserves that source value rather than normalizing it to `NA`.

## Reviewed demographic snapshot

Run `Rscript data-raw/build-demographic-snapshot.R` from the package root with
the external Regionaldatenbank credential environment configured. The builder
uses the existing narrow adapters, retrieves the contiguous 2017--2025 range
required by the API, and stores only reviewed years 2017--2020 and 2022--2025.
It also retrieves official annual-average population for 2022--2025 through
the existing `12411-05-01-4` / `BEV028` adapter. It validates component
contracts, exact annual geography, reviewed source statuses, and the
deterministic checksum. Raw envelopes and credentials are never written.

The current installed resource is
`data/regionalepi_demographic_snapshot_v3.rda`; the immutable v2 and v1
resources are retained, with v2 recorded as its predecessor. A refresh is a reviewed
development/release build of a new immutable version; Shiny live retrieval
never updates these resources.

`build-state-boundaries-2024.R` selects exactly 16 `GF = 9` features from the
official local VG2500 `vg2500_lan` layer, transforms EPSG:25832 to EPSG:4326
without simplification, and writes a display-only resource. The ignored source
GeoPackage is never copied into the package.
