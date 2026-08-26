# Geography resource development inputs

The Git repository contains the reproducible builder and reviewed, versioned
SurvStat alias and spatial-unit inputs. Run
`Rscript data-raw/build-geography-resources-2024.R` from the package root.

The official BKG VG-Hist GeoPackage must be supplied locally as
`data-raw/local/vg-hist.gpkg`. Everything under `data-raw/local/` is ignored and
must not be committed. The builder checks the reviewed source checksum and
reads only non-geometric attributes through DBI and RSQLite; it does not use
`sf`, download data, or create historical or aggregation relations.

The complete `data-raw` directory is excluded from built source archives. The
built and installed package instead contains the generated non-geometric
resource, its R documentation, and `inst/NOTICE` with source attribution.

The alias and spatial-unit validity interval 2024-12-31 through 2024-12-31
means “reviewed and established for this reference date.” It does not assert
that a mapping existed for only one day and does not claim unreviewed historical
applicability.

VG-Hist documentation defines `BEG` and `END` as inclusive validity dates but
does not explicitly define `9999-12-31` as an open-ended sentinel. The builder
therefore preserves that source value rather than normalizing it to `NA`.
