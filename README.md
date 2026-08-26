# regionalepi

`regionalepi` is an early-stage research R package for reproducible demographic
contextualization of regional infectious-disease surveillance data.

The package defines canonical data contracts, deterministic geographic
resolution, and a documented non-geometric 2024 geography resource derived
from BKG VG-Hist with reviewed SurvStat directives. Official source-data
attribution is installed in `NOTICE`; BKG source-data licensing remains
distinct from the package code license (`TBD`).

Reproducible development inputs are kept in Git under `data-raw`, while the
ignored local VG-Hist GeoPackage and all `data-raw` materials are excluded from
built source packages. The package contains no source download, indicator
calculation, clustering, spatial processing, mapping, or application code.

See `docs/architecture.md` and `docs/data-contracts.md` for the v0.1 design.
