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
built source packages. The package contains no indicator calculation,
clustering, spatial processing, mapping, or application code.

Reviewed source spatial relations support additive aggregation of the 12
SurvStat Berlin Bezirk identities to canonical Berlin without inventing a
territorial vintage. Historical vintage harmonization remains a separate
operation and contract.

`fetch_regional_population()` retrieves reviewed district total-population
data for 2019-2025 from Regionaldatenbank table `12411-01-01-4`. It requires
the ordinary environment variables `REGIONALSTATISTIK_USER` and
`REGIONALSTATISTIK_PASSWORD`; the package does not parse `.env` or accept
credentials as function arguments. Geographic vintage remains unresolved and
no incidence or demographic indicator is calculated.

See `docs/architecture.md` and `docs/data-contracts.md` for the v0.1 design.
