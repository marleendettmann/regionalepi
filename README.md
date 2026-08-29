# regionalepi

`regionalepi` is an early-stage research R package for reproducible demographic
contextualization of regional infectious-disease surveillance data.

The package defines canonical data contracts, deterministic geographic
resolution, and a documented non-geometric 2024 geography resource derived
from BKG VG-Hist with reviewed SurvStat directives. Official source-data
attribution is installed in `NOTICE`; BKG source-data licensing remains
distinct from the package code license (`TBD`).

Reproducible development inputs are kept in Git under `data-raw`, while ignored
local source files and all `data-raw` materials are excluded from built source
packages. The package contains no clustering, spatial processing, mapping, or
application code.

SurvStat counts and source-provided incidence use separate canonical contracts.
`read_survstat()` remains the additive count reader;
`read_survstat_incidence()` supports only the two reviewed incidence layouts
and preserves blank rates as missing. Reviewed incidence assembly replaces
explicit source units with separately queried replacement observations without
summing or averaging rates.

Reviewed source spatial relations support additive aggregation of the 12
SurvStat Berlin Bezirk identities to canonical Berlin without inventing a
territorial vintage. Historical vintage harmonization remains a separate
operation and contract.

`fetch_regional_population()` retrieves reviewed district total-population
data for 2017-2025 from Regionaldatenbank table `12411-01-01-4`. It requires
the ordinary environment variables `REGIONALSTATISTIK_USER` and
`REGIONALSTATISTIK_PASSWORD`; the package does not parse `.env` or accept
credentials as function arguments. Narrow companion adapters retrieve
source-provided mean age and youth dependency quotients and district area.
Population density is derived without rounding, and complete annual indicators
may be summarized with an unweighted arithmetic mean. Geographic vintage
remains unresolved; no historical harmonization is inferred from observation
dates.

See `docs/architecture.md` and `docs/data-contracts.md` for the v0.1 design.
