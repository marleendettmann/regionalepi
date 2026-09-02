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
packages. The package contains the narrow frozen dissertation reproduction, a
separate reviewed dynamic typology path, a visualization-only map resource,
and a first small Shiny proof of concept. It contains no runtime spatial
processing or general-purpose clustering framework.

SurvStat counts and source-provided incidence use separate canonical contracts.
`read_survstat()` remains the additive count reader;
`read_survstat_incidence()` supports only the two reviewed incidence layouts
and preserves blank rates as missing. Reviewed incidence assembly replaces
explicit source units with separately queried replacement observations without
summing or averaging rates.

`fetch_survstat_incidence()` is the first narrow live adapter for the official
RKI SurvStat SOAP service. It retrieves only source-provided weekly incidence
for explicit reporting years, either for Kreis rows or one exactly selected
Bundesland. The adapter discovers exact source members, preserves blank cells
as missing, records the live cube status and member identifiers, and performs
neither geographic resolution nor incidence calculation. Its return contract
is the same as the local incidence reader, so reviewed Berlin replacement and
the existing downstream analysis remain separate steps.

Reviewed source spatial relations support additive aggregation of the 12
SurvStat Berlin Bezirk identities to canonical Berlin without inventing a
territorial vintage. Historical vintage harmonization remains a separate
operation and contract.

`regionalepi_map_geometry()` supplies a separate visualization-only 2024
district resource derived from BKG VG2500. It contains GeoJSON-compatible
EPSG:4326 MultiPolygons, canonical reviewed names, complete BKG attribution,
and no population-area semantics. Ordinary loading and validation require no
`sf`; the reproducible development builder uses `sf` only as a suggested
dependency.

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

`fit_dissertation_typology()` consumes the complete 2017--2020 indicator
summary and reproduces the dissertation's explicit base-R scaling and frozen
three-center k-means. Historical labels require an explicit reproduction mode.
`compare_typology()` safely aligns arbitrary raw cluster numbers to a reviewed
historical partition by evaluating all six permutations.

The separate `demographic_structure_v1` indicator set and
`dynamic_kmeans_v1` fitting specification support deterministic exploratory
demographic typologies for `k = 2` through `5`. Dynamic fits retain raw
k-means numbers, add only fit-local neutral `C01`, `C02`, ... identifiers, and
return original-scale and standardized cluster profiles. They never apply the
frozen dissertation labels or generate natural-language cluster names.

Reviewed period resources represent RKI Influenza waves and frozen
dissertation COVID waves as inclusive date intervals. Seasons may have zero,
one, or multiple waves; boundaries are not inferred from SurvStat. Canonical-ID
typology attachment validates reviewed geography differences. Weekly summaries
calculate median district incidence plus the empirical first and third
quartiles (R quantile type 7). Missing incidence remains distinct from zero;
the IQR describes observed district incidence and is not a confidence interval.
No pooled cluster incidence is calculated.

See `docs/architecture.md` and `docs/data-contracts.md` for the v0.1 design.

## Shiny proof of concept

Install the optional `shiny` and `leaflet` packages, then launch the installed
application with:

```r
regionalepi::run_regionalepi_app()
```

The app does not query live services on startup. Select the reviewed settings
and press **Analyse laden / aktualisieren**. Regionaldatenbank queries require
`REGIONALSTATISTIK_USER` and `REGIONALSTATISTIK_PASSWORD` in the process
environment; SurvStat@RKI must be reachable. Credentials and live responses
are not persisted. Results are cached only in the active Shiny session.

The default path uses the 2022--2024 demographic reference period,
`demographic_structure_v1`, `dynamic_kmeans_v1`, `k = 3`, the reviewed VG2500
2024 map, source-provided SurvStat incidence, reviewed Berlin replacement, and
weekly median district incidence by cluster. Cluster IDs such as `C01` are
fit-local neutral identifiers and must not be interpreted as stable categories
across settings.

The interface offers two explicitly distinct typology modes. Dynamic fits use
neutral fit-local IDs and a neutral deterministic palette. Dissertation mode
reproduces the frozen 2017--2020 fit without refitting and uses the historical
ClD/ClJ/ClA labels and core palette. Its 401-to-current-400 surveillance
compatibility remains explicit.

COVID choices separate the frozen dissertation/RKI pandemic periods from the
reviewed post-pandemic RKI activity waves 2023/24 and 2024/25. RKI phase 8 is
not exposed as a closed interval because no sufficiently authoritative closing
boundary was established. The epidemiological view displays weekly cluster
medians with empirical IQR and an unweighted distribution of one period median
per district. It performs no inference and does not pool district-week values.
Post-2023 COVID reports require caution because testing and reporting conditions
changed.

This remains a development application, not a production service. It has no
disk cache, background jobs, automatic cluster naming, pooled incidence,
arbitrary indicator selection, export workflow, deployment infrastructure, or
inferential main-UI tests. A historical peak-week reproduction is deferred.
BKG attribution is rendered from map-resource provenance.

### Reviewed demographic snapshot

Ordinary Shiny use defaults to the immutable reviewed Regionaldatenbank
snapshot returned by `regionalepi_demographic_snapshot()`. It covers
2017--2020 and 2022--2024 and stores source-provided mean age and youth
dependency plus population and area. Density, period summaries, and typologies
remain reproducible ordinary derivations. The default therefore needs no
Regionaldatenbank credentials; explicit live mode remains available under the
advanced settings and never mutates the bundled snapshot. SurvStat stays live.
