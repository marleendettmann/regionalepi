# Architecture

`regionalepi` separates source-specific acquisition from canonical data,
geographic processing, derived calculations, and client applications.

The intended future processing order is:

1. source adapters produce canonical base data;
2. geographic identifiers are explicitly resolved;
3. spatial aggregation and historical-vintage harmonization are handled as
   distinct operations;
4. additive base quantities are harmonized;
5. calculated ratios, rates, incidence, densities, and demographic indicators
   are recalculated from those quantities, while explicitly source-provided
   rates remain separate non-additive observations;
6. versioned typology specifications may be executed; and
7. clients such as Shiny consume package results.

The package core remains independent of external-service availability. The
narrow Regionaldatenbank source adapter is the only authenticated network
adapter in this scope; normal tests and all downstream contracts and geography
operations remain offline.

Geographic matching must never be guessed silently. A missing `geo_id` is
allowed in canonical surveillance data only at the initial source-adapter stage
when no identifier can be resolved. After a future `resolve_geography()` step,
unresolved identifiers must be rejected or explicitly diagnosed before
analytical processing. That function and analytical processing are not part of
v0.1.

The dissertation definitions are preserved as versioned reference
specifications. They are examples that support reproducibility, not fixed
package-wide defaults.

The dissertation typology references authoritative `dissertation_v1`
specifications for `population_density`, `mean_age`, and
`youth_dependency_ratio`. The youth ratio's age boundaries are defined only by
its indicator specification.

## Geography Layer v0.1

`aggregate_geography()` transforms source-specific differences in spatial
resolution using reviewed `source_spatial_relations` plus canonical same-ID
passthrough. Its explicit reference date selects applicable relations and
target-register rows; it does not require, infer, or change `geo_vintage`.

`harmonize_vintage()` transforms historical merges using `identity` and
`historical_merge` relations. It verifies explicit source and target vintages
and sets output `geo_vintage` to the target vintage.

Both functions require callers to identify additive value columns and every
non-geographic grouping dimension. They reject unmatched or ambiguous units,
unclassified columns, unsupported relation types, and failed group-wise mass
balance. Target `geo_id`, `geo_name`, and `geo_level` are read only from the
canonical geography row valid at the relevant reference or target date. The
same ID may occur in other non-applicable historical register rows.

The functions return ordinary lists containing transformed data and simple
diagnostics. They do not calculate or transform derived epidemiological or
demographic indicators.

## SurvStat file adapter v0.1

`read_survstat()` is a local-file source adapter for the narrowly supported
SurvStat case-count export layout. It performs no retrieval and has no API or
PDF dependency. It preserves source geography labels rather than resolving or
normalizing them, and returns unresolved identifiers and, when unavailable, an
unresolved geography vintage. Those values must be resolved before geography
transformations or analytical processing.

The adapter converts ISO reporting weeks to canonical Monday dates and checks
weekly counts against the national source totals. It neither imports nor
calculates incidence. The result is an ordinary `list(data, diagnostics)`,
consistent with the Geography Layer return convention.

## Source-provided SurvStat incidence v0.1

`surveillance_incidence` is a separate non-additive contract. Its shared
geographic-observation fields permit deterministic identity resolution without
weakening count validation, but `incidence` is rejected by both additive
geography transformations.

`read_survstat_incidence()` supports only two evidenced layouts: Kreis rows
with one explicit reporting year, and Meldejahr rows with one explicit source
geography supplied through query-filter metadata. Blank rate cells are
`NA_real_`, not zero. Query settings are explicit caller-supplied provenance;
the package does not parse Info PDFs or expose a generic public query-spec API.

`fetch_survstat_incidence()` is a deliberately narrow network adapter for the
official RKI `SurvStatWebService.svc` SOAP service. It uses the reviewed
`SurvStat` cube and its exact hierarchy/member identifiers, requests only the
service-provided incidence measure, and chunks deterministic requests by one
explicit reporting year. The public interface supports Kreis output or one
exactly filtered Bundesland; it is not a generic MDX or SOAP client. The
official endpoint is
`https://tools.rki.de/SurvStat/SurvStatWebService.svc`; the reviewed service
metadata requires no authentication. The
adapter records retrieval time, cube data status, endpoint, binding, operation,
hierarchies, and selected member identifiers. SOAP faults, malformed payloads,
unknown source members, duplicate observations, and unsupported measure or
geography scopes fail explicitly. Ordinary tests replace an internal transport
boundary and require no network access.

The API is a second acquisition path into the existing non-additive incidence
contract, not a second analytical model. Source labels and source member IDs
remain source provenance, `geo_id` and `geo_vintage` remain unresolved before
the ordinary resolver, and no rate is calculated, summed, or averaged. The
reviewed Berlin workflow still consists of an independent Kreis query, an
independent Bundesland query, deterministic resolution, and reviewed
replacement assembly. No Berlin condition is present in the adapter.

`assemble_surveillance_incidence()` is a reviewed replacement operation. It
removes specified base source units and appends separately queried replacement
observations after strict temporal and query compatibility checks. It performs
no rate arithmetic. Query-level metadata is retained once and observation rows
link to it through `query_id`.

## Geographic Resolution v0.1

`resolve_geography()` establishes geographic identity without aggregating,
harmonizing vintages, or calculating indicators. It resolves each unique source
unit once, using deterministic source parsing followed by exact canonical name
and type matching. If exact matching fails, a complete source label may use an
explicit reviewed alias. There is no fuzzy matching, normalization, or inferred
alias behavior.

The resolver returns `data`, a one-row-per-source-unit `resolution` audit, and
compact `diagnostics`. Observation rows retain only `source_geo_id`,
`source_geo_name`, and `source_geo_level` provenance. Canonical target names,
levels, and optional `vghid` provenance come from the applicable canonical
register row.

Reviewed `spatial_units` identify real finer- or different-resolution source
units without pretending that they are an aggregate target. Their opaque
source IDs are caller-supplied, and their successful state is
`requires_spatial_relation`. A later relation and `aggregate_geography()` call
remain necessary.

The register `reference_date` is resolution provenance only. Source
`geo_vintage` is preserved unchanged, including `NA_Date_`. Any unresolved or
ambiguous identity aborts the complete resolution call.

## Reviewed geography resources v0.1

`regionalepi_geography_resources_2024` is the small non-geometric runtime layer
for the reviewed 2024 resolution case. It contains the BKG VG-Hist district
register applicable on 2024-12-31, exact reviewed SurvStat aliases, reviewed
Berlin Bezirk source identities, and compact dataset-level provenance.
It also contains a separate reviewed Bundesland incidence alias and versioned
12-row incidence assembly specification. Those data express the Berlin PoC;
the reader, resolver, and assembly function contain no Berlin-specific branch.

The reproducible builder and reviewed inputs are kept in Git under `data-raw`.
The official GeoPackage stays ignored under `data-raw/local`, and `data-raw` is
excluded from built source packages. Only the generated resource, its
documentation, and the installed attribution notice are shipped.

The register reference date is not evidence of the SurvStat source territorial
vintage, so resolution preserves `geo_vintage = NA_Date_`.

## Source spatial aggregation relations

Source-version-specific spatial-resolution mappings are represented by
`source_spatial_relations`, not historical `geography_relations`.
`aggregate_geography()` selects them at an explicit applicability/reference
date and permits canonical same-ID passthrough only against one uniquely valid
target-register row. It never interprets that date as `geo_vintage`.

The reviewed resource includes 12 ordinary relations from the reviewed
SurvStat Berlin Bezirk identities to canonical Berlin `11000`.
`harmonize_vintage()` remains a separate historical operation using explicit
source and target vintages and `geography_relations`.

## Regionaldatenbank demographic context adapters v0.1

`fetch_regional_population(reference_dates, regions = NULL)` retrieves only
reviewed Regionaldatenbank table `12411-01-01-4` (statistic `12411`) for
2017--2025. It uses
the official GENESIS-compatible `data/table` operation, validates the API
envelope and `Structure` metadata, and then parses the semicolon-delimited table
content. It selects district total population (`Insgesamt`, unit `Anzahl`) and
does not expose a generic public GENESIS client.

Authentication is resolved internally from `REGIONALSTATISTIK_USER` and
`REGIONALSTATISTIK_PASSWORD`. Credentials are absent from the public signature,
returned results, errors, examples, and stored package state. The package does
not read `.env`. An internal transport boundary makes the ordinary test suite
completely synthetic and offline.

The return value is `list(data, diagnostics, provenance)`. Observation data use
the separate `population_denominator` contract. Diagnostics contain compact
request/result counts and credential-free status information; longer source,
methodological, population-basis, and attribution material appears once in
provenance.

Regionaldatenbank supplies character AGS and source names directly. It is not
used for name resolution, and source names do not supersede the canonical BKG
register. Reference dates are population observation dates. Because they do not
independently establish territorial vintage, `geo_vintage` remains `NA_Date_`.
The documented population-basis break is represented as `census_2011` for
2017--2021 and `census_2022` for 2022--2025.

Three additional narrow adapters use the same internal credential, transport,
envelope, and provenance infrastructure. `fetch_regional_mean_age()` reads
source-provided `BEV519` from `12411-07-01-4`;
`fetch_regional_youth_dependency()` reads authoritative `BEV216` from
`12411-08-01-4`; and `fetch_regional_area()` reads `FLC006` square kilometres
from `11111-01-01-4`. An internal `12411-09-01-4` age-population adapter exists
only to reproduce the source youth quotient independently. It never replaces
the authoritative source value.

The KREISE responses include Deutschland, Länder, and intermediate hierarchy
rows. The adapters retain district observations only. Regionaldatenbank emits
district-equivalent Hamburg and Berlin at their two-character city-state keys
`02` and `11`; the adapters deterministically expand those source keys to AGS
`02000` and `11000`. Structurally absent historical rows marked `-` are omitted
from an all-district result and recorded in diagnostics; an explicitly
requested structurally absent region is an error. Other missing or quality
markers remain errors.

`derive_population_density()` joins population and area only by exact AGS and
reference date, requires compatible names, levels, and NA-aware vintages, and
divides without rounding. `summarize_indicator_period()` accepts only complete
annual observations and an unweighted arithmetic mean. Dissertation workflow
code obtains 2017--2020 and the method from `dissertation_typology_spec()`.
Neither function harmonizes geography, infers `geo_vintage`, or calculates
incidence.

## Dissertation typology reproduction

`fit_dissertation_typology()` is deliberately not a generic clustering API. It
accepts the complete `dissertation_v1` 2017--2020 period summary, requires the
three authoritative indicator definitions and units, orders the columns from
the frozen specification, and orders five-character AGS lexically before
fitting. It explicitly centers by column means and divides by sample standard
deviations with denominator `n - 1`, without rounding.

The fit uses the frozen base-R k-means parameters. Raw cluster numbers retain
no general substantive meaning. The historical ClD/ClJ/ClA mapping is applied
only under the explicit historical-reference option. `compare_typology()`
instead evaluates all six mappings of a candidate three-cluster solution and
chooses the greatest assignment agreement on common IDs, leaving raw clusters
unchanged. Local historical workbooks remain ignored development evidence and
are not package data or ordinary test fixtures.

## Map geometry and dynamic typology

The reviewed `regionalepi_map_geometry_2024` resource is visualization
geography, not an extension of the canonical non-spatial geography contract.
Its 400 features are exact-ID joined to the reviewed 2024 register and use
canonical register names. Geometry comes from BKG VG2500 `vg2500_krs`, is
transformed once from EPSG:25832 to EPSG:4326, and is not simplified. The
three reviewed source/canonical name differences remain explicit resource
data. Geometry is stored as ordinary GeoJSON-compatible R lists, so core
loading and validation do not depend on `sf`; the reproducible builder uses
suggested `sf`. Map geometry is never an area denominator.

Dynamic demographic clustering is a separate path from
`fit_dissertation_typology()`. `demographic_structure_v1` orders the three
reviewed indicator definitions and specifies complete common-unit sample-SD
z-standardization without fixing a period. `dynamic_kmeans_v1` independently
specifies Lloyd k-means, 50 starts, 100 iterations, seed `20241231`, ascending
character-ID row order, and supported `k` values 2 through 5.

`fit_dynamic_typology()` validates complete period summaries, constructs the
ordered matrix, preserves caller RNG state, and returns immutable raw cluster
numbers plus deterministic fit-local `C01`, `C02`, ... display IDs. Those IDs
are ordered lexicographically by standardized centers in indicator-set order
and have meaning only when qualified by `fit_id`. Profiles retain original
means/medians, standardized centers, neutral ranks, sizes, and small-cluster
flags. No dissertation or generated natural-language labels are applied.

`compare_dynamic_partitions()` reports only same-unit contingency, cluster
sizes, and adjusted Rand index. It accepts differing `k` without forcing a
one-to-one mapping. Historical `compare_typology()` remains unchanged and
three-cluster-specific.

## Epidemiological contextualization

The epidemiology layer follows canonical geography, reviewed incidence
assembly, and typology fitting. Reviewed compatibility data describe expected
typology/surveillance ID-set differences; generic code has no Eisenach,
Berlin, or name-based branch.

Versioned period resources keep RKI Influenza-wave metadata separate from
SurvStat observations. A season can have no wave or multiple waves, while
COVID wave semantics remain separate. Assignment uses inclusive dates and
preserves query provenance. Weekly contextualization calculates the median
distribution of district incidences, not a pooled rate. Missing incidence is
never converted to zero by the production path.

Frozen resources preserve the dissertation's three Influenza intervals and
principal COVID `Welle2` intervals. The current reviewed RKI resource covers
2017/18 through 2025/26, including wave-free 2020/21, the late low 2021/22
wave, and both 2022/23 waves. Automatic wave detection and inferential tests
remain future blocks.

## Shiny proof of concept

The package-owned application under `inst/shiny/regionalepi` is a thin client
of the existing backend. `run_regionalepi_app()` locates the installed app and
checks optional `shiny` and `leaflet` dependencies. Ordinary analytical use
does not load either package. Profile and incidence charts use base graphics;
the map consumes the browser-ready GeoJSON-compatible resource without `sf`.

An explicit action button starts live work. A session-local environment caches
the map, demographic sources and summary by reviewed reference period,
typology by summary/specification/k, SurvStat results by pathogen/year scope,
and epidemiological summaries by source scope/period/fit. Changing a map
selection changes only display state. No response or credential is written to
disk.

The server orchestrates, but does not reproduce, Regionaldatenbank retrieval,
indicator derivation and summarization, dynamic fitting, SurvStat retrieval,
reviewed geography resolution, Berlin incidence replacement, period
assignment, canonical-ID typology attachment, or median-incidence summary.
The default 2022--2024 path has exact 400-unit map compatibility. The reviewed
2017--2020 path retains its 401-unit fit and reports `16056` as the expected
fit-only identifier while rendering the compatible 400 current map units.

## Reviewed demographic snapshot

The bundled demographic snapshot is an explicit immutable Regionaldatenbank
source state, not an opaque or silently stale cache. Normalized components
preserve values, table/measure provenance, component retrieval times, the
reviewed source-status compatibility rule, population bases, and a
deterministic checksum. It covers exactly 2017--2020 and 2022--2024.
Population density, period summaries, and typologies are not stored: the
snapshot is reconstructed into the existing validated source results and then
uses the ordinary derivation pipeline.

Shiny defaults to this snapshot without credentials. Explicit live mode uses
the existing authenticated adapters only in session memory. A refresh means a
reviewed development/release build of a new version; Shiny never overwrites
package data. SurvStat retrieval remains live and separate.

## Structured Shiny analysis interface

The persistent analysis sidebar feeds four display sections; changing tabs or
the selected map district does not enter any retrieval or fitting dependency.
The default remains the reviewed 2022--2024 snapshot, dynamic k = 3 typology,
and live SurvStat. Explicit dissertation mode instead uses the unchanged frozen
2017--2020 fit, historical ClD/ClJ/ClA labels and core palette, and the reviewed
401-to-current-400 compatibility with `16056` retained as fit-only provenance.

COVID selection keeps frozen dissertation/RKI pandemic periods separate from
`covid_rki_activity_waves_v1`, which contains only the reviewed post-pandemic
2023/24 and 2024/25 RKI activity waves. Phase 8 is historical context only:
its 2022-KW22 start is documented, but no unreviewed closing boundary is
manufactured.

Weekly contextualization displays the median and empirical Q1/Q3 of observed
district incidences. This IQR is descriptive, not a confidence interval. The
period distribution first calculates one unweighted weekly-incidence median
per district; district-week observations are never pooled. The main interface
contains no inferential tests. `ggplot2` is an optional Shiny visualization
dependency and core analytical use remains independent of it.

## SurvStat additive count context

The official count wrapper reuses the incidence adapter's SOAP transport,
member resolution, one-year chunks, geography resolution and session-only
principles. It exposes only `Anzahl.71s`, not arbitrary cube measures. Counts
remain an additive surveillance contract and never enter incidence calculation.

After resolution, the reviewed twelve Berlin source units are removed and the
independent Bundesland Berlin count is appended, preserving the existing
399-plus-one analytical geography. Their additive sum is diagnostic only.
Incidence/count combination requires exact canonical ID/date keys, year/week,
pathogen, reference definition, reporting path and cube data status. A cube
status change between sequential requests fails explicitly rather than joining
different source snapshots.

The Shiny server retrieves incidence and reported counts once per pathogen and
required source-year coverage and combines them with the validated exact-key
operation. Broader cached coverage can satisfy a narrower window. Analysis
range, zoom, tab and district/week selection do not cause source retrieval.

### Observation windows and linked heatmaps

An Influenza observation window runs from KW40 through KW20 of
the following year, with the reviewed RKI wave highlighted inside it; complete
reporting years remain available when analytically necessary. COVID should use
the neutral term observation window, provisionally KW20 through KW20 of the
following year, never "official season". Reviewed activity waves are overlays,
and recent data may remain visible without a completed official wave. Any later
custom interval must be labelled user-defined rather than an RKI wave.

The preferred primary weekly heatmap estimand is cluster median incidence minus
the all-district median for the same week. Its diverging colours mean below or
above the contemporaneous all-district level; differences remain stable when
the national level is near zero, unlike ratios. Missing summaries remain
missing. It supports k = 2--5 and suits dashboard and descriptive paper use.
Secondary pairwise cluster-median differences suit detailed comparison; a
district-by-week heatmap ordered by cluster is a drill-down, with missing cells
explicit and counts available only as contextual hover information.

Pathogen and observation-window inputs are reconciled as one validated Shiny
selection state. A pathogen change cannot expose the preceding pathogen's
window ID to range selection or retrieval. If the incoming window has no
reviewed period, a reviewed-range selection safely falls back to the complete
observation window.

Dynamic cluster colours remain display metadata. For k = 2, a cluster receives
an anchor colour only when at least 80 percent of its members come from the
same k = 3 anchor, that anchor is also its nearest standardized center, and the
distance is below 0.75. A coarse mixture uses the existing additional purple;
this does not assert a historical identity. For k = 3 the three unchanged
dissertation colours are aligned one-to-one through the reviewed defining
features population density, youth dependency, and mean age. For k = 4 or 5,
one-to-one maximum district overlap with the k = 3 anchors retains visual
continuity; standardized center distance breaks overlap ties, and remaining
clusters receive the additional purple and muted-rose colours. Every colour is
unique within a fit. C01/C02/C03 alone never determine colour or dissertation
semantics, and the procedure does not make k-means hierarchical.

The Shiny client constructs one fit-local display-metadata table before any
view is rendered. It is the sole source of cluster order, labels, profile
descriptions, colours, and displayed district counts. Frozen dissertation
display order is `ClD`, `ClJ`, `ClA`; dynamic display IDs remain fit-local.
Display metadata never changes analytical assignments.

The district-by-week view constructs explicit district and week registries and
aligned incidence, case-count, and missingness matrices before Plotly
conversion. Matrix cells are checked against the analytical `geo_id`-by-date
observations. The categorical strip uses each registry row's exact display
colour rather than a continuously interpolated heatmap colour scale.
Valid ranges with no observations return a typed empty attachment and stop at
the exploration boundary with a concise user-facing message; no rows are
fabricated. Changing only dates remains downstream of retrieval and fitting
caches.
Changing pathogen or observation window invalidates only the currently
displayed result before downstream widgets can combine the new date state with
an older surveillance bundle. Session caches remain available for the next
explicit analysis load.
