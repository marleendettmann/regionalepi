# Canonical data contracts v0.1

Tabular contracts are ordinary data frames (including tibbles). Specifications
are ordinary named lists. Validators return their input invisibly on success
and stop with an actionable error on failure; they do not coerce values.

Required fields must be complete: values may not be `NA` or empty unless a
contract explicitly allows that exception. The exceptions are pre-resolution
surveillance `geo_id` and `geo_vintage`; blank source-provided `incidence`;
open-ended `age_to`; unresolved source-stage `geo_vintage` in context
population, demographic indicators, regional area, and population denominator;
open geography
`valid_to`; optional relation `weight` and `note`; optional fields such as
`parent_geo_id`, `sex`, and `retrieved_at`; and genuinely unavailable
surveillance/context source versions in analysis provenance.

Geographic identifiers are always character vectors. This preserves leading
zeros in identifiers such as German AGS values. Numeric identifiers are errors.

Dates use `Date`; retrieval timestamps use `POSIXct`. Counts are finite,
non-negative, whole-valued numeric values. Population values are numeric,
finite, and non-negative. Population is base data; no contract for later
harmonized population representations is defined here.

## Population denominator

`population_denominator` is a separate total-population contract. It requires
`geo_id`, `geo_name`, `geo_level`, `geo_vintage`, `reference_date`,
`population`, `population_basis`, `source`, `source_table`, `retrieved_at`, and
`data_status`. All fields are complete except that `geo_vintage` may be
`NA_Date_` when no independently established territorial vintage is available.

Each `geo_id` and `reference_date` combination is unique. All rows in one data
set share one complete `POSIXct` retrieval event. Population is finite,
non-negative numeric base data and is not required to use integer storage.
`population_basis` is a documented, non-empty source-provenance code.

This contract does not replace age-banded `context_population`. It also does
not select an incidence denominator, calculate incidence, or establish that an
observation reference date is a geographic vintage.

The reviewed Regionaldatenbank adapter emits `census_2011` for 2017-2021 and
`census_2022` for 2022-2025. Detailed methodological descriptions are stored
once in result-level provenance rather than repeated in every observation.

An age interval has a non-negative, whole-valued numeric `age_from`, regardless
of integer or double storage. Its `age_to` follows the same rule and is greater
than or equal to `age_from`, or is `NA` for an open-ended group.

Indicator specifications contain `indicator_id`, `indicator_type`,
`definition_version`, and `parameters`. Exactly `source_provided`, `ratio`, and
`density` are accepted. Type-specific structure is confined to `parameters`.
A ratio has numerator and denominator variable/age definitions, a multiplier,
and a unit; source-provided and density specifications have their own strict
structures.

## Demographic indicators and regional area

`demographic_indicator` is annual long-form district data with geographic and
31 December reference fields, an indicator ID/value/unit and definition
version, `source_provided` or `derived` value origin, population basis, source,
and compact provenance ID. Values are finite and non-negative, keys are unique,
and `geo_vintage` may remain `NA_Date_` until an annual source identifier set is
independently matched to a canonical snapshot. No validator requires a fixed
number of districts.

`regional_area` contains positive finite `area_km2` observations from reviewed
table `11111-01-01-4`, measure `FLC006`, plus source, retrieval, status, and
provenance fields. It also permits unresolved `geo_vintage` and does not infer
vintage from its observation date.

The narrow Regionaldatenbank adapters filter KREISE response hierarchy rows
and expand source city-state keys `02` and `11` to the five-character AGS
`02000` and `11000`. This is explicit source-format handling, not geographic
name matching or territorial-vintage inference.

The authoritative dissertation specifications are
`mean_age@dissertation_v1`,
`youth_dependency_ratio@dissertation_v1`, and
`population_density@dissertation_v1`. The youth unit is
`persons_under_20_per_100_persons_20_64`; this is a semantic-unit terminology
correction, not a numerical definition change. Its sole authoritative formula
remains population age 0--19 divided by population age 20--64, multiplied by
100.

Density output links once to both population and area provenance and is not
rounded. Period summaries require exactly one complete compatible observation
per requested year and calculate only an unweighted arithmetic mean; no missing
values are removed.

Geography relation weights are optional finite allocation proportions in
`[0, 1]`. No sum-to-one or relation-cardinality rules are defined in v0.1.

Geography transformations accept ordinary data frames with complete canonical
`geo_id`, `geo_name`, `geo_level`, and `geo_vintage` fields. Value columns must
be explicitly declared additive, numeric, finite, and complete. Every other
non-geographic column must be named as a grouping dimension; unclassified
columns are errors rather than silently dropped.

Transformation v0.1 supports only one-to-one and many-to-one mappings, so an
applicable relation weight must be `NA` or one. Fractional weights are valid
relation metadata but require allocation behavior that is not implemented.
Mass balance is checked for every value column within every grouping
combination.

The exported validator documentation lists each contract's required and
optional fields. The optional `geometry` field is retained without requiring or
interpreting any particular spatial class.

## SurvStat file adapter v0.1

`read_survstat()` reads only the supported German-language, UTF-16LE,
tab-delimited SurvStat case-count export layout. It returns an ordinary list
with `data` and `diagnostics`; diagnostics are not data-frame attributes.
Source geography labels are preserved verbatim, while `geo_id` remains
`NA_character_` pending explicit resolution. `geo_vintage` is caller-supplied
and may remain `NA_Date_` at this same pre-resolution stage.

Blank geographic weekly cells become zero only when the caller accepts the
explicit default `blank_is_zero = TRUE`; otherwise they are errors. Weekly
geographic totals must reconcile exactly with the national `Gesamt` row.
`Unbekannt` and presentation totals are not canonical geography observations.

The adapter preserves `reporting_year` and `reporting_week` and represents the
canonical `date` as the Monday of the ISO week. `reference_definition` is
logical provenance: `TRUE` means selected, `FALSE` means not selected, and `NA`
means unavailable or unknown. `reporting_path` is character provenance.
`retrieved_at` is the time at which the export or query was retrieved or
executed. `data_status` is the source data status reported by SurvStat for that
query; it is a separate provenance concept.

## Source-provided surveillance incidence

`surveillance_incidence` requires the common surveillance geographic and
temporal fields, `incidence`, and `query_id`. Incidence is numeric, finite when
present, non-negative, and explicitly non-additive. `NA_real_` preserves a
blank or unavailable source rate and is not numeric zero. Count surveillance
remains a separate whole-valued `cases` contract.

`read_survstat_incidence()` returns `list(data, diagnostics, provenance)`.
Repeated query settings are stored once in internal query-level provenance;
observation rows contain the compact query identifier. The reader supports
only reviewed Kreis-row and Meldejahr-row layouts and does not parse Info PDFs.

`fetch_survstat_incidence()` returns the same three-part structure and the same
validated observation contract. Its API provenance additionally records the
official endpoint, SOAP binding and operation, cube, exact pathogen/geography
member identifiers, selected geography hierarchy, and reporting-week coverage
for each requested year. `retrieved_at` is the local UTC execution time;
`data_status` is the source cube's independently reported last-update time.
The query uses explicit years and deterministic one-year chunks; no paging
token or implicit current-year default is accepted.

For API observations, blank source cells are `NA_real_` and an explicit
numeric source zero is `0`. Kreis labels are returned verbatim with
`geo_level = "survstat_kreis"`; a filtered state result uses the exact selected
source member caption with `geo_level = "survstat_bundesland"`. Both enter the
contract with unresolved `geo_id` and `geo_vintage`, to be resolved by the
existing reviewed geography layer. Source member identifiers are provenance,
not canonical geographic identifiers.

## Annual-average population and derived incidence

`annual_average_population` is distinct from the 31 December population
denominator. It contains one strictly positive population for each character
district identifier and reporting year, with `population_measure =
"annual_average_population"`, `population_reference =
"reporting_year_annual_average"`, table `12411-05-01-4`, retrieval time, data
status, population basis, and a compact provenance ID. `geo_vintage` may remain
`NA_Date_`; reporting year does not establish territorial vintage.

`derive_incidence_annual_average()` joins source-provided SurvStat cases to that
contract exactly by `geo_id` and reporting year and creates the separate field
`incidence_annual_average = cases / annual_average_population * 100000`. It does
not round, weight, standardize, or overwrite a source-provided `incidence`
field. Structural-zero cases therefore yield zero, while genuinely missing
cases yield missing derived incidence.

`prepare_analysis_incidence()` is the explicit boundary for current dynamic
analysis. It preserves the source-provided SurvStat rate as
`incidence_source`, places `incidence_annual_average` into the established
downstream `incidence` field, and records `population_year` and
`incidence_status`. Equal reporting and population years are final. An
available official denominator from exactly the preceding year is permitted
only with `provisional` status; larger lags and silent fallbacks are rejected.
Historical reference reproduction does not cross this boundary.

Reviewed incidence assembly requires exact query compatibility for source,
version, pathogen, measure, reference definition, reporting path, relevant
filters, time unit, years, weeks, and data status. Retrieval timestamps may
differ and remain in provenance. Assembly only removes and appends rows: rates
are never summed or averaged. Additive geography operations reject a canonical
`incidence` value column.

## Geography aliases

Reviewed aliases contain `source`, `source_version`, `source_label`,
`source_type`, `target_geo_id`, `valid_from`, `valid_to`, `reason`, and
`review_status`. `target_vghid` is optional. Identifiers are character and
validity dates use `Date`; `valid_to` may be open. In v0.1 `review_status` must
be `"reviewed"`.

An `NA_character_` source version is permitted only when the source genuinely
has no version. Matching is NA-aware and exact: missing versions match only
missing versions and are never wildcards. Aliases are complete-label mappings,
not normalization rules.

## Spatial-unit registry

The optional geographic-resolution spatial-unit registry contains `source`,
`source_version`, `source_label`, `source_type`, `source_geo_id`,
`source_geo_name`, `source_geo_level`, `valid_from`, `valid_to`, `reason`, and
`review_status`. A source ID is a non-empty opaque character identifier supplied
by the reviewed registry; it is not inferred and is not assumed to be an AGS.
Applicable source IDs are unique within their source and source-version
context.

These records do not contain or imply an eventual aggregate target. They mark
recognized source identities that require explicit spatial relations.

## Reviewed geography resources

`regionalepi_geography_resources_2024` bundles the canonical non-geometric BKG
VG-Hist district register applicable on 2024-12-31 with the reviewed SurvStat
aliases and spatial-unit identities established for that reference date.
Separate reviewed data identify the Bundesland incidence alias and the 12-to-1
incidence replacement specification; these are not spatial aggregation or
historical-vintage relations.
Dataset-level provenance records the BKG product/version, source checksum,
license, derivation, builder, and selection date without repeating long
metadata in every row.

The one-day directive intervals express the scope of review, not one-day
geographic existence or unreviewed historical applicability. The BKG
resolution reference date must not be substituted for SurvStat `geo_vintage`.

## Source spatial relations

`source_spatial_relations` are distinct from historical
`geography_relations`. They contain `source`, NA-aware `source_version`,
`from_geo_id`, `from_geo_level`, `to_geo_id`, `relation_type`, `valid_from`,
`valid_to`, `review_status`, and `reason`. Applicability dates select reviewed
source-resolution directives; they are not territorial vintages. V0.1 permits
only non-overlapping reviewed `aggregate` and `identity` mappings.

`aggregate_geography()` uses these relations plus validated canonical same-ID
passthrough. It accepts an explicit `reference_date`, never infers or changes
`geo_vintage`, and keeps different vintage values in separate groups. Known
resolver `source_geo_*` provenance is omitted from aggregated output because
the separate resolution audit is authoritative; other unclassified columns
remain errors.

## Historical reference typology input and output

The typology input is the ordinary `list(data, diagnostics, provenance)` from
`summarize_indicator_period()`. Its long-form data must contain exactly one
complete 2017--2020 arithmetic mean per five-character `geo_id` and each of
`population_density@dissertation_v1`, `mean_age@dissertation_v1`, and
`youth_dependency_ratio@dissertation_v1`. Units, versions, periods, and ID sets
must match exactly; missing values are errors and are never silently omitted.

The fit returns an ordinary list. Assignment data contain `geo_id`, immutable
`raw_cluster`, and optional historical `cluster_code`/`cluster_label`. Matrix
diagnostics retain the input and standardized matrices, indicator and row
order, scaling centers, and sample standard deviations. Fit diagnostics retain
centers, sizes, sums of squares, iterations, and any base-R fault value.

## Map geometry resource

`map_geometry_resource` is an ordinary `list(features, provenance,
discrepancies)` and is separate from canonical non-spatial geography.
Features contain one complete five-character character `geo_id`, canonical
name, level, complete map vintage, source feature ID, and one non-empty closed
GeoJSON-compatible `MultiPolygon`. The generic validator requires unique IDs
but no universal feature count. Geometry coordinates are finite EPSG:4326 XY
positions and require no `sf` class.

Dataset-level provenance records source product/vintage/file/layer, input and
output CRS, feature and vertex counts, checksum, acquisition context, builder,
license, attribution, and change notice. Reviewed source/canonical name
differences are explicit keyed data. The reviewed 2024 resource additionally
requires exact equality with the canonical 2024 register, 400 features, no
simplification, and vintage 2024-12-31. It is visualization-only and is never
an area source.

## Dynamic indicator sets and typologies

`indicator_set_spec` contains a versioned ordered indicator-reference list,
standardization settings, completeness policy, and review provenance. It does
not contain a demographic reference period. `demographic_structure_v1`
requires, in order, population density, mean age, and youth dependency ratio,
all at `dissertation_v1`, with complete common IDs and explicit base-R
sample-SD z-standardization.

`dynamic_fitting_spec` separately versions algorithm, starts, iteration limit,
seed, row ordering, and supported `k`. `dynamic_kmeans_v1` supports exactly
2--5 and does not inherit frozen historical-reference parameters.

Dynamic fit assignments retain `fit_id`, canonical `geo_id`, immutable raw
cluster number, and a neutral fit-local display ID. Profile rows contain one
cluster/indicator combination with original mean/median, standardized center,
neutral ranks, size, proportion, definition, and unit. Cluster diagnostics
carry withinss and the non-mutating minimum-size warning threshold
`max(5, ceiling(0.02 * n))`. Display IDs and comparisons never imply
cross-fit semantic equivalence or historical ClD/ClJ/ClA labels.

## Epidemiological periods

An epidemiological period row contains `period_set_id`, `period_id`,
`pathogen`, optional `season_id`, `period_type`, `label`, inclusive
`start_date` and `end_date`, `definition_version`, `source_reference`,
`review_status`, and optional `note`. Dates are authoritative. Identifiers are
complete, intervals are ordered, IDs are unique within a set, and mutually
exclusive periods may not overlap.

An Influenza season is not an Influenza wave. A reviewed season can contain
zero, one, or multiple wave rows. A separate season-review table records that
cardinality, including seasons with no wave. RKI boundaries are reviewed
external metadata; regionalepi does not infer them from SurvStat incidence or
cases. Influenza and COVID waves retain separate `period_type` semantics.

`assign_epidemiological_periods()` matches canonical observation dates to
inclusive intervals and returns data, diagnostics, and the period audit.
Observations outside all periods remain explicit and an observation may match
at most one period in a mutually exclusive set.

## Typology attachment and incidence summary

Typology/surveillance compatibility records the exact reviewed ID-set
difference. `attach_typology()` joins canonical character `geo_id` only,
requires unique assignments, validates the complete observed set difference,
and never performs name matching, refitting, reassignment, or incidence
transformation.

`summarize_incidence_by_typology()` supports only the median in v0.1. Its
estimand is median district incidence from the explicitly selected analysis
contract by pathogen, period, week, typology, and cluster. It reports expected,
observed non-missing,
missing, and zero counts plus minimum-group-size status. Zero remains zero;
`NA` is omitted only explicitly and an all-missing group has an `NA` median.
No pooled or population-weighted cluster incidence is calculated.

Internal pairwise Shiny summaries separate `pair_key` from `pair_label`.
`pair_key` is the stable combination of two cluster IDs in authoritative
display order; `pair_label` is the concise `A - B` axis text. There is exactly
one row per date and pair key, and `difference` is `median_a - median_b`.

## Shiny orchestration boundaries

The Shiny application introduces no analytical data contract. Its internal
cache keys encode only the inputs that can change each stage: demographic years for live
demography, pathogen and reporting years for SurvStat, summary provenance plus
fitting specification and `k` for typology, and surveillance scope, reviewed
period and fit ID for the final summary. Map joins use canonical `geo_id`
exclusively. The 2017--2020 map difference is retained explicitly as typology-
only `16056`; it is neither renamed nor silently treated as a 2024 feature.

## Demographic snapshot

The current `demographic_snapshot` contains normalized `mean_age`,
`youth_dependency`, `population`, and `area` source components plus the
distinct official `annual_average_population` component for reporting years
2022--2025. Components require duplicate-free five-character IDs. The typology
components retain 401 units in 2017--2020 and 400 in 2022--2025 without
harmonization or inferred `geo_vintage`; every annual-average population year
matches the reviewed 400-district geography.

Regionaldatenbank reports response-generation times separately for sequential
table requests. The four typology components preserve their statuses and must
come from one calendar date within a 15-minute reviewed build window. The
annual-average population retains its separately reviewed source status and
retrieval timestamp. The deterministic checksum covers normalized values and
stable source provenance while excluding volatile retrieval/build times.

## Period display and incidence-distribution additions

Epidemiological period rows additionally carry optional `variant_context` and
`historical_context` plus required `evidence_class`. These fields and the
authoritative inclusive dates drive UI titles, ISO-week boundaries, evidence,
and source display; the UI does not duplicate period labels.

The weekly typology summary preserves its median estimand and now also reports
empirical Q1 and Q3 using R quantile type 7 plus completeness proportion. The
IQR is not estimation uncertainty. `summarize_period_incidence_by_district()`
returns exactly one unweighted median of weekly district incidence from the
selected analysis contract per district, with week completeness diagnostics.
Neither contract pools or population-weights incidence.

## SurvStat reported cases and incidence compatibility

API case observations use the existing `surveillance` fields with whole-valued
non-negative `cases`; `NA_real_` is the explicit source-null exception and is
not converted to zero. Query provenance identifies the exact member
`[Measures].[FallCount_71_Web]`, label `Anzahl.71s`, request value `Count`, and
`value_semantics = "additive"`.

This API-null rule does not retroactively change the local case-file adapter's
explicit `blank_is_zero` policy. That policy is an export-specific caller
decision backed by its national-total reconciliation; it is not evidence that
SOAP nulls are zero.

The assembled case-count result has one canonical row per `geo_id` and date.
The combined observation adds `cases` and `count_query_id` to unchanged
source-provided incidence rows. Compatibility requires exact key sets, matching
reporting year/week metadata, pathogen, requested years, reference definition,
reporting path, source/version and cube status. Both query registries remain
separate provenance; population and derived incidence are absent.

Future district detail may show incidence, reported cases, ISO week, cluster,
and observed/missing status. Period outlier detail may show the district period
median, cumulative cases, and observed/expected weeks. Weekly cluster totals
are optional context only because population and cluster size affect them.

## Observation windows and display boundaries

The `observation_window` contract is separate from epidemiological periods. It
contains a stable ID, pathogen, label, inclusive dates with matching ISO
year/week boundaries, window type, definition version, and note. V1 supports
Influenza KW40--KW20 season windows and neutral COVID-19 KW20--KW20 observation
windows. A window neither asserts nor infers an RKI wave.

`regionalepi_state_boundaries_2024` contains exactly 16 browser-ready Länder
outlines from BKG VG2500 `vg2500_lan`. They are display-only EPSG:4326 geometry
with EPSG:25832 source provenance and no simplification.

The Shiny client joins Bundesland names from this reviewed state context via
canonical geographic identifiers. District-by-week ordering, categorical
cluster strips, separators, fit-local profile-aligned colours, zoom controls,
and selected-row
highlights are presentation only. Missing incidence remains `NA` and is
rendered separately from numeric zero.

Internal Shiny display metadata contain one row per displayed cluster with fit
ID, display and raw cluster IDs, optional profile anchor, display label,
profile description, colour, order, displayed district count, and mode.
Historical-reference rows additionally carry their frozen code and label.
All views consume this same session object; it is display metadata, not a new
analytical or public package contract.

For dynamic k = 2, clear continuation of a k = 3 profile anchor requires a
dominant membership share, agreement with the nearest standardized center,
and a bounded center distance. Otherwise the cluster is explicitly displayed
with an existing additional category colour as a coarse mixture. Empty date
subsets retain typed display columns and are rejected by the exploration
boundary rather than receiving fabricated observations.
