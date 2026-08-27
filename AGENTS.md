# regionalepi architectural principles

1. Source adapters are separated from analysis logic.
2. The package core must not depend on SurvStat, GENESIS, BKG, or any other external API being available.
3. Geographic harmonization precedes calculation of derived indicators.
4. Additive base quantities are harmonized first. Ratios, rates, incidences, densities, and demographic indicators are recalculated afterwards.
5. Never silently guess geographic matches. Ambiguous or unresolved mappings must produce an explicit error, warning, or diagnostic as appropriate.
6. Do not implement Berlin-specific conditions in analytical functions. Different spatial resolutions must be represented through geographic relations.
7. Spatial aggregation and historical boundary/vintage harmonization are conceptually distinct operations.
8. Geographic identifiers such as AGS are strings, never numeric identifiers.
9. Indicator definitions are explicit and versioned. Age boundaries must not be hard-coded into generic indicator functions.
10. The dissertation typology is a versioned reference specification, not the package's fixed typology.
11. Analysis provenance must remain traceable: source, retrieval/reference date, geographic vintage, indicator definition, typology specification, and package version.
12. Shiny is a client of the package. Core analytical logic must not live exclusively in Shiny.
13. New core behavior should be covered by automated tests.

## Current scope

The implemented v0.1 foundation includes canonical data contracts, validators,
versioned reference specifications, the approved local SurvStat file adapter,
deterministic geographic identity resolution, geography transformations, tests,
and architecture documentation. Network retrieval, derived calculations,
clustering, visualization, and applications remain outside this scope.

Required contract fields are complete unless an explicit contract exception
permits `NA`. Geography relation weights are optional allocation proportions in
`[0, 1]`; sum-to-one and relation-cardinality rules are deferred. The v0.1
relation types do not include an unresolved relation record.

## Geography Layer v0.1

Spatial-resolution aggregation and historical-vintage harmonization are
separate public operations. Both operate only on caller-declared additive base
quantities, preserve caller-declared dimensions, require complete unambiguous
relations, and enforce group-wise mass balance. Derived indicators are never
transformed.

`aggregate_geography()` uses applicability-dated `source_spatial_relations` and
validated canonical same-ID passthrough. It does not require, infer, or change
`geo_vintage`; differing values remain separate data dimensions.
`harmonize_vintage()` requires explicit source and target vintages and sets the
output vintage to the target. Target identifiers, names, and levels come only
from the canonical geography row valid at the relevant reference or target
date. Historical splits, boundary allocation, and fractional relation weights
are not implemented in v0.1.

## SurvStat file adapter v0.1

The local-file adapter supports only the approved UTF-16LE, tab-delimited
SurvStat weekly case-count export structure. It returns an ordinary list with
validated canonical `data` and separate `diagnostics`. Source geography labels
remain verbatim; `geo_id` and `geo_vintage` may be `NA` only at this
pre-resolution stage. Blank weekly cells are zero only under the explicit
`blank_is_zero` policy, and imported weekly geographic counts must reconcile
with national totals. Retrieval, PDF parsing, incidence data, name resolution,
and source-specific geographic special cases remain out of scope.

## Geographic Resolution v0.1

`resolve_geography()` performs source-specific deterministic identity
resolution against a canonical register selected at an explicit reference
date. Exact matches use both name and an explicitly selected canonical type
column. Reviewed aliases are exact mappings and are considered only after exact
matching fails. Fuzzy matching and text normalization are not used.

Reviewed spatial units represent real source-level identities that remain
distinct pending explicit geographic relations and aggregation. Their opaque
source IDs come only from caller-supplied reviewed registry data. Resolution
does not infer an aggregation target or territorial vintage. Unresolved or
ambiguous units abort the complete call.

## Reviewed geography resources v0.1

`regionalepi_geography_resources_2024` contains a non-geometric BKG VG-Hist
district register for 2024-12-31, 19 exact reviewed SurvStat aliases, 12
reviewed Berlin Bezirk source identities, and dataset-level provenance. The
development builder and reviewed inputs are version-controlled under
`data-raw`; the official local GeoPackage is ignored and all `data-raw`
materials are excluded from built packages.

The dated directives mean reviewed and established for the reference date and
do not claim historical applicability. The resource contains 12 reviewed
source spatial relations from the Berlin identities to canonical Berlin
`11000`; these are not historical territorial-vintage relations.

`aggregate_geography()` uses `source_spatial_relations`, canonical same-ID
passthrough, and an explicit applicability/reference date. It never infers or
changes `geo_vintage`. `harmonize_vintage()` remains exclusively based on
historical `geography_relations` and explicit source/target vintages.
