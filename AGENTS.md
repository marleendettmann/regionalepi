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
architecture documentation, and narrow authenticated Regionaldatenbank
demographic-context adapters. The separate source-provided SurvStat incidence
contract, local-file reader, and reviewed non-additive replacement assembly are
also implemented. Other network retrieval, standardization, clustering,
visualization, and applications remain outside this scope.

The frozen dissertation typology reproduction is implemented as a narrow
exception to otherwise deferred clustering. It consumes only validated
2017--2020 demographic period summaries, reproduces explicit base-R scaling
and the approved k-means specification, and keeps historical fitted-solution
labels separate from permutation-safe comparison of later fits. It is not a
generic clustering framework.

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
with national totals. Retrieval, PDF parsing, incidence calculation, name resolution,
and source-specific geographic special cases remain out of scope.

## SurvStat source-provided incidence v0.1

Source-provided incidence is separate from additive count surveillance.
`read_survstat_incidence()` supports only Kreis rows with one explicit year and
Meldejahr rows with one explicit filtered source geography. Blank incidence is
`NA_real_`, distinct from numeric zero. Query metadata is caller-supplied,
stored once as internal query-level provenance, and referenced by `query_id` on
observations; Info PDFs are not parsed.

`assemble_surveillance_incidence()` performs only reviewed row replacement. It
never sums or averages rates and incidence is explicitly rejected by additive
geography transformations. The reviewed resource supplies the Berlin-specific
12-to-1 specification and Bundesland alias as data; analytical function code
contains no Berlin branch. Incidence calculation remains deferred.

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
reviewed Berlin Bezirk source identities, a separate reviewed Bundesland
incidence alias, a 12-row incidence replacement specification, and
dataset-level provenance. The
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

## Regionaldatenbank demographic context adapters v0.1

`fetch_regional_population()` is a narrow authenticated adapter for reviewed
Regionaldatenbank table `12411-01-01-4` and statistic `12411`. It retrieves
district total population at 31 December for the approved 2017-2025 period and
returns validated `population_denominator` data with separate diagnostics and
dataset-level provenance. It is not a generic GENESIS client.

Credentials are infrastructure configuration read only from
`REGIONALSTATISTIK_USER` and `REGIONALSTATISTIK_PASSWORD`. The package does not
parse `.env`, expose credential arguments, store credentials, or include them
in returned objects and errors. Ordinary tests use a synthetic internal
transport boundary and require neither credentials nor network access.

Regionaldatenbank AGS values remain character identifiers and source names are
source metadata. The observation `reference_date` does not establish a BKG
territorial vintage, so this adapter returns `geo_vintage = NA_Date_`.
Population-basis codes are `census_2011` for 2017-2021 and `census_2022` for
2022-2025. Narrow adapters retrieve source-provided mean age and youth
dependency quotients plus district area. An internal age-population adapter
validates, but never replaces, the authoritative youth quotient. Population
density is derived by exact identifier/date joins without rounding, and annual
indicators may be summarized only by a complete unweighted arithmetic mean.
All adapter-produced `geo_vintage` values remain `NA_Date_`. Incidence
calculation, denominator selection, standardization, and clustering remain
outside this block.

## Epidemiological contextualization v0.1

Reviewed epidemiological periods are external versioned metadata with
inclusive dates. Influenza seasons and waves are distinct; a season may have
zero, one, or multiple waves, and boundaries are never inferred from SurvStat
incidence or cases. COVID wave semantics remain separate.

Typology attachment uses canonical character `geo_id` plus reviewed expected
set differences; it never uses name matching or hard-coded territorial cases.
The weekly summary estimand is median source-provided district incidence by
period and cluster. Numeric zero is retained, missing incidence remains `NA`,
and no pooled or population-weighted cluster incidence is calculated.
