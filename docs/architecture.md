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

The dissertation typology currently names `population_density` and `mean_age`
without claiming authoritative definition versions for them. Its
`youth_dependency_ratio` entry references the authoritative
`dissertation_v1` indicator specification.

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

## Regionaldatenbank total-population adapter v0.1

`fetch_regional_population(reference_dates, regions = NULL)` retrieves only
reviewed Regionaldatenbank table `12411-01-01-4` (statistic `12411`). It uses
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
2019-2021 and `census_2022` for 2022-2025.

Age table `12411-09-01-4` is the reviewed next potential context source, but
age retrieval, youth dependency, mean age, density, incidence calculation, and
denominator-selection policy are not implemented here.
