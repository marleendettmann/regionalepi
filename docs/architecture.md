# Architecture

`regionalepi` separates source-specific acquisition from canonical data,
geographic processing, derived calculations, and client applications.

The intended future processing order is:

1. source adapters produce canonical base data;
2. geographic identifiers are explicitly resolved;
3. spatial aggregation and historical-vintage harmonization are handled as
   distinct operations;
4. additive base quantities are harmonized;
5. ratios, rates, incidence, densities, and demographic indicators are
   recalculated from those quantities;
6. versioned typology specifications may be executed; and
7. clients such as Shiny consume package results.

Only the contracts and specifications needed to describe these boundaries are
implemented in v0.1. The package core has no network or external-API dependency.

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

`aggregate_geography()` transforms differences in spatial resolution using
`identity` and `aggregate` relations. Non-empty input is constrained to one
common `geo_vintage`, which the output preserves.

`harmonize_vintage()` transforms historical merges using `identity` and
`historical_merge` relations. It verifies explicit source and target vintages
and sets output `geo_vintage` to the target vintage.

Both functions require callers to identify additive value columns and every
non-geographic grouping dimension. They reject unmatched or ambiguous units,
unclassified columns, fractional allocation weights, unsupported relation
types, and failed group-wise mass balance. Target `geo_id`, `geo_name`, and
`geo_level` are read only from the canonical geography row valid at the target
date. The same ID may occur in other non-applicable historical register rows.

The functions return ordinary lists containing transformed data and simple
diagnostics. They do not calculate or transform derived epidemiological or
demographic indicators.
