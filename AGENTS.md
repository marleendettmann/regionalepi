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

The v0.1 foundation consists only of canonical data contracts, validators,
versioned reference specifications, tests, and architecture documentation.
Source adapters, geographic resolution and harmonization, calculations,
clustering, visualization, and applications are outside this scope.

Required contract fields are complete unless an explicit contract exception
permits `NA`. Geography relation weights are optional allocation proportions in
`[0, 1]`; sum-to-one and relation-cardinality rules are deferred. The v0.1
relation types do not include an unresolved relation record.
