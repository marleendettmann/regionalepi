# Canonical data contracts v0.1

Tabular contracts are ordinary data frames (including tibbles). Specifications
are ordinary named lists. Validators return their input invisibly on success
and stop with an actionable error on failure; they do not coerce values.

Required fields must be complete: values may not be `NA` or empty unless a
contract explicitly allows that exception. The exceptions are pre-resolution
surveillance `geo_id` and `geo_vintage`; open-ended `age_to`; open geography
`valid_to`; optional relation `weight` and `note`; optional fields such as
`parent_geo_id`, `sex`, and `retrieved_at`; and genuinely unavailable
surveillance/context source versions in analysis provenance.

Geographic identifiers are always character vectors. This preserves leading
zeros in identifiers such as German AGS values. Numeric identifiers are errors.

Dates use `Date`; retrieval timestamps use `POSIXct`. Counts are finite,
non-negative, whole-valued numeric values. Population values are numeric,
finite, and non-negative. Population is base data; no contract for later
harmonized population representations is defined here.

An age interval has a non-negative, whole-valued numeric `age_from`, regardless
of integer or double storage. Its `age_to` follows the same rule and is greater
than or equal to `age_from`, or is `NA` for an open-ended group.

Indicator specifications contain `indicator_id`, `indicator_type`,
`definition_version`, and `parameters`. Type-specific structure is confined to
`parameters`. A ratio has numerator and denominator variable/age definitions,
a multiplier, and a unit; other indicator types are not forced into that shape.

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
