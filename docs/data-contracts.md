# Canonical data contracts v0.1

Tabular contracts are ordinary data frames (including tibbles). Specifications
are ordinary named lists. Validators return their input invisibly on success
and stop with an actionable error on failure; they do not coerce values.

Required fields must be complete: values may not be `NA` or empty unless a
contract explicitly allows that exception. The exceptions are pre-resolution
surveillance `geo_id`; open-ended `age_to`; open geography `valid_to`; optional
relation `weight` and `note`; optional fields such as `parent_geo_id`, `sex`, and
`retrieved_at`; and genuinely unavailable surveillance/context source versions
in analysis provenance.

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

The exported validator documentation lists each contract's required and
optional fields. The optional `geometry` field is retained without requiring or
interpreting any particular spatial class.
