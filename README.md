# regionalepi

`regionalepi` is an R package for reproducible analysis of regional
infectious-disease surveillance in Germany using demographic indicators and
typologies.
It provides tools to construct demographic regional typologies for German
districts, link district assignments to other data, and analyse
infectious-disease surveillance by demographic type and time. The package
supports both exact reproduction of a historical reference typology and the
fitting of updated demographic typologies.

## Scientific approach

The typology uses population density, mean age, and the youth dependency ratio.

The historical reference typology uses demographic data from 2017--2020 and
reproduces the three-cluster regional typology developed by
[Dettmann (2026)](https://doi.org/10.17169/refubium-51449). This workflow is
provided for exact historical reproduction.

Updated typologies can be fitted for demographic periods covered by the
package data. The bundled demographic snapshot currently supports 2017--2020
and 2022--2025. These fits support `k = 2` through `5`; identifiers such as
`C01` and `C02` apply only to the individual fit and are not stable categories
across settings. For updated typologies, `k = 3` is retained as an
interpretative reference for comparison with the historical three-cluster
structure; this does not imply that three clusters are statistically optimal.

The historical and updated workflows also use different incidence measures.
Exact historical reproduction uses the source-provided historical SurvStat
incidence. Current analyses calculate incidence from reported SurvStat cases
and the official annual-average population for the reporting year.

## Data sources

Surveillance data come from SurvStat@RKI 2.0, provided by the Robert
Koch-Institut. The bundled Shiny application is configured for the exact
SurvStat pathogen captions
`"Influenza, saisonal"`, `"COVID-19"`, and
`"Norovirus-Gastroenteritis"`. These are reviewed Shiny configurations, not a
general restriction of the package API. Other SurvStat pathogens can be
processed programmatically where they satisfy the package's surveillance and
geography contracts, but they have not necessarily undergone pathogen-specific
validation in `regionalepi`. Influenza and COVID-19 include additional
methodological review and epidemiological period definitions. Norovirus is
included in the Shiny application and covered by regression tests, but has not
undergone the same depth of pathogen-specific methodological review.
Surveillance data are retrieved live; query time and the source data status
reported by SurvStat are retained with the results.

Demographic data come from Regionaldatenbank Deutschland, provided by the
Statistische Ämter des Bundes und der Länder. The package includes a reviewed
snapshot for reproducible offline analysis. It contains the source data needed
for the 2017--2020 and 2022--2025 demographic reference periods and supplies
the typology indicators used by the Shiny application by default, together
with official annual-average population for reporting years 2017--2025.
Snapshot mode therefore supports the complete default dynamic analysis without
Regionaldatenbank credentials. Public package functions can retrieve population
stock, annual-average population, area, mean age, and youth dependency directly
from Regionaldatenbank Deutschland; see `?fetch_regional_population`,
`?fetch_regional_average_population`, and the related `?fetch_regional_*` help
pages. Live retrieval of the typology data and annual-average population is
optional and requires `REGIONALSTATISTIK_USER` and
`REGIONALSTATISTIK_PASSWORD`.

The annual-average population uses the Census 2011 progression basis for
2017--2021 and the Census 2022 progression basis from 2022. This changes the
population-estimation basis, not the incidence formula; comparisons across the
2021/2022 transition should acknowledge the methodological discontinuity.

District identities, map geometry, and Länder boundaries use BKG resources
with the common geographic reference date 31 December 2024. Source attribution
and licensing details are recorded in the installed `NOTICE` file.

## Getting started

The following workflow prepares the bundled 2022--2025 demographic data and
fits a three-cluster updated typology entirely offline:

```r
snapshot <- regionalepi_demographic_snapshot()

demography <- prepare_demographic_snapshot(
  snapshot = snapshot,
  reference_years = 2022:2025
)

fit <- fit_dynamic_typology(
  demography$summary,
  k = 3L
)

head(fit$assignments)
fit$profiles

clusters <- fit$assignments[c("geo_id", "display_cluster_id")]
head(clusters)
```

The fitted result contains district assignments and cluster profiles on both
the original indicator scales and the standardized scale. `clusters` provides
the demographic regional type for each district through the five-character
district identifier `geo_id`. After the package's geography-resolution step,
compatible district-level data use the same five-character district identifier
`geo_id`, allowing these assignments to be joined to surveillance or other
district data.

The package also exposes public functions for retrieving SurvStat observations,
preparing incidence based on official annual-average population, attaching a
demographic typology, assigning stored epidemiological periods where available,
and summarizing incidence by demographic regional type and time. See
`?fetch_survstat_cases`, `?prepare_analysis_incidence`, `?attach_typology`,
`?assign_epidemiological_periods`, and `?summarize_incidence_by_typology`.

### Programmatic surveillance

Surveillance retrieval can also be started directly from R. The following is a
live, network-dependent request using an exact SurvStat pathogen caption; it
does not require Regionaldatenbank credentials:

```r
influenza_cases <- fetch_survstat_cases(
  pathogen = "Influenza, saisonal",
  reporting_years = 2025L,
  geography = "kreis",
  query_id = "example-influenza-2025"
)

head(influenza_cases$data[c(
  "geo_name", "reporting_year", "reporting_week", "cases"
)])
```

The returned source labels must first be resolved to district `geo_id`s and,
where necessary, aggregated to the analysis geography. Incidence can then be
prepared, linked to `clusters`, assigned to epidemiological periods, and
summarized by demographic type and time. The required geography resources and
validation rules are described in the architecture and data-contract
documentation. This workflow can be used directly from R without the Shiny
application.

## Shiny application

The package can be used directly from R. The Shiny application is an optional
interactive interface built on the package's analytical workflows; some of its
data-retrieval coordination and visualization preparation remain
application-internal.

Install the optional `shiny` and `leaflet` packages, then launch the installed
application with:

```r
regionalepi::run_regionalepi_app()
```

The application supports exploration of historical and updated demographic
typologies, cluster profiles and stability, district distributions and maps,
weekly infectious-disease activity, Kreis × Woche incidence, national and
regional comparisons, and individual districts.

SurvStat observations are retrieved live after the user starts an analysis.
Demographic indicators for the typology and official annual-average population
use the bundled snapshot by default. Live retrieval of the typology data and
annual-average population from the Regionaldatenbank is optional and requires
configured credentials. SurvStat
observations remain live in either mode. Results are cached only for the current
session, and source responses are not persisted. Changing tabs, selecting a
district, or zooming and moving the map does not itself trigger another source
retrieval.

Regional comparisons include the four Großregionen used by the RKI
Arbeitsgemeinschaft Influenza (AGI), the documented 12-group Länder
aggregation, and all 16 Länder. These groups provide descriptive comparisons
of district incidence; they are not separate source geographies or official
regional incidence measures. The AGI grouping is documented at
<https://influenza.rki.de/Glossar.aspx>.

## Methodological notes

For current dynamic analyses, weekly district incidence is calculated as:

```text
reported cases / official annual-average population × 100,000
```

A derived incidence value is considered final when the official annual-average
population for the same reporting year is available. If only the immediately
preceding year's official population is available, the derived incidence is
marked provisional; older denominators are rejected. The reporting and
denominator years remain visible with the result. Running observation windows
are limited by an explicit analysis cutoff, which remains separate from the
SurvStat source-data status.

Missing and zero observations are kept distinct. In reviewed complete local
SurvStat case-count exports, empty cells are interpreted as zero reported cases
only when the complete source matrix reconciles exactly with its totals; their
derived incidence is therefore zero. This is `regionalepi`'s documented
interpretation of the export structure, not an explicit statement by the RKI.
Null case values returned by the API and genuinely missing observations remain
missing (`NA`).

Weekly typology summaries use the unweighted median of district incidence.
Their empirical first and third quartiles describe the observed district
distribution and are not confidence intervals. The package does not calculate
a pooled or population-weighted cluster incidence.

Where Berlin source observations occur at a finer spatial resolution, they are
handled through documented aggregation or replacement rules before analysis.
Epidemiological periods are supplied as reviewed date intervals rather than
being inferred from the observed incidence curves.

## Documentation

- `?regionalepi` introduces the scientific workflows, incidence definitions,
  data sources, and executable package examples.
- [`docs/architecture.md`](docs/architecture.md) describes processing order,
  geography handling, source adapters, and the Shiny/backend separation.
- [`docs/data-contracts.md`](docs/data-contracts.md) documents fields,
  validation rules, missing-value policies, and versioned specifications.
- [`docs/regionaldatenbank.md`](docs/regionaldatenbank.md) describes the
  Regionaldatenbank tables, credentials, population bases, and attribution.
- `?fetch_survstat_cases`, `?fetch_survstat_incidence`, and the
  `?fetch_regional_*` help pages document live data retrieval.

## Citation and licenses

Use `citation("regionalepi")` for the package citation. The regionalepi source
code is licensed under GPL-3.

SurvStat observations remain subject to the RKI data-usage conditions and
source-attribution requirements. Regionaldatenbank data retain the applicable
Datenlizenz Deutschland – Namensnennung – Version 2.0. BKG resources retain
their respective source terms. See `inst/NOTICE` and the source provenance
recorded by the package for details. GPL-3 does not relicense these third-party
data or resources.
