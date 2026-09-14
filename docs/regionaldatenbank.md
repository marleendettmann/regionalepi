# Regionaldatenbank Deutschland source adapters v0.1

The year-end population adapter uses Regionaldatenbank Deutschland table
`12411-01-01-4`, statistic `12411`, *Bevölkerung nach Geschlecht - Stichtag
31.12. - regionale Tiefe: Kreise und kreisfreie Städte*. The adapter retrieves
only total population (`Insgesamt`, `Bevölkerungsstand`, unit `Anzahl`) for the
approved reference dates 2017-12-31 through 2025-12-31.

`fetch_regional_average_population()` is a separate narrow adapter for table
`12411-05-01-4`, statistic `12411`, measure `BEV028`, sex `Insgesamt`, and
reporting years 2022--2025. It returns official reporting-year annual-average
population under its own validated contract. Provenance retains the table,
measure, year, Census population basis, retrieval time, data status, source
notes, and attribution. This adapter does not calculate incidence and does not
reinterpret a 31 December population value.

The demographic-context adapters additionally retrieve source-provided mean
age (`BEV519`, table `12411-07-01-4`), source-provided youth dependency
quotient (`BEV216`, table `12411-08-01-4`), and district area (`FLC006`, table
`11111-01-01-4`). `derive_population_density()` joins compatible year-end
population and area by exact character district identifier and reference date
and divides without rounding. An internal age-population request is used only
for validation of the authoritative youth quotient.

The official REST endpoint family is
`https://www.regionalstatistik.de/genesisws/rest/2020/`. The implementation
uses the authenticated `data/table` operation and its table response. It checks
the API status, operation and table identity, and `Structure` codes `12411`,
`KREISE`, `STAG`, `GES`, and `BEVSTD` before parsing `Object.Content`.
The source's `-` marker denotes no observation for a displayed historical
region/date combination. An unrestricted retrieval omits those combinations
and records the marker in diagnostics; a missing value for an explicitly
requested region is an error. Blank, malformed, and other marked population
cells also fail explicitly.

Credentials must be available as `REGIONALSTATISTIK_USER` and
`REGIONALSTATISTIK_PASSWORD`. They are read as ordinary environment variables;
the package neither reads `.env` nor exposes credential arguments. No
credential or authenticated request object is returned or persisted.

AGS values and names are preserved from the source. Names are source metadata,
not canonical BKG authority. `reference_date` records the population stock
date; `geo_vintage` remains `NA_Date_` because the table does not independently
establish the canonical territorial register vintage.

The source documentation distinguishes the population basis used through 2021
from the Census 2022 basis used from 2022. The adapter records `census_2011` for
2017-2021 and `census_2022` for 2022-2025, and retains the methodological break
once in result-level provenance.

The API response attributes the data to the Statistische Ämter des Bundes und
der Länder under Datenlizenz Deutschland - Namensnennung - Version 2.0. Runtime
source-data licensing is distinct from the package-code license, which remains
`TBD`.

The source copyright and license text is retained in result-level provenance.
The installed demographic snapshot contains reviewed observations from tables
`12411-07-01-4`, `12411-08-01-4`, `12411-01-01-4`, and `11111-01-01-4`;
runtime annual-average population retrieval uses `12411-05-01-4`.
