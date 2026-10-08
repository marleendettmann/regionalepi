# Regionaldatenbank Deutschland demographic-data adapters v0.1

The source database is Regionaldatenbank Deutschland, provided by the
Statistische Ämter des Bundes und der Länder.

The year-end population adapter uses Regionaldatenbank Deutschland table
`12411-01-01-4`, statistic `12411`, *Bevölkerung nach Geschlecht - Stichtag
31.12. - regionale Tiefe: Kreise und kreisfreie Städte*. The adapter retrieves
only total population (`Insgesamt`, `Bevölkerungsstand`, unit `Anzahl`) for the
approved reference dates 2017-12-31 through 2025-12-31.

`fetch_regional_average_population()` is a separate narrow adapter for table
`12411-05-01-4`, statistic `12411`, measure `BEV028`, sex `Insgesamt`, and
reporting years from 2017. It returns official reporting-year annual-average
population under its own validated contract. Provenance retains the table,
measure, year, Census population basis, retrieval time, data status, source
notes, and attribution. This adapter does not calculate incidence and does not
reinterpret a 31 December population value.

The demographic-data adapters additionally retrieve source-provided mean
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
der Länder under Datenlizenz Deutschland – Namensnennung – Version 2.0
(`dl-de/by-2-0`). These
source-data terms remain distinct from the GPL-3 license of the regionalepi
package code; GPL-3 does not relicense the Regionaldatenbank data.

The source copyright and license text is retained in result-level provenance.
Retrieval time records when regionalepi executed the request; source data
status records the database state reported for that response; observation
reference date or year identifies the period represented by the value. These
three concepts remain distinct. Observation dates do not independently
establish a canonical territorial register vintage, so adapter-produced
`geo_vintage` remains unresolved.

The installed demographic snapshot contains reviewed observations from tables
`12411-07-01-4`, `12411-08-01-4`, `12411-01-01-4`, `11111-01-01-4`, and
`12411-05-01-4`. The last component is the distinct official annual-average
population for reporting years 2017--2025. Values for 2017--2021 use the
Census 2011 progression basis; values from 2022 use the Census 2022 progression
basis. This is a change in the population-estimation basis, not in the incidence
formula. The 2017--2020 additive denominator is harmonized through the reviewed
Eisenach-to-Wartburgkreis relation before incidence is calculated; the source
already provides the current 400-district set in 2021. Optional live retrieval
uses the same narrow adapters and source definitions. Population density and
incidence remain ordinary package derivations.
