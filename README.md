
# PatientProfiles <img src="man/figures/logo.png" align="right" height="200"/>

[![CRAN
status](https://www.r-pkg.org/badges/version/PatientProfiles)](https://CRAN.R-project.org/package=PatientProfiles)
[![codecov.io](https://codecov.io/github/darwin-eu-dev/PatientProfiles/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu-dev/PatientProfiles?branch=main)
[![R-CMD-check](https://github.com/darwin-eu-dev/PatientProfiles/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu-dev/PatientProfiles/actions)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/PatientProfiles)](https://cran.r-project.org/package=PatientProfiles)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/PatientProfiles)](https://cran.r-project.org/package=PatientProfiles)

## Package overview

PatientProfiles contains functions for adding characteristics to OMOP
CDM tables containing patient level data (e.g. condition occurrence,
drug exposure, and so on) and OMOP CDM cohort tables. The
characteristics that can be added include an individual´s sex, age, and
days of prior observation Time varying characteristics, such as age, can
be estimated relative to any date in the corresponding table. In
addition, PatientProfiles also provides functionality for identifying
intersections between a cohort table and OMOP CDM tables containing
patient level data or other cohort tables.

## Package installation

You can install the latest version of PatientProfiles like so:

``` r
install.packages("PatientProfiles")
```

## Example usage

### Create a reference to data in the OMOP CDM format

The PatientProfiles package is designed to work with data in the OMOP
CDM format, so our first step is to create a reference to the data using
the CDMConnector package.

``` r
library(CDMConnector)
library(PatientProfiles)
library(dplyr)
```

Creating a connection to a Postgres database would for example look
like:

``` r
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
  host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
  user = Sys.getenv("CDM5_POSTGRESQL_USER"),
  password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
)

cdm <- cdm_from_con(
  con,
  cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
  write_schema = Sys.getenv("CDM5_POSTGRESQL_RESULT_SCHEMA")
)
```

To see how you would create a reference to your database please consult
the CDMConnector package
[documentation](https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html).
For this example though we’ll work with simulated data, and we’ll
generate an example cdm reference like so:

``` r
cdm <- mockPatientProfiles(numberIndividuals = 1000)
```

### Adding individuals´ characteristics

#### Adding characteristics to patient-level data

Say we wanted to get individuals´sex and age at condition start date for
records in the condition occurrence table. We can use the `addAge` and
`addSex` functions to do this:

``` r
cdm$condition_occurrence %>%
  glimpse()
#> Rows: ??
#> Columns: 6
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ person_id                 <int> 322, 848, 164, 947, 854, 952, 142, 885, 993,…
#> $ condition_start_date      <date> 2053-12-28, 1941-08-12, 1999-01-04, 2090-08…
#> $ condition_end_date        <date> 2058-12-22, 2043-11-26, 2007-08-01, 2097-07…
#> $ condition_occurrence_id   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1…
#> $ condition_concept_id      <int> 1, 5, 4, 4, 5, 5, 5, 9, 5, 4, 9, 9, 10, 8, 4…
#> $ condition_type_concept_id <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…

cdm$condition_occurrence <- cdm$condition_occurrence %>%
  addAge(indexDate = "condition_start_date") %>%
  addSex()

cdm$condition_occurrence %>%
  glimpse()
#> Rows: ??
#> Columns: 8
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ person_id                 <int> 50, 115, 127, 142, 164, 186, 193, 200, 201, …
#> $ condition_start_date      <date> 1969-11-13, 2050-05-15, 2073-10-08, 1940-11…
#> $ condition_end_date        <date> 1971-01-14, 2064-06-24, 2089-05-15, 1964-12…
#> $ condition_occurrence_id   <int> 18, 11, 32, 7, 3, 15, 17, 34, 22, 14, 12, 37…
#> $ condition_concept_id      <int> 1, 9, 6, 5, 4, 4, 8, 5, 2, 8, 9, 10, 1, 10, …
#> $ condition_type_concept_id <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ age                       <int> 35, 111, 125, 7, 12, 78, 58, 54, 20, 42, 88,…
#> $ sex                       <chr> "Female", "Female", "Female", "Female", "Fem…
```

We could, for example, then limit our data to only males aged between 18
and 65

``` r
cdm$condition_occurrence %>%
  filter(age >= 18 & age <= 65) %>%
  filter(sex == "Male")
#> # Source:   SQL [9 x 8]
#> # Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#>   person_id condition_start_date condition_end_date condition_occurrence_id
#>       <int> <date>               <date>                               <int>
#> 1       193 2045-09-13           2071-06-16                              17
#> 2       202 2005-09-29           2076-09-19                              14
#> 3       655 1956-11-22           1957-09-27                              25
#> 4       670 2006-01-18           2042-12-04                              13
#> 5       672 1992-04-13           2048-11-16                              23
#> 6       742 1963-08-12           2007-01-26                              31
#> 7       852 2035-01-15           2040-04-06                              20
#> 8       948 1944-12-22           1963-01-19                              26
#> 9       993 1961-01-29           1975-12-19                               9
#> # ℹ 4 more variables: condition_concept_id <int>,
#> #   condition_type_concept_id <int>, age <int>, sex <chr>
```

#### Adding characteristics of a cohort

As with other tables in the OMOP CDM, we can work in a similar way with
cohort tables. For example, say we have the below cohort table

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 1, 2, 1, 1, 1, 1, 2, 1, 3, 2, 2, 1, 1, 2, 3, 1, 2…
#> $ subject_id           <int> 804, 115, 126, 721, 245, 28, 344, 208, 955, 934, …
#> $ cohort_start_date    <date> 1993-06-01, 2011-07-24, 2050-10-09, 1953-12-03, …
#> $ cohort_end_date      <date> 1994-11-30, 2057-07-10, 2091-11-09, 2002-01-01, …
```

We can add age, age groups, sex, and days of prior observation to a
cohort like so

``` r
cdm$cohort1 <- cdm$cohort1 %>%
  addAge(
    indexDate = "cohort_start_date",
    ageGroup = list(c(0, 18), c(19, 65), c(66, 100))
  ) %>%
  addSex() %>%
  addPriorObservation()

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 8
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 1, 1, 1, 2, 2, 1, 2, 3, 1, 2, 1, 2, 2, 1, 3, 2, 1…
#> $ subject_id           <int> 804, 126, 245, 934, 197, 998, 778, 564, 123, 873,…
#> $ cohort_start_date    <date> 1993-06-01, 2050-10-09, 1999-05-15, 2058-01-04, …
#> $ cohort_end_date      <date> 1994-11-30, 2091-11-09, 2081-08-11, 2074-07-20, …
#> $ age                  <int> 15, 32, 66, 134, 28, 8, 25, 82, 64, 12, 82, 88, 6…
#> $ age_group            <chr> "0 to 18", "19 to 65", "66 to 100", "None", "19 t…
#> $ sex                  <chr> "Female", "Female", "Male", "Male", "Male", "Male…
#> $ prior_observation    <int> 5630, 11969, 24240, 48947, 10586, 2926, 9260, 302…
```

We could use this information to subset the cohort. For example limiting
to those with at least 365 days of prior observation available before
their cohort start date like so

``` r
cdm$cohort1 %>%
  filter(prior_observation >= 365)
#> # Source:   SQL [?? x 8]
#> # Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date   age
#>                   <int>      <int> <date>            <date>          <int>
#>  1                    1        804 1993-06-01        1994-11-30         15
#>  2                    1        126 2050-10-09        2091-11-09         32
#>  3                    1        245 1999-05-15        2081-08-11         66
#>  4                    2        934 2058-01-04        2074-07-20        134
#>  5                    2        197 2001-12-26        2076-01-24         28
#>  6                    1        998 2006-01-05        2055-12-26          8
#>  7                    2        778 2025-05-09        2028-02-20         25
#>  8                    3        564 2072-11-30        2077-01-14         82
#>  9                    1        123 2039-06-27        2095-01-22         64
#> 10                    2        873 2022-11-07        2029-06-17         12
#> # ℹ more rows
#> # ℹ 3 more variables: age_group <chr>, sex <chr>, prior_observation <int>
```

### Cohort intersections

#### Detect the presence of another cohort in a certain window

We can use `addCohortIntersectFlag` to add a flag for the presence (or
not) of a cohort in a certain window.

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 1, 1, 2, 3, 3, 1, 1, 3, 2, 3
#> $ subject_id           <int> 1, 10, 2, 4, 5, 6, 8, 7, 3, 9
#> $ cohort_start_date    <date> 2035-11-04, 1921-03-20, 1975-02-05, 1948-03-01, 1…
#> $ cohort_end_date      <date> 2056-06-27, 1924-08-01, 2013-10-17, 2041-05-23, 2…

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectFlag(
    targetCohortTable = "cohort2",
    window = c(-Inf, -1)
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 7
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 2, 1, 2, 1, 1, 3, 3, 1, 3, 3
#> $ subject_id           <int> 2, 6, 3, 1, 10, 4, 5, 8, 7, 9
#> $ cohort_start_date    <date> 1975-02-05, 1994-09-25, 2166-05-03, 2035-11-04, 1…
#> $ cohort_end_date      <date> 2013-10-17, 2019-06-13, 2166-05-24, 2056-06-27, 1…
#> $ cohort_3_minf_to_m1  <dbl> 0, 1, 1, 0, 0, 0, 0, 0, 0, 0
#> $ cohort_2_minf_to_m1  <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0
#> $ cohort_1_minf_to_m1  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
```

#### Count appearances of a certain cohort in a certain window

If we wanted the number of appearances, we could instead use the
`addCohortIntersectCount` function

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 2, 2, 1, 3, 1, 3, 1, 1, 1, 2
#> $ subject_id           <int> 4, 1, 3, 9, 10, 8, 5, 7, 2, 6
#> $ cohort_start_date    <date> 1968-11-28, 1924-02-01, 2066-06-12, 2037-06-11, 2…
#> $ cohort_end_date      <date> 2039-05-07, 1933-07-10, 2067-11-30, 2052-11-05, 2…

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectCount(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    window = list("short_term" = c(1, 30), "mid_term" = c(31, 180))
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 6
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 2, 1, 1, 2, 3, 1, 3, 1, 1, 2
#> $ subject_id           <int> 1, 3, 2, 4, 9, 10, 8, 5, 7, 6
#> $ cohort_start_date    <date> 1924-02-01, 2066-06-12, 2073-01-11, 1968-11-28, 2…
#> $ cohort_end_date      <date> 1933-07-10, 2067-11-30, 2096-01-10, 2039-05-07, 2…
#> $ cohort_1_short_term  <dbl> 0, 1, 1, 0, 0, 0, 0, 0, 0, 0
#> $ cohort_1_mid_term    <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0
```

#### Add a column with the first/last event in a certain window

Say we wanted the date at which an individual was in another cohort then
we can use the `addCohortIntersectDate` function. As there might be
multiple records for the other cohort, we can also choose the first or
the last appearance in that cohort.

First occurrence:

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 2, 1, 3, 1, 1, 1, 2, 3, 1, 2
#> $ subject_id           <int> 6, 3, 1, 9, 8, 5, 2, 4, 7, 10
#> $ cohort_start_date    <date> 2046-09-01, 2169-07-04, 2019-06-13, 1985-03-23, 1…
#> $ cohort_end_date      <date> 2056-05-09, 2184-02-17, 2058-06-12, 2001-10-24, 2…

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectDate(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    order = "first",
    window = c(-Inf, Inf)
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 5
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 1, 2, 2, 1, 3, 1, 1, 2, 3, 1
#> $ subject_id           <int> 5, 10, 6, 3, 1, 9, 8, 2, 4, 7
#> $ cohort_start_date    <date> 2018-09-09, 2030-11-01, 2046-09-01, 2169-07-04, 2…
#> $ cohort_end_date      <date> 2045-01-19, 2037-02-15, 2056-05-09, 2184-02-17, 2…
#> $ cohort_1_minf_to_inf <date> 2026-11-03, 1996-02-09, NA, NA, NA, NA, NA, NA, …
```

Last occurrence:

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 1, 3, 1, 1, 3, 1, 2, 3, 3, 1
#> $ subject_id           <int> 2, 7, 5, 10, 8, 4, 1, 6, 3, 9
#> $ cohort_start_date    <date> 1915-03-10, 2016-03-25, 1991-09-12, 2089-03-29, 2…
#> $ cohort_end_date      <date> 2021-09-16, 2033-01-04, 2002-10-07, 2168-04-02, 2…

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectDate(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    order = "last",
    window = c(-Inf, Inf)
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 5
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 1, 1, 3, 1, 3, 1, 2, 3, 3, 1
#> $ subject_id           <int> 2, 10, 7, 5, 8, 4, 1, 6, 3, 9
#> $ cohort_start_date    <date> 1915-03-10, 2089-03-29, 2016-03-25, 1991-09-12, 2…
#> $ cohort_end_date      <date> 2021-09-16, 2168-04-02, 2033-01-04, 2002-10-07, 2…
#> $ cohort_1_minf_to_inf <date> 1920-09-18, 2121-03-29, NA, NA, NA, NA, NA, NA, …
```

#### Add the number of days instead of the date

Instead of returning a date, we could return the days to the
intersection by using `addCohortIntersectDays`

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 1, 2, 1, 1, 1, 1, 3, 2, 2, 1
#> $ subject_id           <int> 8, 2, 4, 10, 9, 3, 7, 1, 6, 5
#> $ cohort_start_date    <date> 1995-03-13, 2035-08-18, 2007-06-02, 2124-02-18, 1…
#> $ cohort_end_date      <date> 2056-08-10, 2047-02-17, 2009-07-10, 2138-06-19, 1…

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectDays(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    order = "last",
    window = c(-Inf, Inf)
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 5
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 1, 3, 1, 1, 2, 1, 1, 1, 2, 2
#> $ subject_id           <int> 10, 7, 5, 8, 2, 4, 9, 3, 1, 6
#> $ cohort_start_date    <date> 2124-02-18, 1947-08-05, 2072-02-15, 1995-03-13, 2…
#> $ cohort_end_date      <date> 2138-06-19, 2076-10-20, 2073-01-11, 2056-08-10, 2…
#> $ cohort_1_minf_to_inf <dbl> -1103, 35968, -43877, NA, NA, NA, NA, NA, NA, NA
```

#### Combine multiple cohort intersects

If we want to combine multiple cohort intersects we can concatenate the
operations using the `pipe` operator:

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 1, 3, 3, 3, 3, 3, 2, 1, 1, 3
#> $ subject_id           <int> 2, 9, 1, 10, 8, 6, 7, 3, 4, 5
#> $ cohort_start_date    <date> 1926-12-03, 2027-10-08, 1956-01-06, 1987-07-06, 2…
#> $ cohort_end_date      <date> 1931-08-23, 2055-08-04, 1965-02-25, 2032-09-30, 2…

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectDate(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    order = "last",
    window = c(-Inf, Inf)
  ) %>%
  addCohortIntersectCount(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    window = c(-Inf, Inf)
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 5
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 1, 3, 2, 1, 3, 3, 3, 3, 1, 3
#> $ subject_id           <int> 2, 9, 7, 3, 1, 10, 8, 6, 4, 5
#> $ cohort_start_date    <date> 1926-12-03, 2027-10-08, 1937-04-07, 1982-01-06, 1…
#> $ cohort_end_date      <date> 1931-08-23, 2055-08-04, 1963-01-28, 2000-03-26, 1…
#> $ cohort_1_minf_to_inf <dbl> 1, 1, 1, 1, 0, 0, 0, 0, 0, 0
```

``` r
mockDisconnect(cdm)
```
