
# PatientProfiles <img src="man/figures/logo.png" align="right" height="200"/>

[![CRAN
status](https://www.r-pkg.org/badges/version/PatientProfiles)](https://CRAN.R-project.org/package=PatientProfiles)
[![codecov.io](https://codecov.io/github/darwin-eu-dev/PatientProfiles/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu-dev/PatientProfiles?branch=main)
[![R-CMD-check](https://github.com/darwin-eu-dev/PatientProfiles/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu-dev/PatientProfiles/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) -->
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
#> $ person_id                 <int> 708, 792, 501, 560, 314, 203, 922, 504, 483,…
#> $ condition_start_date      <date> 1941-10-11, 1945-07-07, 1977-01-15, 1937-02…
#> $ condition_end_date        <date> 1949-03-09, 1951-05-14, 2040-08-29, 1937-08…
#> $ condition_occurrence_id   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1…
#> $ condition_concept_id      <int> 2, 7, 2, 8, 6, 1, 10, 7, 7, 6, 8, 6, 4, 6, 9…
#> $ condition_type_concept_id <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…

cdm$condition_occurrence <- cdm$condition_occurrence %>%
  addAge(indexDate = "condition_start_date") %>%
  addSex()

cdm$condition_occurrence %>%
  glimpse()
#> Rows: ??
#> Columns: 8
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ person_id                 <int> 708, 792, 501, 560, 314, 203, 922, 504, 483,…
#> $ condition_start_date      <date> 1941-10-11, 1945-07-07, 1977-01-15, 1937-02…
#> $ condition_end_date        <date> 1949-03-09, 1951-05-14, 2040-08-29, 1937-08…
#> $ condition_occurrence_id   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 13, 14, 15, 1…
#> $ condition_concept_id      <int> 2, 7, 2, 8, 6, 1, 10, 7, 7, 6, 4, 6, 9, 9, 5…
#> $ condition_type_concept_id <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ age                       <dbl> 22, 11, 18, -28, 20, 15, -39, 55, -8, 15, -5…
#> $ sex                       <chr> "Female", "Female", "Male", "Female", "Femal…
```

We could, for example, then limit our data to only males aged between 18
and 65

``` r
cdm$condition_occurrence %>%
  filter(age >= 18 & age <= 65) %>%
  filter(sex == "Male")
#> # Source:   SQL [?? x 8]
#> # Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#>    person_id condition_start_date condition_end_date condition_occurrence_id
#>        <int> <date>               <date>                               <int>
#>  1       501 1977-01-15           2040-08-29                               3
#>  2       504 1978-08-06           2033-03-27                               8
#>  3       808 2018-06-22           2018-11-04                              28
#>  4       517 1987-07-21           2097-04-13                              33
#>  5       376 1946-04-13           1975-01-20                              37
#>  6       787 2020-09-23           2029-04-20                              45
#>  7       296 2069-10-09           2070-10-03                              50
#>  8       296 2049-12-20           2067-03-13                              79
#>  9       665 1972-03-02           1986-11-08                              86
#> 10       978 1949-08-18           1955-03-31                              87
#> # ℹ more rows
#> # ℹ 4 more variables: condition_concept_id <int>,
#> #   condition_type_concept_id <int>, age <dbl>, sex <chr>
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
#> $ cohort_definition_id <int> 1, 2, 1, 2, 3, 2, 2, 1, 2, 3, 2, 3, 3, 3, 1, 1, 2…
#> $ subject_id           <int> 683, 760, 84, 279, 981, 833, 576, 311, 105, 873, …
#> $ cohort_start_date    <date> 1949-03-31, 2008-12-15, 1977-09-03, 1956-09-05, …
#> $ cohort_end_date      <date> 1957-08-23, 2010-09-04, 2017-05-17, 2025-09-03, …
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
#> $ cohort_definition_id <int> 1, 2, 1, 2, 3, 3, 3, 1, 2, 2, 1, 1, 1, 1, 1, 2, 1…
#> $ subject_id           <int> 84, 279, 311, 105, 275, 398, 483, 380, 191, 783, …
#> $ cohort_start_date    <date> 1977-09-03, 1956-09-05, 1968-11-25, 1969-09-09, …
#> $ cohort_end_date      <date> 2017-05-17, 2025-09-03, 2075-10-04, 1981-08-16, …
#> $ age                  <dbl> -28, 44, 52, 52, 47, 1, 34, 98, 7, 24, -29, -62, …
#> $ age_group            <chr> "None", "19 to 65", "19 to 65", "19 to 65", "19 t…
#> $ sex                  <chr> "Female", "Male", "Female", "Male", "Male", "Fema…
#> $ prior_observation    <dbl> 6112, 16319, 19322, 19244, 17418, 618, 23225, 359…
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
#>                   <int>      <int> <date>            <date>          <dbl>
#>  1                    1         84 1977-09-03        2017-05-17        -28
#>  2                    2        279 1956-09-05        2025-09-03         44
#>  3                    1        311 1968-11-25        2075-10-04         52
#>  4                    2        105 1969-09-09        1981-08-16         52
#>  5                    3        275 1986-09-09        1990-05-11         47
#>  6                    3        398 1916-09-10        1946-10-27          1
#>  7                    3        483 2000-06-04        2020-09-14         34
#>  8                    1        380 2064-05-24        2071-03-13         98
#>  9                    2        191 1932-03-23        2037-02-20          7
#> 10                    2        783 2040-09-25        2042-01-27         24
#> # ℹ more rows
#> # ℹ 3 more variables: age_group <chr>, sex <chr>, prior_observation <dbl>
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
#> $ cohort_definition_id <int> 1, 1, 1, 2, 3, 3, 1, 2, 3, 2
#> $ subject_id           <int> 4, 1, 7, 10, 8, 6, 2, 3, 9, 5
#> $ cohort_start_date    <date> 2053-02-20, 1953-12-20, 1911-10-31, 2042-10-17, 1…
#> $ cohort_end_date      <date> 2057-04-01, 1959-02-19, 1953-02-10, 2063-12-31, 1…

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
#> $ cohort_definition_id <int> 1, 1, 2, 3, 3, 2, 1, 1, 3, 2
#> $ subject_id           <int> 4, 1, 10, 8, 6, 3, 7, 2, 9, 5
#> $ cohort_start_date    <date> 2053-02-20, 1953-12-20, 2042-10-17, 1938-05-20, 2…
#> $ cohort_end_date      <date> 2057-04-01, 1959-02-19, 2063-12-31, 1938-07-29, 2…
#> $ cohort_3_minf_to_m1  <dbl> 1, 0, 0, 0, 1, 0, 0, 0, 0, 0
#> $ cohort_2_minf_to_m1  <dbl> 0, 1, 1, 0, 0, 0, 0, 0, 0, 0
#> $ cohort_1_minf_to_m1  <dbl> 0, 0, 0, 1, 0, 1, 0, 0, 0, 0
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
#> $ cohort_definition_id <int> 2, 3, 1, 3, 3, 2, 2, 2, 2, 2
#> $ subject_id           <int> 2, 6, 4, 8, 1, 9, 7, 3, 5, 10
#> $ cohort_start_date    <date> 2008-12-02, 1928-01-08, 1931-04-05, 1964-12-19, 1…
#> $ cohort_end_date      <date> 2016-05-08, 1929-02-28, 1955-11-14, 1983-12-17, 1…

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
#> $ cohort_definition_id <int> 2, 3, 1, 3, 3, 2, 2, 2, 2, 2
#> $ subject_id           <int> 2, 6, 4, 8, 1, 9, 7, 3, 5, 10
#> $ cohort_start_date    <date> 2008-12-02, 1928-01-08, 1931-04-05, 1964-12-19, 1…
#> $ cohort_end_date      <date> 2016-05-08, 1929-02-28, 1955-11-14, 1983-12-17, 1…
#> $ cohort_1_short_term  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#> $ cohort_1_mid_term    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
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
#> $ cohort_definition_id <int> 1, 2, 2, 1, 2, 3, 2, 3, 3, 3
#> $ subject_id           <int> 5, 9, 4, 1, 8, 6, 7, 3, 10, 2
#> $ cohort_start_date    <date> 2031-12-23, 1995-12-08, 2014-07-08, 1994-06-28, 1…
#> $ cohort_end_date      <date> 2055-02-03, 2028-07-07, 2035-03-06, 1995-01-23, 1…

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
#> $ cohort_definition_id <int> 1, 2, 2, 2, 3, 3, 1, 3, 2, 3
#> $ subject_id           <int> 5, 9, 4, 8, 3, 10, 1, 6, 7, 2
#> $ cohort_start_date    <date> 2031-12-23, 1995-12-08, 2014-07-08, 1979-10-09, 1…
#> $ cohort_end_date      <date> 2055-02-03, 2028-07-07, 2035-03-06, 1991-11-29, 1…
#> $ cohort_1_minf_to_inf <date> 2045-01-03, 2030-01-01, 1948-02-01, 1981-11-19, …
```

Last occurrence:

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 3, 3, 3, 2, 3, 1, 1, 3, 2, 1
#> $ subject_id           <int> 2, 4, 9, 5, 1, 7, 3, 10, 8, 6
#> $ cohort_start_date    <date> 1929-02-12, 1927-10-23, 1929-10-04, 1988-09-10, 1…
#> $ cohort_end_date      <date> 1933-08-10, 1943-10-06, 1947-05-11, 2033-03-21, 1…

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
#> $ cohort_definition_id <int> 3, 1, 1, 3, 3, 3, 2, 1, 3, 2
#> $ subject_id           <int> 1, 3, 6, 2, 4, 9, 5, 7, 10, 8
#> $ cohort_start_date    <date> 1924-06-08, 1995-02-24, 2030-06-02, 1929-02-12, 1…
#> $ cohort_end_date      <date> 1932-06-29, 1999-06-11, 2036-07-30, 1933-08-10, 1…
#> $ cohort_1_minf_to_inf <date> 1963-06-01, 1971-11-26, 1999-07-05, NA, NA, NA, …
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
#> $ cohort_definition_id <int> 1, 3, 1, 2, 3, 2, 2, 3, 3, 2
#> $ subject_id           <int> 10, 9, 3, 5, 7, 4, 6, 8, 2, 1
#> $ cohort_start_date    <date> 1949-12-09, 1943-09-19, 2018-05-02, 1945-06-29, 2…
#> $ cohort_end_date      <date> 1950-01-29, 1986-12-01, 2030-11-05, 1977-01-13, 2…

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
#> $ cohort_definition_id <int> 3, 3, 2, 2, 3, 1, 1, 2, 3, 2
#> $ subject_id           <int> 9, 7, 4, 6, 2, 10, 3, 5, 8, 1
#> $ cohort_start_date    <date> 1943-09-19, 2049-09-22, 1971-06-01, 1946-03-23, 1…
#> $ cohort_end_date      <date> 1986-12-01, 2053-01-14, 1975-04-14, 1952-01-11, 1…
#> $ cohort_1_minf_to_inf <dbl> 4218, -1565, -1035, 393, 16653, NA, NA, NA, NA, NA
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
#> $ cohort_definition_id <int> 3, 1, 3, 3, 1, 3, 2, 2, 3, 3
#> $ subject_id           <int> 2, 8, 9, 3, 7, 1, 5, 10, 4, 6
#> $ cohort_start_date    <date> 1958-06-17, 2000-09-29, 2001-08-19, 1989-02-07, 1…
#> $ cohort_end_date      <date> 1963-09-07, 2009-12-27, 2039-02-06, 1994-07-16, 1…

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
#> $ cohort_definition_id <int> 2, 3, 3, 1, 3, 3, 1, 3, 2, 3
#> $ subject_id           <int> 10, 4, 2, 8, 9, 3, 7, 1, 5, 6
#> $ cohort_start_date    <date> 1946-05-11, 1946-11-21, 1958-06-17, 2000-09-29, 2…
#> $ cohort_end_date      <date> 1985-05-15, 1947-01-03, 1963-09-07, 2009-12-27, 2…
#> $ cohort_1_minf_to_inf <dbl> 1, 1, 0, 0, 0, 0, 0, 0, 0, 0
```

``` r
mockDisconnect(cdm)
```
