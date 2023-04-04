
# PatientProfiles <img src='man/figures/Hex.png' align="right" height="139"/>

[![CRAN
status](https://www.r-pkg.org/badges/version/PatientProfiles)](https://CRAN.R-project.org/package=PatientProfiles)
[![codecov.io](https://codecov.io/github/darwin-eu/PatientProfiles/coverage.svg?branch=main)](https://codecov.io/github/darwin-eu/PatientProfiles?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/PatientProfiles/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/PatientProfiles/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://www.tidyverse.org/lifecycle/#experimental)

## Package overview

PatientProfiles contains functions for adding characteristics to OMOP
CDM tables containing patient level data (e.g. condition_occurrence,
drug_exposure, and so on) and OMOP CDM cohort tables. The
characteristics that can be added include an individual´s sex, age
(relative to a date in the table), and days of prior history. Time
varying characteristics, such as age, can be estimated relative to any
dates in the corresponding table. In additition, PatientProfiles also
provides functionality for identifying intersections between a cohort
table and OMOP CDM tables containing patient level data or other cohort
tables.

## Package installation

You can install the latest version of PatientProfiles like so:

``` r
install.packages("remotes")
remotes::install_github("oxford-pharmacoepi/PatientProfiles")
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
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
                      host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
                      user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                      password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))

cdm <- CDMConnector::cdm_from_con(con, 
                    cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"), 
                    write_schema = Sys.getenv("CDM5_POSTGRESQL_RESULT_SCHEMA"))
```

To see how you would create a reference to your database please consult
the CDMConnector package documentation. For this example though we´ll
work with simulated data, and we’ll generate an example cdm reference
like so:

``` r
cdm <- mockPatientProfiles(patient_size = 1000, drug_exposure_size = 1000)
```

### Adding individuals´ chartacteristics

#### Adding chartacteristics to patient-level data

Say we wanted to get individuals´sex and age at condition start date for
records in the condition_occurrence table. We can use the `addAge` and
`addSex` functions to do this:

``` r
cdm$condition_occurrence %>% 
  glimpse()
#> Rows: ??
#> Columns: 5
#> Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-139-generic:R 4.1.2/:memory:]
#> $ condition_occurrence_id <int> 314, 970, 349, 234, 193, 919, 113, 277, 491, 9…
#> $ person_id               <int> 314, 970, 349, 234, 193, 919, 113, 277, 491, 9…
#> $ condition_concept_id    <int> 4, 5, 1, 2, 3, 4, 3, 2, 1, 2, 1, 5, 3, 4, 4, 1…
#> $ condition_start_date    <date> 2005-08-25, 2007-02-15, 2009-02-15, 2008-05-0…
#> $ condition_end_date      <date> 2006-06-14, 2007-06-05, 2011-06-24, 2009-01-0…

cdm$condition_occurrence <- cdm$condition_occurrence  %>% 
  addAge(cdm = cdm, indexDate = "condition_start_date")  %>% 
  addSex(cdm)

cdm$condition_occurrence %>% 
  glimpse()
#> Rows: ??
#> Columns: 7
#> Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-139-generic:R 4.1.2/:memory:]
#> $ condition_occurrence_id <int> 314, 970, 349, 234, 193, 919, 113, 277, 491, 9…
#> $ subject_id              <int> 314, 970, 349, 234, 193, 919, 113, 277, 491, 9…
#> $ condition_concept_id    <int> 4, 5, 1, 2, 3, 4, 3, 2, 1, 2, 1, 5, 3, 4, 1, 3…
#> $ condition_start_date    <date> 2005-08-25, 2007-02-15, 2009-02-15, 2008-05-0…
#> $ condition_end_date      <date> 2006-06-14, 2007-06-05, 2011-06-24, 2009-01-0…
#> $ age                     <dbl> 58, 77, 68, 58, 63, 54, 52, 23, 85, 74, 10, 77…
#> $ sex                     <chr> "Male", "Female", "Female", "Male", "Female", …
```

We could, for example, then limit our data to only males aged between 18
and 65

``` r
cdm$condition_occurrence  %>%
  filter(age >= 18 & age <= 65) %>%
  filter(sex == "Male")
#> # Source:   SQL [?? x 7]
#> # Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-139-generic:R 4.1.2/:memory:]
#>    condition_occurrence_id subject_id condit…¹ conditio…² conditio…³   age sex  
#>                      <int>      <int>    <int> <date>     <date>     <dbl> <chr>
#>  1                     314        314        4 2005-08-25 2006-06-14    58 Male 
#>  2                     234        234        2 2008-05-03 2009-01-05    58 Male 
#>  3                     277        277        2 2007-04-26 2007-05-07    23 Male 
#>  4                     944        944        3 2018-08-11 2019-06-22    50 Male 
#>  5                     110        110        3 2014-10-26 2014-12-26    39 Male 
#>  6                     249        249        2 2014-12-08 2015-03-08    49 Male 
#>  7                     150        150        4 2011-08-31 2012-06-02    55 Male 
#>  8                     406        406        3 2009-02-11 2010-11-25    54 Male 
#>  9                     438        438        1 2008-09-26 2010-08-25    59 Male 
#> 10                     536        536        4 2009-11-11 2010-06-07    21 Male 
#> # … with more rows, and abbreviated variable names ¹​condition_concept_id,
#> #   ²​condition_start_date, ³​condition_end_date
```

#### Adding chartacteristics of a cohort

As with other tables in the OMOP CDM, we can work in a similar way with
cohort tables. For example, say we have the below cohort table

``` r
cdm$cohort1 %>% 
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-139-generic:R 4.1.2/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 1, 2
#> $ subject_id           <dbl> 1, 1, 2, 3
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-02-02, 2020-03-01
```

We can add age, age groups, sex, and days of prior history to a cohort
like so

``` r
cdm$cohort1 <- cdm$cohort1 %>% 
  addAge(cdm = cdm, indexDate = "cohort_start_date",
         ageGroup =  list(c(0, 18),  
                          c(19, 65), 
                          c(66, 100))
          ) %>% 
  addSex(cdm = cdm) %>% 
  addPriorHistory(cdm = cdm)

cdm$cohort1 %>% 
  glimpse()
#> Rows: ??
#> Columns: 8
#> Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-139-generic:R 4.1.2/:memory:]
#> $ cohort_definition_id <dbl> 2, 1, 1, 1
#> $ subject_id           <dbl> 3, 1, 2, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-03-01, 2020-08-01, 2020-02-02, 2020-04-01
#> $ age                  <dbl> 43, 41, 51, 41
#> $ age_group            <chr> "19 to 65", "19 to 65", "19 to 65", "19 to 65"
#> $ sex                  <chr> "Female", "Male", "Male", "Male"
#> $ prior_history        <dbl> 4635, 5350, 4168, 5198
```

We could use this information to subset the cohort. For example limiting
to those with at least 365 days of prior history available before their
cohort start date like so

``` r
cdm$cohort1  %>%
  filter(prior_history >= 365)
#> # Source:   SQL [4 x 8]
#> # Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-139-generic:R 4.1.2/:memory:]
#>   cohort_definition_id subje…¹ cohort_s…² cohort_e…³   age age_g…⁴ sex   prior…⁵
#>                  <dbl>   <dbl> <date>     <date>     <dbl> <chr>   <chr>   <dbl>
#> 1                    2       3 2020-01-01 2020-03-01    43 19 to … Fema…    4635
#> 2                    1       1 2020-06-01 2020-08-01    41 19 to … Male     5350
#> 3                    1       2 2020-01-02 2020-02-02    51 19 to … Male     4168
#> 4                    1       1 2020-01-01 2020-04-01    41 19 to … Male     5198
#> # … with abbreviated variable names ¹​subject_id, ²​cohort_start_date,
#> #   ³​cohort_end_date, ⁴​age_group, ⁵​prior_history
```

### Cohort intersections

#### Intersections with patient-level data

We can use `addIntersect` to add the intersection between a cohort
table and OMOP CDM tables.

For example, if we want to identify anyone with a drug exposure prior to
their cohort start date we could do this like so:

``` r
cdm$cohort1 %>% 
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-139-generic:R 4.1.2/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 1, 2
#> $ subject_id           <dbl> 1, 1, 2, 3
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-02-02, 2020-03-01

cdm$cohort1  <- cdm$cohort1 %>% 
  addTableIntersect(cdm = cdm,
                    name = "history_of_drug_exposure",
                    tableName = "drug_exposure", 
                    window = c(NA, -1),  
                    value = "binary")

cdm$cohort1 %>% 
  glimpse()
#> Rows: ??
#> Columns: 5
#> Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-139-generic:R 4.1.2/:memory:]
#> $ cohort_definition_id           <dbl> 1, 1, 2, 1
#> $ subject_id                     <dbl> 1, 1, 3, 2
#> $ cohort_start_date              <date> 2020-01-01, 2020-06-01, 2020-01-01, 202…
#> $ cohort_end_date                <date> 2020-04-01, 2020-08-01, 2020-03-01, 202…
#> $ `binary_drug_exposure_(NA,-1)` <dbl> 1, 1, 1, 0
```

#### Intersections with another cohort

We can use `addIntersect` to compare two cohort tables.

Say we have the two following cohort tables

``` r
cdm$cohort1 %>% 
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-139-generic:R 4.1.2/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 1, 2
#> $ subject_id           <dbl> 1, 1, 2, 3
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-02-02, 2020-03-01
cdm$cohort2 %>% 
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-139-generic:R 4.1.2/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 2, 3, 1
#> $ subject_id           <dbl> 1, 3, 1, 2, 1
#> $ cohort_start_date    <date> 2019-12-30, 2020-01-01, 2020-05-25, 2020-01-01, 2…
#> $ cohort_end_date      <date> 2019-12-30, 2020-01-01, 2020-05-25, 2020-01-01, 2…
```

If we want to add a variable to cohorts in the cohort table 1
identifying whether an individual was in the cohort table cohort2 and
cohort definition id 1 any time prior to the cohort start date. we could
add this like so:

``` r
cdm$cohort1 <- cdm$cohort1 %>% 
  addIntersect(cdm = cdm,  
                     name = "history_of_cohort_2_id_1",
                     cohortTableName = "cohort2", 
                     cohortId = 1,
                     window = c(NA, -1),  
                     value = "binary")

cdm$cohort2 %>% 
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-139-generic:R 4.1.2/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 2, 3, 1
#> $ subject_id           <dbl> 1, 3, 1, 2, 1
#> $ cohort_start_date    <date> 2019-12-30, 2020-01-01, 2020-05-25, 2020-01-01, 2…
#> $ cohort_end_date      <date> 2019-12-30, 2020-01-01, 2020-05-25, 2020-01-01, 2…
```
