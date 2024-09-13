# PatientProfiles 1.2.0

* `addObservationPeriodId()` is a new function that adds the number of 
  observation period that an observation is in.
  
* Add `density` estimate to `summariseResult()`

# PatientProfiles 1.1.1

* `addCohortName` overwrites if there already exists a `cohort_name` column 
  #680 and #682.

* Correct nan and Inf for missing values #674

* Fix #670 #671

# PatientProfiles 1.1.0

* addConceptIntersect now includes records with missing end date under the 
  assumption that end date is equal to start date.
  
* add* functions have a new argument called `name` to decide if we want a 
  resultant temp table (`name = NULL`) or have a permanent table with a certain 
  name. Additional functions are provided, e.g. addDemographicsQuery, where the 
  result is not computed.

# PatientProfiles 1.0.0

* Stable release of package
