# PatientProfiles 1.0.0.900 (development version)

* addConceptIntersect now includes records with missing end date under the 
  assumption that end date is equal to start date.
  
* add* functions have a new argument called `name` to decide if we want a 
  resultant temp table (`name = NULL`) or have a permanent table with a certain 
  name. Additional functions are provided, e.g. addDemographicsQuery, where the 
  result is not computed.

# PatientProfiles 1.0.0

* Stable release of package
