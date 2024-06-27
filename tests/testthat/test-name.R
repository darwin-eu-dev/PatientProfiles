
test_that("test that name argument works as expected", {
  skip_on_cran()
  # create simple cdm
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  prefix <- CDMConnector::cdmWriteSchema(cdm)
  prefix <- prefix["prefix"] |> unname()

  # define the different functions
  funs <- list(
    function(x, name) {
      x |> addDemographics(name = name)
    },
    function(x, name) {
      x |> addSex(name = name)
    },
    function(x, name) {
      x |> addAge(name = name)
    },
    function(x, name) {
      x |> addPriorObservation(name = name)
    },
    function(x, name) {
      x |> addFutureObservation(name = name)
    },
    function(x, name) {
      x |> addDateOfBirth(name = name)
    },
    function(x, name) {
      x |> addInObservation(name = name)
    },
    function(x, name) {
      x |> addDeathDate(name = name)
    },
    function(x, name) {
      x |> addDeathDays(name = name)
    },
    function(x, name) {
      x |> addDeathFlag(name = name)
    },
    function(x, name) {
      x |> addCohortIntersectCount(targetCohortTable = "cohort2", name = name)
    },
    function(x, name) {
      x |> addCohortIntersectFlag(targetCohortTable = "cohort2", name = name)
    },
    function(x, name) {
      x |> addCohortIntersectDate(targetCohortTable = "cohort2", name = name)
    },
    function(x, name) {
      x |> addCohortIntersectDays(targetCohortTable = "cohort2", name = name)
    },
    # function(x, name) {
    #   x |> addConceptIntersectDays(conceptSet = list(a = 1), name = name)
    # },
    # function(x, name) {
    #   x |> addConceptIntersectDate(conceptSet = list(a = 1), name = name)
    # },
    # function(x, name) {
    #   x |> addConceptIntersectFlag(conceptSet = list(a = 1), name = name)
    # },
    # function(x, name) {
    #   x |> addConceptIntersectCount(conceptSet = list(a = 1), name = name)
    # },
    function(x, name) {
      x |> addTableIntersectCount(tableName = "drug_exposure", name = name)
    },
    function(x, name) {
      x |> addTableIntersectFlag(tableName = "drug_exposure", name = name)
    },
    function(x, name) {
      x |> addTableIntersectDate(tableName = "drug_exposure", name = name)
    },
    function(x, name) {
      x |> addTableIntersectDays(tableName = "drug_exposure", name = name)
    },
    function(x, name) {
      x |>
        addTableIntersectField(
          tableName = "drug_exposure", name = name, field = "drug_concept_id"
        )
    }
  )

  k <- 1
  for (fun in funs) {
    # check NULL behavior
    initialTables <- readTables(cdm)
    x <- cdm$cohort1 |> fun(name = NULL)
    finalTables <- readTables(cdm)
    expect_identical(setdiff(initialTables, finalTables), character())
    difference <- setdiff(finalTables, initialTables)
    expect_true(length(difference) == 1)
    expect_true(substr(difference, 1, 3) == "og_")

    # check permanent behavior
    name <- paste0("my_test_", k)
    k <- k + 1
    initialTables <- readTables(cdm)
    x <- cdm$cohort1 |> fun(name = name)
    finalTables <- readTables(cdm)
    expect_identical(setdiff(initialTables, finalTables), character())
    difference <- setdiff(finalTables, initialTables)
    expect_identical(difference, paste0(prefix, name))
  }

  mockDisconnect(cdm)
})
