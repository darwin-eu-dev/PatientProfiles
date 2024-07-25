test_that("addCdmName and addCohortName work", {
 cdm <- mockPatientProfiles()
 person <- cdm$person |> addCdmName()
 expect_equal(c("cdm_name", colnames(cdm$person)) |> sort(), colnames(person) |> sort())

 expect_message(cohort <- cdm$cohort1 |> addCdmName() |> addCdmName())
 expect_equal(c("cdm_name", colnames(cdm$cohort1)) |> sort(), colnames(cohort) |> sort())

 expect_error(person <- cdm$person |> addCohortName())

 expect_message(cohort <- cdm$cohort2 |> addCohortName() |> addCohortName())
 expect_equal(c("cohort_name", colnames(cdm$cohort2)) |> sort(), colnames(cohort) |> sort())
})
