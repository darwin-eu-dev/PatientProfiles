test_that("addSex, check imput length and type", {
  cdm <- mockPatientProfiles(connectionDetails)
  expect_error(addSex("cdm$cohort1"))
  expect_warning(addSex(cdm$cohort1, cdm = cdm))
  expect_error(addSex(cdm$drug_strength))
  expect_error(addSex(cdm$cohort1, name = 2))
  expect_error(addSex(cdm$cohort1, name = c("name1", "name2")))
  expect_error(addSex(cdm))
})

test_that("addSex, works in both cohort and condition tables", {
  cdm <- mockPatientProfiles(connectionDetails)
  cdm$cohort1 <- cdm$cohort1 %>% addSex()
  cdm$condition_occurrence <- cdm$condition_occurrence %>% addSex()
  expect_true("sex" %in% colnames(cdm$cohort1))
  expect_true(all(cdm$cohort1 %>% dplyr::pull("sex") == c("Female", "Female")))
  expect_true("sex" %in% colnames(cdm$condition_occurrence))
  expect_true(all(cdm$condition_occurrence %>% dplyr::pull("sex") == c("Female")))
})

test_that("addSex, desired result for all parameters", {
  cdm <- mockPatientProfiles(connectionDetails)
  cdm$cohort2 <- cdm$cohort2 %>% addSex()
  expect_true("sex" %in% colnames(cdm$cohort2))
  expect_true(all(cdm$cohort2 %>% dplyr::pull("sex") == c("Female", "Female", "Female")))
  cdm$condition_occurrence <- cdm$condition_occurrence %>% addSex(sexName = "gender")
  expect_true("gender" %in% colnames(cdm$condition_occurrence))
  expect_false("sex" %in% colnames(cdm$condition_occurrence))
  expect_true(all(cdm$condition_occurrence %>% dplyr::pull("gender") == c("Female")))
})

test_that("different names", {
  cdm <- mockPatientProfiles(connectionDetails)
  cdm$cohort2 <- cdm$cohort2 %>% addSex(sexName = "gender")
  expect_true("gender" %in% colnames(cdm$cohort2))
})
