# test_that("working examples", {
#
#   #functionality
#   cohort1 <- dplyr::tibble(
#     cohort_definition_id = c(1, 1, 1, 1, 1),
#     subject_id = c(1, 1, 1, 2, 2),
#     cohort_start_date = as.Date(
#       c(
#         "2020-01-01",
#         "2020-01-15",
#         "2020-01-20",
#         "2020-01-01",
#         "2020-02-01"
#       )
#     ),
#     cohort_end_date = as.Date(
#       c(
#         "2020-01-01",
#         "2020-01-15",
#         "2020-01-20",
#         "2020-01-01",
#         "2020-02-01"
#       )
#     )
#   )
#
#   drug_exposure <- dplyr::tibble(
#     drug_exposure_id = c(1:7),
#     person_id = c(1, 1, 1, 2, 2, 2, 1),
#     drug_concept_id = c(1,2,2,2,2,3,3),
#     drug_exposure_start_date = as.Date(
#       c(
#         "2020-01-15",
#         "2020-01-25",
#         "2020-01-26",
#         "2020-01-29",
#         "2020-03-15",
#         "2020-01-24",
#         "2020-02-16"
#       )
#     ),
#     drug_exposure_end_date = as.Date(
#       c(
#         "2020-01-15",
#         "2020-01-25",
#         "2020-01-26",
#         "2020-01-29",
#         "2020-03-15",
#         "2020-01-24",
#         "2020-02-16"
#       )
#     ),
#     quantity = c(20,10,3,5,12,44,9),
#     class = c("A","B","B","A","A","A","B")
#   )
#
#   cdm <- mockPatientProfiles(cohort1=cohort1, drug_exposure = drug_exposure)
#
#   result_first <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", value = c("binary","number","quantity"), filter = drug_concept_id %in% c(2,3)) %>% dplyr::collect()
#   result_last <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", value = c("binary","number","quantity"), order = "last", filter = drug_concept_id %in% c(2,3)) %>% dplyr::collect()
#   result_first_w2 <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", window = c(-Inf,0), value = c("binary","number","quantity"), filter = drug_concept_id %in% c(2,3)) %>% dplyr::collect()
#   result_last_w2 <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", window = c(-Inf,0), value = c("binary","number","quantity"), order = "last", filter = drug_concept_id %in% c(2,3)) %>% dplyr::collect()
#   result_first_w3 <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", window = c(-30,30), value = c("binary","number","quantity"), filter = drug_concept_id %in% c(2,3)) %>% dplyr::collect()
#   result_last_w3 <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", window = c(-30,30), value = c("binary","number","quantity"), order = "last", filter = drug_concept_id %in% c(2,3)) %>% dplyr::collect()
#   result_last_w4 <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", order = "last", window = c(-30,40), value = c("binary","number","quantity"), filter = drug_concept_id %in% c(2,3)) %>% dplyr::collect()
#   result_last_w5 <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", order = "last", window = c(-30,50), value = c("binary","number","quantity"), filter = drug_concept_id %in% c(2,3)) %>% dplyr::collect()
#   result_first_f2 <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", value = c("binary","number","quantity"), filter = drug_concept_id %in% c(2,3) & quantity > 9) %>% dplyr::collect()
#   result_first_f3 <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", value = c("binary","number","quantity"), filter = drug_concept_id %in% c(2,3) & quantity > 10) %>% dplyr::collect()
#   result_first_f4 <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", value = c("binary","number","quantity"), filter = drug_concept_id == 2 & drug_exposure_start_date > "2020-01-25") %>% dplyr::collect()
#
#   result_two_cols <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", value = c("binary","class","quantity"), filter = drug_concept_id %in% c(2,3)) %>% dplyr::collect()
#   result_three_cols <- cdm$cohort1 %>% addTableIntersect(cdm = cdm, tableName = "drug_exposure", value = c("drug_exposure_end_date","class","quantity"), filter = drug_concept_id %in% c(2,3)) %>% dplyr::collect()
#
#   # This function returns TRUE wherever elements are the same, including NA's,
#   # and FALSE everywhere else.
#   compareNA <- function(v1,v2) {
#     same <- (v1 == v2) | (is.na(v1) & is.na(v2))
#     same[is.na(same)] <- FALSE
#     return(same)
#   }
#
#   expect_true(all(compareNA(result_first$'number_drug_exposure_(0,NA)', c(3,3,3,3,1))))
#   expect_true(all(compareNA(result_first$'binary_drug_exposure_(0,NA)', c(1,1,1,1,1))))
#   expect_true(all(compareNA(result_first$'quantity_drug_exposure_(0,NA)', c(10,10,10,44,12))))
#
#   expect_true(all(compareNA(result_last$'number_drug_exposure_(NA,0)', c(3,3,3,3,1))))
#   expect_true(all(compareNA(result_last$'binary_drug_exposure_(NA,0)', c(1,1,1,1,1))))
#   expect_true(all(compareNA(result_last$'quantity_drug_exposure_(NA,0)', c(9,9,9,12,12))))
#
#   expect_true(all(compareNA(result_first_w2$'number_drug_exposure_(0,NA)', c(2,0,0,0,0))))
#   expect_true(all(compareNA(result_first_w2$'binary_drug_exposure_(0,NA)', c(1,0,0,0,0))))
#   expect_true(all(compareNA(result_first_w2$'quantity_drug_exposure_(0,NA)', c(44,NA,NA,NA,NA))))
#
#   expect_true(all(compareNA(result_last_w2$'number_drug_exposure_(NA,0)', c(2,0,0,0,0))))
#   expect_true(all(compareNA(result_last_w2$'binary_drug_exposure_(NA,0)', c(1,0,0,0,0))))
#   expect_true(all(compareNA(result_last_w2$'quantity_drug_exposure_(NA,0)', c(5,NA,NA,NA,NA))))
#
#   expect_true(all(compareNA(result_first_w3$'number_drug_exposure_(-30,30)', c(2,2,3,2,2))))
#   expect_true(all(compareNA(result_first_w3$'binary_drug_exposure_(-30,30)', c(1,1,1,1,1))))
#   expect_true(all(compareNA(result_first_w3$'quantity_drug_exposure_(-30,30)', c(10,10,10,44,44))))
#
#   expect_true(all(compareNA(result_last_w3$'number_drug_exposure_(-30,30)', c(2,2,3,2,2))))
#   expect_true(all(compareNA(result_last_w3$'binary_drug_exposure_(-30,30)', c(1,1,1,1,1))))
#   expect_true(all(compareNA(result_last_w3$'quantity_drug_exposure_(-30,30)', c(3,3,9,5,5))))
#
#   expect_true(all(compareNA(result_last_w4$'number_drug_exposure_(-30,40)', c(2,3,3,2,2))))
#   expect_true(all(compareNA(result_last_w4$'binary_drug_exposure_(-30,40)', c(1,1,1,1,1))))
#   expect_true(all(compareNA(result_last_w4$'quantity_drug_exposure_(-30,40)', c(3,9,9,5,5))))
#
#   expect_true(all(compareNA(result_last_w5$'number_drug_exposure_(-30,50)', c(3,3,3,2,3))))
#   expect_true(all(compareNA(result_last_w5$'binary_drug_exposure_(-30,50)', c(1,1,1,1,1))))
#   expect_true(all(compareNA(result_last_w5$'quantity_drug_exposure_(-30,50)', c(9,9,9,5,12))))
#
#   expect_true(all(compareNA(result_first_f2$'number_drug_exposure_(0,NA)', c(1,1,1,2,1))))
#   expect_true(all(compareNA(result_first_f2$'binary_drug_exposure_(0,NA)', c(1,1,1,1,1))))
#   expect_true(all(compareNA(result_first_f2$'quantity_drug_exposure_(0,NA)', c(10,10,10,44,12))))
#
#   expect_true(all(compareNA(result_first_f3$'number_drug_exposure_(0,NA)', c(2,1,0,0,0))))
#   expect_true(all(compareNA(result_first_f3$'binary_drug_exposure_(0,NA)', c(1,1,0,0,0))))
#   expect_true(all(compareNA(result_first_f3$'quantity_drug_exposure_(0,NA)', c(44,12,NA,NA,NA))))
#
#   expect_true(all(compareNA(result_first_f4$'number_drug_exposure_(0,NA)', c(1,1,1,2,1))))
#   expect_true(all(compareNA(result_first_f4$'binary_drug_exposure_(0,NA)', c(1,1,1,1,1))))
#   expect_true(all(compareNA(result_first_f4$'quantity_drug_exposure_(0,NA)', c(3,3,3,5,12))))
#
#
#   expect_true(all(compareNA(result_two_cols$'class_drug_exposure_(0,NA)', c("B","B","B","A","A"))))
#   expect_true(all(compareNA(result_two_cols$'binary_drug_exposure_(0,NA)', c(1,1,1,1,1))))
#   expect_true(all(compareNA(result_two_cols$'quantity_drug_exposure_(0,NA)', c(10,10,10,44,12))))
#
#   expect_true(all(compareNA(result_three_cols$'drug_exposure_end_date_drug_exposure_(0,NA)', c("2020-01-25","2020-01-25","2020-01-25","2020-01-24","2020-03-15"))))
#
#  
#
# })
#
# test_that("check input length and type for each of the arguments", {
#   cohort1 <- dplyr::tibble(
#     cohort_definition_id = c(1, 1, 1, 1, 1),
#     subject_id = c(1, 1, 1, 2, 2),
#     cohort_start_date = as.Date(
#       c(
#         "2020-01-01",
#         "2020-01-15",
#         "2020-01-20",
#         "2020-01-01",
#         "2020-02-01"
#       )
#     ),
#     cohort_end_date = as.Date(
#       c(
#         "2020-01-01",
#         "2020-01-15",
#         "2020-01-20",
#         "2020-01-01",
#         "2020-02-01"
#       )
#     )
#   )
#
#   drug_exposure <- dplyr::tibble(
#     drug_exposure_id = c(1:7),
#     person_id = c(1, 1, 1, 2, 2, 2, 1),
#     drug_concept_id = c(1,2,2,2,2,3,3),
#     drug_exposure_start_date = as.Date(
#       c(
#         "2020-01-15",
#         "2020-01-25",
#         "2020-01-26",
#         "2020-01-29",
#         "2020-03-15",
#         "2020-01-24",
#         "2020-02-16"
#       )
#     ),
#     drug_exposure_end_date = as.Date(
#       c(
#         "2020-01-15",
#         "2020-01-25",
#         "2020-01-26",
#         "2020-01-29",
#         "2020-03-15",
#         "2020-01-24",
#         "2020-02-16"
#       )
#     ),
#     quantity = c(20,10,3,5,12,44,9)
#   )
#
#   cdm <- mockPatientProfiles(cohort1=cohort1, drug_exposure = drug_exposure)
#
#   expect_error(addCohortIntersect("cdm$cohort1", cdm))
#
#   expect_error(addCohortIntersect(cdm$cohort1, "cdm"))
#
#   expect_error(addCohortIntersect(cdm$cohort1, cdm, value = "end_date"))
#
#   expect_error(addCohortIntersect(cdm$cohort1, cdm, value = "binary", order = "lol"))
#
#   expect_error(addCohortIntersect(cdm$cohort1, cdm, value = "number", order = "first", tableName = "condition_visit"))
#
#   expect_error(addCohortIntersect(cdm$cohort1, cdm, value = "number", order = "first", tableName = "drug_exposure"))
#
#   expect_error(addCohortIntersect(cdm$cohort1, cdm, value = "number", order = "first", tableName = "drug_exposure", window = "name"))
#
#   expect_error(addCohortIntersect(cdm$cohort1, cdm, value = "number", order = "first", tableName = "drug_exposure", window = c(0,20), name = 3))
#
#   expect_error(addCohortIntersect(cdm$cohort1, cdm, value = "number", order = "first", tableName = "drug_exposure", filter = "can't filter with this"))
#
#   
#
# })
