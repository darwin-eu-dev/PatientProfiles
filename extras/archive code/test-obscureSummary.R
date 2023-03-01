test_that("default options, one cohort_definition_id",{
  summary <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    variable = c("number_observations","number_subjects","sex_male"),
    estimate = c("count","count","count"),
    value = c(8,6,5)
  )
  summary_obscured <- obscureSummary(summary)
  expect_true(all(summary == summary_obscured))
})

test_that("default options, multiple cohort_definition_id",{
  summary <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1", "1","1","1","2", "2", "3","3"),
    variable = c("number_observations","number_subjects","sex_male","sex_female","age","age","age","age","sex_male","sex_female"),
    estimate = c("count","count","count","count","mean","median","mean","median","count","count"),
    value = c(2,5,4,1,23,24,36,38,12,4)
  )
  summary_obscured <- obscureSummary(summary)

  # This function returns TRUE wherever elements are the same, including NA's,
  # and FALSE everywhere else.
  compareNA <- function(v1,v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  expect_true(all(compareNA(summary_obscured %>% dplyr::filter(cohort_definition_id == 1) %>% dplyr::select(value) %>% dplyr::pull(), c("<5","<5",NA,NA,NA,NA))))
  expect_true(all(compareNA(summary_obscured %>% dplyr::filter(cohort_definition_id == 2) %>% dplyr::select(value) %>% dplyr::pull(), c(36,38))))
  expect_true(all(compareNA(summary_obscured %>% dplyr::filter(cohort_definition_id == 3) %>% dplyr::select(value) %>% dplyr::pull(), c(12,"<5"))))
})

test_that("global variables don't exist",{
  summary <- tibble::tibble(
    cohort_definition_id = c("1", "1","1","1","2", "2", "3","3"),
    variable = c("sex_male","sex_female","age","age","age","age","sex_male","sex_female"),
    estimate = c("count","count","mean","median","mean","median","count","count"),
    value = c(4,1,23,24,36,38,12,4)
  )
  summary_obscured <- obscureSummary(summary)

  expect_true(all(summary_obscured %>% dplyr::filter(cohort_definition_id == 1) %>% dplyr::select(value) %>% dplyr::pull() == c("<5","<5",23,24)))
  expect_true(all(summary_obscured %>% dplyr::filter(cohort_definition_id == 2) %>% dplyr::select(value) %>% dplyr::pull() == c(36,38)))
  expect_true(all(summary_obscured %>% dplyr::filter(cohort_definition_id == 3) %>% dplyr::select(value) %>% dplyr::pull() == c(12,"<5")))
})

test_that("minimumCellCount very large",{
  summary <- tibble::tibble(
    cohort_definition_id = c("1", "1","1","1","2", "2", "3","3"),
    variable = c("sex_male","sex_female","age","age","age","age","sex_male","sex_female"),
    estimate = c("count","count","mean","median","mean","median","count","count"),
    value = c(4,1,23,24,36,38,12,4)
  )
  summary_obscured <- obscureSummary(summary,minimumCellCounts = 40)

  expect_true(all(summary_obscured %>% dplyr::filter(cohort_definition_id == 1) %>% dplyr::select(value) %>% dplyr::pull() == c("<40","<40",23,24)))
  expect_true(all(summary_obscured %>% dplyr::filter(cohort_definition_id == 2) %>% dplyr::select(value) %>% dplyr::pull() == c(36,38)))

})

test_that("minimumCellCount very small",{
  summary <- tibble::tibble(
    cohort_definition_id = c("1", "1","1","1","2", "2", "3","3"),
    variable = c("sex_male","sex_female","age","age","age","age","sex_male","sex_female"),
    estimate = c("count","count","mean","median","mean","median","count","count"),
    value = c(4,1,23,24,36,38,12,4)
  )
  summary_obscured <- obscureSummary(summary,minimumCellCounts = 1)

  expect_true(all(summary_obscured == summary))

})

test_that("expected errors",{
  summary <- tibble::tibble(
    cohort_definition_id = c("1", "1","1","1","2", "2", "3","3"),
    variable = c("sex_male","sex_female","age","age","age","age","sex_male","sex_female"),
    estimate = c("count","count","mean","median","mean","median","count","count"),
    value = c(4,1,23,24,36,38,12,4)
  )
  expect_error(obscureSummary("summary"))
  expect_error(obscureSummary(summary %>% dplyr::select(-cohort_definition_id)))
  expect_error(obscureSummary(summary, minimumCellCounts = "20"))
  expect_error(obscureSummary(summary, minimumCellCounts = c(2,5)))
  expect_error(obscureSummary(summary, globalVariables = 1))
  expect_error(obscureSummary(summary, estimatesToObscure = c(20,3)))
})


