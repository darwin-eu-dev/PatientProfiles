test_that("test variableTypes", {
  x <- dplyr::tibble(
    x1 = c(1, 2, 3), x2 = as.Date(c("2021-01-05", "2025-04-19", "2000-12-12")),
    x3 = c(0, 0, 0), x4 = as.integer(c(1, 0, 1)), x5 = c(-1, -1, -1),
    x6 = c("a", "a", "a"), x7 = c("hi", "bye", "day"), x8 = c(T, F, T)
  )
  vt <- variableTypes(x)
  expect_true(identical(vt$variable_name, colnames(x)))
  expect_true(all(vt$variable_type == c(
    "numeric", "date", "numeric", "integer", "numeric", "categorical",
    "categorical", "logical"
  )))
  expect_true(identical(detectVariables(x, "numeric"), c("x1", "x3", "x5")))
  expect_true(identical(detectVariables(x, "date"), "x2"))
  expect_true(identical(detectVariables(x, "integer"), "x4"))
  expect_true(identical(detectVariables(x, "categorical"), c("x6", "x7")))
})

test_that("test functions", {
  expect_warning(availableFunctions())
  expect_true("tbl" %in% class(availableEstimates("numeric")))
  expect_true("tbl" %in% class(availableEstimates("date")))
  expect_true("tbl" %in% class(availableEstimates("categorical")))
  expect_true(identical(
    colnames(availableEstimates("numeric")),
    c("variable_type", "estimate_name", "estimate_description", "estimate_type")
  ))
})

