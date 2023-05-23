test_that("test variableTypes", {
  x <- dplyr::tibble(
    x1 = c(1, 2, 3), x2 = as.Date(c("2021-01-05", "2025-04-19", "2000-12-12")),
    x3 = c(0, 0, 0), x4 = c(1, 0, 1), x5 = c(-1, -1, -1), x6 = c("a", "a", "a"),
    x7 = c("hi", "bye", "day"), x8 = c(T, F, T)
  )
  vt <- variableTypes(x)
  expect_true(identical(vt$variable, colnames(x)))
  expect_true(all(
    vt$variable_type== c("dbl", "date", "dbl", "dbl", "dbl", "chr", "chr", "lgl")
  ))
  expect_true(identical(
    vt$variable_classification,
    c("numeric", "date", "binary", "binary", "numeric", "categorical",
      "categorical", "binary")
  ))
  expect_true(identical(detectVariables(x, "numeric"), c("x1", "x5")))
  expect_true(identical(detectVariables(x, "date"), "x2"))
  expect_true(identical(detectVariables(x, "binary"), c("x3", "x4", "x8")))
  expect_true(identical(detectVariables(x, "categorical"), c("x6", "x7")))
})

test_that("test functions", {
  expect_true("tbl" %in% class(availableFunctions("numeric")))
  expect_true("tbl" %in% class(availableFunctions("date")))
  expect_true("tbl" %in% class(availableFunctions("binary")))
  expect_true("tbl" %in% class(availableFunctions("categorical")))
  expect_true(identical(
    colnames(availableFunctions("numeric")),
    c("format_key", "applied_function", "info", "are_NA_considered", "result")
  ))
  expect_true(identical(
    colnames(availableFunctions("date")),
    c("format_key", "applied_function", "info", "are_NA_considered", "warnings",
      "result")
  ))
  expect_true(identical(
    colnames(availableFunctions("binary")),
    c("format_key", "applied_function", "are_NA_considered", "result")
  ))
  expect_true(identical(
    colnames(availableFunctions("categorical")),
    c("format_key", "applied_function", "are_NA_considered", "result")
  ))
})

