test_that("test variableTypes", {
  skip_on_cran()
  expect_no_error(x <- variableTypes(dplyr::tibble()))
  expect_identical(
    x,
    dplyr::tibble("variable_name" = character(), "variable_type" = character())
  )

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

  y <- dplyr::tibble(
    x1 = c(1, 2, 3),
    x2 = as.Date(c("2021-01-05", "2025-04-19", "2000-12-12")),
    x3 = c("Jan", "Feb", "May"),
    x4 = factor(x3, levels = x3, ordered = T),
    x5 = c(
      strptime("16/Oct/2005:07:51:00", format = "%d/%b/%Y:%H:%M:%S"),
      strptime("16/Oct/2005:07:51:01", format = "%d/%b/%Y:%H:%M:%S"),
      strptime("16/Oct/2005:07:51:02", format = "%d/%b/%Y:%H:%M:%S")
    ),
    x6 = structure(c(0, 0, 0), class = "integer64"),
    x7 = as.difftime(c(223, 35, 3), units = "secs")
  )
  y$x3 <- as.factor(y$x3)

  vt <- variableTypes(y)

  expect_identical(
    vt %>%
      dplyr::filter(variable_name == "x1") %>%
      dplyr::pull("variable_type"),
    "numeric"
  )

  expect_identical(
    vt %>%
      dplyr::filter(variable_name == "x2") %>%
      dplyr::pull("variable_type"),
    "date"
  )

  expect_identical(
    vt %>%
      dplyr::filter(variable_name == "x3") %>%
      dplyr::pull("variable_type"),
    "categorical"
  )

  expect_identical(
    vt %>%
      dplyr::filter(variable_name == "x4") %>%
      dplyr::pull("variable_type"),
    "categorical"
  )

  expect_identical(
    vt %>%
      dplyr::filter(variable_name == "x5") %>%
      dplyr::pull("variable_type"),
    "date"
  )

  expect_identical(
    vt %>%
      dplyr::filter(variable_name == "x6") %>%
      dplyr::pull("variable_type"),
    "integer"
  )

  expect_identical(
    vt %>%
      dplyr::filter(variable_name == "x7") %>%
      dplyr::pull("variable_type"),
    "numeric"
  )
})

test_that("test functions", {
  skip_on_cran()
  expect_true("tbl" %in% class(availableEstimates("numeric")))
  expect_true("tbl" %in% class(availableEstimates("date")))
  expect_true("tbl" %in% class(availableEstimates("categorical")))
  expect_true(identical(
    colnames(availableEstimates("numeric")),
    c("variable_type", "estimate_name", "estimate_description", "estimate_type")
  ))
})

test_that("test available functions", {
  skip_on_cran()
  num_test <- availableEstimates("numeric")
  expect_true(all(
    c("sd", "median", "mean") %in% (
      num_test %>% dplyr::pull("estimate_name")
    )
  ))
})

test_that("binaryVariable test", {
  skip_on_cran()
  expect_false(
    binaryVariable(c("A", "B", "C"))
  )

  expect_false(
    binaryVariable(c("A", "B", "C", "D"))
  )

  expect_false(
    binaryVariable(c(0, 1, 2, 3))
  )


  expect_false(
    binaryVariable(c("A", "B", NA))
  )
  expect_true(
    binaryVariable(c(1, 0, 1))
  )
  expect_true(
    binaryVariable(c(1, 0, NA))
  )
  expect_false(
    binaryVariable(c(1, 0, "A"))
  )
})

test_that("getFunction tests", {
  skip_on_cran()
  expect_true(estimatesFunc["min"] == "base::min(x, na.rm = TRUE)")
  expect_true(estimatesFunc["max"] == "base::max(x, na.rm = TRUE)")
  expect_true(estimatesFunc["mean"] == "base::mean(x, na.rm = TRUE)")
  expect_true(estimatesFunc["sd"] == "stats::sd(x, na.rm = TRUE)")
  expect_true(estimatesFunc["q99"] == "stats::quantile(x, 0.99, na.rm = TRUE)")
  expect_true(estimatesFunc["q09"] == "stats::quantile(x, 0.09, na.rm = TRUE)")
})
