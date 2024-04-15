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

  y <- dplyr::tibble(
    x1 = c(1, 2, 3),
    x2 = as.Date(c("2021-01-05", "2025-04-19", "2000-12-12")),
    x3 = c("Jan", "Feb", "May"),
    x4 = factor(x3, levels = x3, ordered = T),
    x5 = c(strptime('16/Oct/2005:07:51:00',format='%d/%b/%Y:%H:%M:%S'),
           strptime('16/Oct/2005:07:51:01',format='%d/%b/%Y:%H:%M:%S'),
           strptime('16/Oct/2005:07:51:02',format='%d/%b/%Y:%H:%M:%S')),
    x6 = structure(c(0,0,0), class = "integer64"),
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
  expect_warning(availableFunctions())
  expect_true("tbl" %in% class(availableEstimates("numeric")))
  expect_true("tbl" %in% class(availableEstimates("date")))
  expect_true("tbl" %in% class(availableEstimates("categorical")))
  expect_true(identical(
    colnames(availableEstimates("numeric")),
    c("variable_type", "estimate_name", "estimate_description", "estimate_type")
  ))
})

test_that("test available functions",{
  expect_warning(
    num_test <- availableFunctions("numeric")
  )
  expect_true(all(
    c("sd", "median", "mean") %in% (
      num_test %>% dplyr::pull("format_key")
      )
  ))
})
