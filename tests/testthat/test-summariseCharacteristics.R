test_that("output format - one outcome cohort", {
  x <- dplyr::tibble(
    a = c(1, 2, 3, 4, 6),
    b = c("a", "b", "a", "b", "0")
  )
  summariseCharacteristics(x)
})
