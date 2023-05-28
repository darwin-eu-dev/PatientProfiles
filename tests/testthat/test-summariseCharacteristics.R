test_that("test all functions", {
  x <- dplyr::tibble(
    s  = c("g1", "g1", "g2", "g1&&g2", "g2", "g1&&g2"),
    v1 = c(1, 2, 3, 4, 6, 3),
    v2 = c("a", "b", "a&&b", "b", "0", "0&&ab"),
    v3 = c(0, 1, 0, 1, 1, 0),
    v4 = as.Date(c(
      "2021-05-12", "2012-05-15", "2023-11-30", "2015-12-10", "2014-01-12",
      "1993-04-190"
    ))
  )
  s1 <- summariseCharacteristics(x)
  s2 <- summariseCharacteristics(x, strata = list("group" = "s"))
  s3 <- summariseCharacteristics(
    x, strata = list("group" = "s"), suppressCellCount = 1
  )
  s4 <- summariseCharacteristics(
    x, strata = list("group1" = c("s", "v2"), group2 = "s"),
    suppressCellCount = 1
  )
})
