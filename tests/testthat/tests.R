
testthat::expect_equal(
  multiCodeVars(factor(c("0", "0", "1", "2"), levels = c("0", "1", "2"))),
  c(0, 0, 1, 2)
)
