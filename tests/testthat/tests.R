
# multiCodeVars
testthat::expect_equal(
  multiCodeVars(factor(c("0", "0", "1", "2"), levels = c("0", "1", "2"))),
  c(0, 0, 1, 2)
)

testthat::expect_equal(
  multiCodeVars(factor(c("11", "11", "21", "42"), levels = c("11", "21", "42"))),
  c(0, 0, 1, 2)
)

# coerceClass

dat = data.table::data.table(
  a = 1:5,
  b = as.character(1:5)
)

testthat::expect_equal(
  unique(
    sapply(
      coerceClass(dat, "a", "as.character"), class
    )
  ),
  "character"
)
