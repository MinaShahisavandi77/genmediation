test_that("standardise_vars z-scores selected columns", {
  set.seed(42)
  n  <- 100
  df <- data.frame(x = rnorm(n, 5, 2), y = rnorm(n, 3, 1), z = rnorm(n))
  out <- standardise_vars(df, vars = c("x", "y"))
  expect_equal(mean(out$x), 0, tolerance = 1e-10)
  expect_equal(mean(out$y), 0, tolerance = 1e-10)
  expect_equal(sd(out$x),   1, tolerance = 1e-10)
  expect_equal(out$z, df$z)   # untouched
})

test_that("standardise_vars errors on missing columns", {
  df <- data.frame(x = 1:5)
  expect_error(standardise_vars(df, vars = c("x", "missing_col")), "not found")
})

test_that("standardise_vars errors on non-data.frame input", {
  expect_error(standardise_vars(list(x = 1:5), vars = "x"), "data.frame")
})

test_that("standardise_vars handles a single column", {
  df  <- data.frame(a = rnorm(50, 10, 3))
  out <- standardise_vars(df, vars = "a")
  expect_equal(mean(out$a), 0, tolerance = 1e-10)
})
