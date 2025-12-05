library(testthat)

test_that("fisher returns factor with correct number of classes", {
  set.seed(1)
  x <- rnorm(100)

  result <- fisher(x, n = 5)

  expect_s3_class(result, "factor")
  expect_equal(nlevels(result), 5)
  expect_equal(length(result), length(x))
})

test_that("error when vec not numeric", {
  x <- letters[1:10]
  expect_error(fisher(x), "`vec` must be numeric")
})

test_that("error when vec shorter than n", {
  x <- 1:5
  expect_error(fisher(x, n = 10), "longer than number of classes")
})

test_that("same result when rerun on same data (deterministic)", {
  set.seed(123)
  x <- rnorm(50)

  result1 <- fisher(x, n = 4)
  result2 <- fisher(x, n = 4)

  expect_identical(result1, result2)
})
