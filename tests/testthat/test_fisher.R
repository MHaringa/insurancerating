library(testthat)

test_that("fisher returns factor with correct number of classes", {
  set.seed(1)
  x <- rnorm(100)

  result <- expect_warning(fisher_classify(x, n = 5), "deprecated")

  expect_s3_class(result, "factor")
  expect_equal(nlevels(result), 5)
  expect_equal(length(result), length(x))
})

test_that("error when vec not numeric", {
  x <- letters[1:10]
  expect_error(suppressWarnings(fisher_classify(x)), "`x` must be numeric")
})

test_that("error when vec shorter than n", {
  x <- 1:5
  expect_error(
    suppressWarnings(fisher_classify(x, n = 10)),
    "longer than number of classes"
  )
})

test_that("same result when rerun on same data (deterministic)", {
  set.seed(123)
  x <- rnorm(50)

  result1 <- suppressWarnings(fisher_classify(x, n = 4))
  result2 <- suppressWarnings(fisher_classify(x, n = 4))

  expect_identical(result1, result2)
})
