library(testthat)

test_that("biggest_reference sets correct reference", {
  x <- factor(c("A", "B", "A", "C", "B", "B"))
  weight <- c(1, 5, 2, 1, 3, 1)
  # totals: A=3, B=9, C=1 → B largest

  result <- biggest_reference(x, weight)
  expect_equal(levels(result)[1], "B")
  expect_s3_class(result, "factor")
})

test_that("original levels attribute is stored", {
  x <- factor(c("low", "medium", "high"))
  weight <- c(1, 2, 3)

  result <- biggest_reference(x, weight)
  expect_true("xoriginal" %in% names(attributes(result)))
  expect_equal(attr(result, "xoriginal"), levels(x))
})

test_that("works with equal weights (alphabetical tie-break)", {
  x <- factor(c("dog", "cat", "dog", "cat"))
  weight <- c(1, 1, 1, 1)
  # totals: dog=2, cat=2 → tie → sort() chooses alphabetically

  result <- biggest_reference(x, weight)
  expect_equal(levels(result)[1], "cat")
})

test_that("errors when x is not a factor", {
  x <- c("A", "B", "C")
  weight <- c(1, 2, 3)

  expect_error(biggest_reference(x, weight), "`x` must be a factor")
})

test_that("errors when weight not numeric", {
  x <- factor(c("A", "B"))
  weight <- c("a", "b")

  expect_error(biggest_reference(x, weight), "`weight` must be numeric")
})

test_that("errors when length mismatch", {
  x <- factor(c("A", "B"))
  weight <- c(1)

  expect_error(biggest_reference(x, weight),
               "`x` and `weight` must have the same length")
})
