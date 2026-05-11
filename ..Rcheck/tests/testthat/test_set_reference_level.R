library(testthat)

test_that("set_reference_level sets correct reference level", {
  x <- factor(c("A", "B", "A", "C", "B", "B"))
  weight <- c(1, 5, 2, 1, 3, 1)
  # totals: A=3, B=9, C=1 → B largest

  result <- set_reference_level(x, weight)
  expect_equal(levels(result)[1], "B")
  expect_s3_class(result, "factor")
})

test_that("original levels attribute is stored", {
  x <- factor(c("low", "medium", "high"))
  weight <- c(1, 2, 3)

  result <- set_reference_level(x, weight)
  expect_true("xoriginal" %in% names(attributes(result)))
  expect_equal(attr(result, "xoriginal"), levels(x))
})

test_that("works with equal weights (alphabetical tie-break)", {
  x <- factor(c("dog", "cat", "dog", "cat"))
  weight <- c(1, 1, 1, 1)
  # totals: dog=2, cat=2 → tie → sort() chooses alphabetically

  result <- set_reference_level(x, weight)
  expect_equal(levels(result)[1], "cat")
})

test_that("errors when x is not a factor", {
  x <- c("A", "B", "C")
  weight <- c(1, 2, 3)

  expect_error(set_reference_level(x, weight), "`x` must be a factor")
})

test_that("errors when weight not numeric", {
  x <- factor(c("A", "B"))
  weight <- c("a", "b")

  expect_error(set_reference_level(x, weight), "`weight` must be numeric")
})

test_that("errors when length mismatch", {
  x <- factor(c("A", "B"))
  weight <- c(1)

  expect_error(set_reference_level(x, weight),
               "`x` and `weight` must have the same length")
})

test_that("method is validated", {
  x <- factor(c("A", "B"))
  weight <- c(1, 2)

  expect_error(set_reference_level(x, weight, method = "smallest_weight"),
               "`method` must be 'largest_weight' or 'manual'")
})

test_that("manual method sets chosen reference level", {
  x <- factor(c("Thuis", "Kantoor", "Winkel"))

  result <- set_reference_level(
    x,
    method = "manual",
    reference_level = "Kantoor"
  )

  expect_equal(levels(result)[1], "Kantoor")
  expect_equal(attr(result, "xoriginal"), levels(x))
})

test_that("manual method validates reference level", {
  x <- factor(c("Thuis", "Kantoor", "Winkel"))

  expect_error(
    set_reference_level(x, method = "manual"),
    "`reference_level` must be a single character string"
  )
  expect_error(
    set_reference_level(x, method = "manual", reference_level = "Garage"),
    "`reference_level` must be one of the levels of `x`"
  )
})

test_that("reference_level is only accepted for manual method", {
  x <- factor(c("A", "B"))
  weight <- c(1, 2)

  expect_error(
    set_reference_level(x, weight, reference_level = "A"),
    "`reference_level` can only be used when `method = 'manual'`"
  )
})

test_that("biggest_reference remains available as deprecated alias", {
  x <- factor(c("A", "B"))
  weight <- c(1, 2)

  expect_warning(
    result <- biggest_reference(x, weight),
    "deprecated"
  )
  expect_equal(levels(result)[1], "B")
})
