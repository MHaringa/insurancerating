testthat::test_that("split_level supports old and named-vector syntax", {
  old <- split_level(
    level = "commercial",
    new_levels = c("shop", "office"),
    relativities = c(1.10, 0.90)
  )
  modern <- split_level(
    level = "commercial",
    new_levels = c(shop = 1.10, office = 0.90)
  )

  testthat::expect_identical(modern, old)
  testthat::expect_identical(relativities(modern), relativities(old))
  testthat::expect_named(modern, "commercial")
  testthat::expect_named(modern$commercial, c("new_level", "relativity"))
})

testthat::test_that("split_level accepts non-syntactic named levels", {
  result <- split_level(
    "commercial",
    c("retail shop" = 1.10, "office / services" = 0.90)
  )

  testthat::expect_identical(
    result$commercial$new_level,
    c("retail shop", "office / services")
  )
  testthat::expect_equal(result$commercial$relativity, c(1.10, 0.90))
})

testthat::test_that("split_level validates named-vector syntax", {
  testthat::expect_error(
    split_level("commercial", c(1.10, 0.90)),
    "named numeric vector"
  )
  testthat::expect_error(
    split_level(
      "commercial",
      stats::setNames(c(1.10, 0.90), c("shop", ""))
    ),
    "every value.*non-empty name"
  )
  testthat::expect_error(
    split_level("commercial", c(shop = "1.10", office = "0.90")),
    "named numeric vector"
  )
  testthat::expect_error(
    split_level(
      "commercial",
      stats::setNames(c(1.10, 0.90), c("shop", "shop"))
    ),
    "duplicate level names"
  )
})

testthat::test_that("split_level retains old length validation", {
  testthat::expect_error(
    split_level(
      "commercial",
      new_levels = c("shop", "office"),
      relativities = 1.10
    ),
    "same length"
  )
})
