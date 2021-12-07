library(insurancerating)
context("univariate")

test_that("check if object is of univariate class", {
  x <- univariate(MTPL2, x = area, severity = amount, nclaims = nclaims,
                  exposure = exposure, premium = premium)
  expect_is(x, "univariate")
})

test_that("check if object is of data.frame class", {
  x <- univariate(MTPL2, x = area, severity = amount, nclaims = nclaims,
                  exposure = exposure, premium = premium)
  expect_is(x, "data.frame")
})

test_that("check if frequency is calculated", {
  x <- univariate(MTPL, x = zip, nclaims = nclaims, exposure = exposure)
  expect_true("frequency" %in% names(x))
})

test_that("check if average severity is calculated", {
  x <- univariate(MTPL, x = zip, severity = amount, nclaims = nclaims)
  expect_true("average_severity" %in% names(x))
})

test_that("check if risk premium is calculated", {
  x <- univariate(MTPL, x = zip, severity = amount, exposure = exposure)
  expect_true("risk_premium" %in% names(x))
})

test_that("check if loss ratio is calculated", {
  x <- univariate(MTPL2, x = area, severity = amount, premium = premium)
  expect_true("loss_ratio" %in% names(x))
})

test_that("check if average premium is calculated", {
  x <- univariate(MTPL2, x = area, premium = premium, exposure = exposure)
  expect_true("average_premium" %in% names(x))
})

test_that("check correct error message if column that does not exist is used", {
  expect_error(univariate(MTPL2, x = areaaa, premium = premium,
                          exposure = exposure),
               "Column areaaa can't be found")
})

test_that("check if correct attribute for `by` is returned if length = 1", {
  x <- univariate(MTPL, x = zip, nclaims = nclaims, exposure = exposure,
                  by = bm)
  expect_equal(as.character(attr(x, "by")[[1]]), "bm")
})

test_that("check if correct attribute for `by` is returned if length > 1", {
  x <- univariate(MTPL, x = zip, nclaims = nclaims, exposure = exposure,
                  by = list(bm, power))
  expect_equal(as.character(attr(x, "by")), c("bm","power"))
})

test_that("check if correct attribute for `by` is returned if length = 0", {
  x <- univariate(MTPL, x = zip, nclaims = nclaims, exposure = exposure)
  expect_equal(length(attr(x, "by")), 0)
})

