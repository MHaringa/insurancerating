library(insurancerating)
context("univariate")

test_that("check if object is of univariate class", {
  x <- univariate(MTPL2, x = area, severity = amount, nclaims = nclaims,
                  exposure = exposure, premium = premium)
  expect_is(x, "univariate")
})
