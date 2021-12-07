library(insurancerating)
context("smoothing")

test_that("check if object is of fitgam class", {
  x <- fit_gam(MTPL, nclaims = nclaims, x = age_policyholder,
               exposure = exposure)
  expect_is(x, "fitgam")
})
