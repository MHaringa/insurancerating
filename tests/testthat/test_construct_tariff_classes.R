library(insurancerating)
context("construct_tariff_classes")

test_that("check if object is of insurancerating class", {
  x <- fit_gam(MTPL, nclaims = nclaims, x = age_policyholder, exposure = exposure)
  expect_is(x, "fitgam")
})
