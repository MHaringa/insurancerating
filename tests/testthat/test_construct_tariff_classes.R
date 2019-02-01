library(insurancerating)
context("construct_tariff_classes")

test_that("check if object is of insurancerating class", {
  x <- construct_tariff_classes(MTPL, nclaims, age_policyholder, exposure)
  expect_is(x, "insurancerating")
})
