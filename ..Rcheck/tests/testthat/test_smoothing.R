library(insurancerating)
context("smoothing")

test_that("check if object is of fitgam class", {
  x <- risk_factor_gam(MTPL,
                      claim_count = "nclaims",
                      risk_factor = "age_policyholder",
                      exposure = "exposure")
  expect_is(x, "fitgam")
})
