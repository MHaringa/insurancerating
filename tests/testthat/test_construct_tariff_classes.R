context("construct_tariff_classes")

testthat::test_that("check if object is of fitgam class", {
  x <- fit_gam(MTPL, nclaims = nclaims, x = age_policyholder,
               exposure = exposure)
  testthat::expect_is(x, "fitgam")
})
