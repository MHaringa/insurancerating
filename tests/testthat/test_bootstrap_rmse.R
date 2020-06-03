library(insurancerating)
context("bootstrap_rmse")

test_that("check if object is of bootstrap_rmse class", {
  mod1 <- glm(nclaims ~ age_policyholder, data = MTPL,
       offset = log(exposure), family = poisson())

  x <- bootstrap_rmse(mod1, MTPL, n = 10, show_progress = FALSE)
  expect_is(x, "bootstrap_rmse")
})
