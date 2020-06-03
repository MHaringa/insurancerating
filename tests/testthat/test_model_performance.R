library(insurancerating)
context("model_performance")

test_that("check if object is of model_performance class", {
  m1 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = MTPL2)
  m2 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = MTPL2)
  x <- model_performance(m1, m2)
  expect_is(x, "model_performance")
})
