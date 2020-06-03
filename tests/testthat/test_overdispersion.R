library(insurancerating)
context("overdispersion")

test_that("check if object is of overdispersion class", {
  x <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = MTPL2)
  x_disp <- check_overdispersion(x)
  expect_is(x_disp, "overdispersion")
})
