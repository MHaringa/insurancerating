library(insurancerating)
context("Model refinement: smoothing and restrictions coefficients")

mod1 <- glm(cyl ~ mpg + disp + offset(log(gear)),
            family = "poisson",
            data = mtcars)
mod2 <- glm(cyl ~ mpg + disp, offset = log(gear),
            family = "poisson",
            data = mtcars)
mod3 <- glm(cyl ~ mpg + disp,
            family = "poisson",
            data = mtcars)
mod4 <- glm(cyl ~ mpg + disp + offset(log(gear)) + offset(log(disp)),
            family = "poisson",
            data = mtcars)
mod5 <- glm(cyl ~ mpg + disp + offset(log(gear)),
            offset = log(disp),
            family = "poisson", data = mtcars)

testthat::test_that(
  "Correct offset-term is returned", {
    testthat::expect_equal(get_offset(mod1), "log(gear)")
    testthat::expect_equal(get_offset(mod2), "log(gear)")
    testthat::expect_equal(get_offset(mod3), NULL)
  }
)

testthat::test_that(
  "Error is returned for multiple offset-terms", {
    testthat::expect_error(get_offset(mod4))
    testthat::expect_error(get_offset(mod5))
  }
)

testthat::test_that(
  "Offset-term is removed from formula", {
    fm <- formula(cyl ~ mpg + disp)
    testthat::expect_equal(remove_offset_formula(formula(mod1)), fm)
    testthat::expect_equal(remove_offset_formula(formula(mod2)), fm)
    testthat::expect_equal(remove_offset_formula(formula(mod3)), fm)
    testthat::expect_equal(remove_offset_formula(formula(mod4)), fm)
    testthat::expect_equal(remove_offset_formula(formula(mod5)), fm)
  }
)


