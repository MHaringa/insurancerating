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

testthat::test_that(
  "Input and output in matchColClasses() are of same type", {
    freq <- glm(nclaims ~ bm + zip, weights = power,
                family = poisson(), data = MTPL)
    zip_df <- data.frame(zip = c(0, 1, 2, 3),
                         zip_rst = c(0.8, 0.9, 1, 1.2))
    x <- restrict_coef(freq, zip_df)
    names_rf <- names(x$restrictions_lst)
    name <- names_rf[length(names_rf)]
    naam_rst <- x$restrictions_lst[[name]]
    rf <- x$rating_factors
    naam_rf <- rf[rf$risk_factor == name, ]
    naam_rf <- naam_rf[, 2:3]
    names(naam_rst)[names(naam_rst) == name] <- "level"
    rt <- matchColClasses(naam_rst, naam_rf)
    testthat::expect_type(rt$level, typeof(naam_rst$level))
  })
