library(insurancerating)
context("Construct model points")

mtcars1 <- mtcars[, c("cyl", "vs")]
mtcars2 <- mtcars[, c("cyl", "vs", "disp")]
mtcars3 <- mtcars[, c("cyl", "vs", "disp", "gear")]
mtcars4 <- mtcars[, c("cyl", "vs", "disp", "gear", "mpg")]

testthat::test_that(
  "No errors are returned for data.frames", {
    testthat::expect_error(construct_model_points(mtcars1), NA)
    testthat::expect_error(construct_model_points(mtcars2, exposure = disp), NA)
    testthat::expect_error(construct_model_points(mtcars3, exposure = disp,
                                                  exposure_by = gear), NA)
    testthat::expect_error(construct_model_points(mtcars4, exposure = disp,
                                                  exposure_by = gear,
                                                  agg_cols = list(mpg)), NA)
  }
)

mtcars5 <- mtcars
mtcars5$am <- as.factor(mtcars5$am)
mtcars5$gear <- as.factor(mtcars5$gear)

model1 <- glm(cyl ~ am + gear, offset = log(mpg), family = "poisson",
              data = mtcars5)

testthat::test_that(
  "No errors are returned for glm objects", {
    testthat::expect_error(construct_model_points(model_data(model1)), NA)
    testthat::expect_error(construct_model_points(model_data(model1),
                                                  exposure = qsec), NA)
    testthat::expect_error(construct_model_points(model_data(model1),
                                                  exposure = qsec,
                                                  exposure_by = vs), NA)
    testthat::expect_error(construct_model_points(model_data(model1),
                                                  exposure = qsec,
                                                  exposure_by = vs,
                                                  agg_cols = list(wt)), NA)
  }
)




