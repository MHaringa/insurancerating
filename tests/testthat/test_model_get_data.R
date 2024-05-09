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

context("Get data from restricted model")

library(dplyr)
freq <- glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
            data = MTPL)
sev <- glm(amount ~ bm + zip, weights = nclaims,
            family = Gamma(link = "log"),
            data = MTPL %>% filter(amount > 0))

# Add predictions for freq and sev to data, and calculate premium
premium_df <- MTPL %>%
   add_prediction(freq, sev) %>%
   mutate(premium = pred_nclaims_freq * pred_amount_sev)

# Restrictions on risk factors for region (zip)
zip_df <- data.frame(zip = c(0, 1, 2, 3), zip_rst = c(0.8, 0.9, 1, 1.2))

# Fit unrestricted model
burn <- glm(premium ~ bm + zip, weights = exposure,
            family = Gamma(link = "log"), data = premium_df)

# Fit restricted model
burn_rst <- burn %>%
  restrict_coef(., zip_df) %>%
  update_glm()

testthat::test_that(
  "No errors are returned for updated glm objects with data.frame", {
    testthat::expect_error(model_data(burn_rst), NA)
  }
)

burn_rst2 <- burn_rst
burn_rst2$data <- data.table::data.table(burn_rst2$data)

testthat::test_that(
  "No errors are returned for updated glm objects with data.table", {
    testthat::expect_error(model_data(burn_rst2), NA)
  }
)
