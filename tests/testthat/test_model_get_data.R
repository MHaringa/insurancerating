mtcars1 <- mtcars[, c("cyl", "vs")]
mtcars2 <- mtcars[, c("cyl", "vs", "disp")]
mtcars3 <- mtcars[, c("cyl", "vs", "disp", "gear")]
mtcars4 <- mtcars[, c("cyl", "vs", "disp", "gear", "mpg")]

testthat::test_that(
  "rating_grid works and aggregates correctly for data.frames", {

    res1 <- rating_grid(
      mtcars1,
      group_vars = c("cyl", "vs")
    )

    testthat::expect_s3_class(res1, "data.frame")
    testthat::expect_true(all(c("cyl", "vs", "count") %in% names(res1)))
    testthat::expect_equal(
      nrow(res1),
      nrow(unique(mtcars1))
    )

    res2 <- rating_grid(
      mtcars2,
      group_vars = c("cyl", "vs"),
      exposure = "disp"
    )

    testthat::expect_s3_class(res2, "data.frame")
    testthat::expect_true(all(c("cyl", "vs", "disp") %in% names(res2)))

    # check aggregation: sum disp gelijk aan origineel
    testthat::expect_equal(
      sum(res2$disp, na.rm = TRUE),
      sum(mtcars2$disp, na.rm = TRUE)
    )

    res3 <- rating_grid(
      mtcars3,
      group_vars = c("cyl", "vs"),
      exposure = "disp",
      exposure_by = "gear"
    )

    testthat::expect_s3_class(res3, "data.frame")
    testthat::expect_true(any(grepl("^disp_", names(res3))))

    res4 <- rating_grid(
      mtcars4,
      group_vars = c("cyl", "vs"),
      exposure = "disp",
      exposure_by = "gear",
      agg_cols = c("mpg")
    )

    testthat::expect_s3_class(res4, "data.frame")
    testthat::expect_true("mpg" %in% names(res4))
  }
)

mtcars5 <- mtcars
mtcars5$am <- as.factor(mtcars5$am)
mtcars5$gear <- as.factor(mtcars5$gear)

model1 <- glm(
  cyl ~ am + gear,
  offset = log(mpg),
  family = "poisson",
  data = mtcars5
)

testthat::test_that(
  "rating_grid infers group_vars from model_data when rf is missing", {
    res <- model1 |>
      rating_grid()

    testthat::expect_s3_class(res, "data.frame")
    testthat::expect_true(all(c("am", "gear") %in% names(res)))
  }
)

testthat::test_that(
  "rating_grid works for extract_model_data objects", {

    res <- model1 |>
      model_data() |>
      rating_grid()

    testthat::expect_s3_class(res, "data.frame")

    # check dat grouping variabelen aanwezig zijn
    rf <- attr(extract_model_data(model1), "rf")
    testthat::expect_true(all(rf %in% names(res)))
  }
)

library(dplyr)

freq <- glm(
  nclaims ~ bm + zip,
  offset = log(exposure),
  family = poisson(),
  data = MTPL
)

sev <- glm(
  amount ~ bm + zip,
  weights = nclaims,
  family = Gamma(link = "log"),
  data = MTPL %>% filter(amount > 0)
)

premium_df <- MTPL |>
  add_prediction(freq, sev) |>
  mutate(premium = pred_nclaims_freq * pred_amount_sev)

zip_df <- data.frame(
  zip = c(0, 1, 2, 3),
  zip_rst = c(0.8, 0.9, 1, 1.2)
)

burn <- glm(
  premium ~ bm + zip,
  weights = exposure,
  family = Gamma(link = "log"),
  data = premium_df
)

burn_rst <- prepare_refinement(burn) |>
  add_restriction(zip_df) |>
  refit()

testthat::test_that(
  "extract_model_data works for restricted glm objects", {

    res <- burn_rst |>
      extract_model_data()

    testthat::expect_s3_class(res, "model_data")
  }
)

testthat::test_that(
  "rating_grid works for restricted model_data", {

    res <- burn_rst |>
      model_data() |>
      rating_grid()

    testthat::expect_s3_class(res, "data.frame")

    # check dat belangrijke kolommen aanwezig zijn
    testthat::expect_true(all(c("bm", "zip") %in% names(res)))

    # check dat aantal rijen kleiner is dan origineel (aggregatie)
    testthat::expect_true(nrow(res) <= nrow(premium_df))
  }
)
