mtcars1 <- mtcars[, c("cyl", "vs")]
mtcars2 <- mtcars[, c("cyl", "vs", "disp")]
mtcars3 <- mtcars[, c("cyl", "vs", "disp", "gear")]
mtcars4 <- mtcars[, c("cyl", "vs", "disp", "gear", "mpg")]

testthat::test_that(
  "rating_grid works and aggregates correctly for data.frames", {

    res1 <- rating_grid(
      mtcars1,
      group_by = c("cyl", "vs")
    )

    testthat::expect_s3_class(res1, "data.frame")
    testthat::expect_true(all(c("cyl", "vs", "count") %in% names(res1)))
    testthat::expect_equal(
      nrow(res1),
      nrow(unique(mtcars1))
    )
    testthat::expect_equal(sum(res1$count), nrow(mtcars1))

    res2 <- rating_grid(
      mtcars2,
      group_by = c("cyl", "vs"),
      exposure = "disp"
    )

    testthat::expect_s3_class(res2, "data.frame")
    testthat::expect_true(all(c("cyl", "vs", "disp") %in% names(res2)))
    testthat::expect_equal(
      sum(res2$disp, na.rm = TRUE),
      sum(mtcars2$disp, na.rm = TRUE)
    )

    res3 <- rating_grid(
      mtcars3,
      group_by = c("cyl", "vs"),
      exposure = "disp",
      exposure_by = "gear"
    )

    testthat::expect_s3_class(res3, "data.frame")
    testthat::expect_true(all(c("disp_3", "disp_4", "disp_5") %in% names(res3)))

    res4 <- rating_grid(
      mtcars4,
      group_by = c("cyl", "vs"),
      exposure = "disp",
      exposure_by = "gear",
      aggregate_cols = c("mpg")
    )

    testthat::expect_s3_class(res4, "data.frame")
    testthat::expect_true("mpg" %in% names(res4))
    testthat::expect_equal(
      sum(res4$mpg, na.rm = TRUE),
      sum(mtcars4$mpg, na.rm = TRUE)
    )
  }
)

testthat::test_that(
  "rating_grid uses exposure column name when splitting exposure", {
    df <- data.frame(
      a = c("x", "x", NA),
      exposure = c(1, 2, 3),
      year = c(2020, 2021, 2020)
    )

    res <- rating_grid(
      df,
      group_by = "a",
      exposure = "exposure",
      exposure_by = "year"
    )

    testthat::expect_true(all(c("exposure_2020", "exposure_2021") %in% names(res)))
    testthat::expect_false(any(c("1_2020", "2_2021", "3_2020") %in% names(res)))
  }
)

testthat::test_that(
  "rating_grid drops missing group values when requested", {
    df <- data.frame(a = c("x", NA), exposure = c(1, 2))

    res <- rating_grid(
      df,
      group_by = "a",
      exposure = "exposure",
      drop_na = TRUE
    )

    testthat::expect_equal(nrow(res), 1)
    testthat::expect_equal(res$a, "x")
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
  "extract_model_data stores important plain GLM metadata", {
    res <- extract_model_data(model1)

    testthat::expect_s3_class(res, "model_data")
    testthat::expect_equal(attr(res, "response"), "cyl")
    testthat::expect_equal(attr(res, "rf"), c("am", "gear"))
    testthat::expect_equal(attr(res, "offweights"), "mpg")
    testthat::expect_s3_class(attr(res, "terms"), "terms")
  }
)

testthat::test_that(
  "rating_grid infers group_by from plain GLM objects", {
    res <- rating_grid(model1)

    testthat::expect_s3_class(res, "data.frame")
    testthat::expect_true(all(c("am", "gear") %in% names(res)))
    testthat::expect_false(all(names(mtcars5) %in% names(res)))
    testthat::expect_equal(names(res), c("am", "gear", "count", "mpg"))
  }
)

testthat::test_that(
  "deprecated rating_grid argument names remain available", {
    testthat::expect_warning(
      res <- rating_grid(
        mtcars4,
        group_vars = c("cyl", "vs"),
        exposure = "disp",
        agg_cols = "mpg"
      ),
      "deprecated"
    )

    testthat::expect_s3_class(res, "data.frame")
    testthat::expect_true("mpg" %in% names(res))
  }
)

testthat::test_that(
  "rating_grid works for extract_model_data objects", {
    res <- model1 |>
      extract_model_data() |>
      rating_grid()

    testthat::expect_s3_class(res, "data.frame")

    rf <- attr(extract_model_data(model1), "rf")
    testthat::expect_true(all(rf %in% names(res)))
  }
)

testthat::test_that(
  "model_data remains available as a deprecated wrapper", {
    testthat::expect_warning(
      res <- model_data(model1),
      "deprecated"
    )
    testthat::expect_s3_class(res, "model_data")
  }
)

freq <- glm(
  nclaims ~ bm + zip,
  offset = log(exposure),
  family = poisson(),
  data = MTPL
)

sev_data <- MTPL[MTPL$amount > 0, , drop = FALSE]
sev <- glm(
  amount ~ bm + zip,
  weights = nclaims,
  family = Gamma(link = "log"),
  data = sev_data
)

premium_df <- add_prediction(MTPL, freq, sev)
premium_df$premium <- premium_df$pred_nclaims_freq * premium_df$pred_amount_sev

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
    testthat::expect_equal(attr(res, "rf"), c("zip", "bm"))
    testthat::expect_equal(attr(res, "offweights"), "exposure")
  }
)

testthat::test_that(
  "rating_grid works for restricted model_data without cross-joining refinements", {
    res <- burn_rst |>
      extract_model_data() |>
      rating_grid()

    testthat::expect_s3_class(res, "data.frame")
    testthat::expect_true(all(c("bm", "zip", "zip_rst") %in% names(res)))
    testthat::expect_true(nrow(res) <= nrow(premium_df))

    pairs <- unique(res[, c("zip", "zip_rst"), drop = FALSE])
    testthat::expect_equal(nrow(pairs), length(unique(pairs$zip)))
  }
)

testthat::test_that(
  "construct_model_points remains available as a deprecated wrapper", {
    testthat::expect_warning(
      res <- construct_model_points(mtcars1, group_by = c("cyl", "vs")),
      "deprecated"
    )
    testthat::expect_s3_class(res, "data.frame")
  }
)
