context("construct_tariff_classes")

testthat::test_that("construct_tariff_classes returns tariff classes", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")

  result <- construct_tariff_classes(fit)

  testthat::expect_s3_class(result, "tariff_classes")
  testthat::expect_s3_class(result, "constructtariffclasses")
  testthat::expect_type(result$splits, "double")
  testthat::expect_s3_class(result$tariff_classes, "factor")
  testthat::expect_equal(length(result$tariff_classes), length(fit$x_obs))
  testthat::expect_true(all(result$splits >= min(fit$x_obs, na.rm = TRUE)))
  testthat::expect_true(all(result$splits <= max(fit$x_obs, na.rm = TRUE)))
})

testthat::test_that("decimal risk factors do not create spurious split points", {
  data <- MTPL
  data$age_policyholder_half <- data$age_policyholder + 0.5

  fit <- risk_factor_gam(data, claim_count = "nclaims",
                        risk_factor = "age_policyholder_half",
                        exposure = "exposure")
  result <- construct_tariff_classes(fit)

  testthat::expect_true(all(result$splits >= min(fit$x_obs, na.rm = TRUE)))
  testthat::expect_true(all(result$splits <= max(fit$x_obs, na.rm = TRUE)))
})

testthat::test_that("autoplot can show confidence intervals", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")
  result <- construct_tariff_classes(fit)

  plot <- autoplot(result, confidence = TRUE)
  built <- ggplot2::ggplot_build(plot)

  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_gte(length(built$data), 3)
})

testthat::test_that("print and as.vector methods expose splits", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")
  result <- construct_tariff_classes(fit)

  testthat::expect_output(print(result), "Tariff class splits")
  testthat::expect_equal(as.vector(result), result$splits)
})

testthat::test_that("deprecated evtree argument names still work", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")

  result <- testthat::expect_warning(
    construct_tariff_classes(
      fit,
      alpha = 0,
      niterations = 1000,
      ntrees = 50
    ),
    "deprecated"
  )

  testthat::expect_s3_class(result, "tariff_classes")
})

testthat::test_that("invalid control arguments fail clearly", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")

  testthat::expect_error(
    construct_tariff_classes(fit, complexity = -1),
    "`complexity`"
  )
})
