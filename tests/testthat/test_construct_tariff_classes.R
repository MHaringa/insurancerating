context("derive_tariff_groups")

testthat::test_that("derive_tariff_groups returns tariff groups", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")

  result <- derive_tariff_groups(fit)

  testthat::expect_s3_class(result, "tariff_groups")
  testthat::expect_s3_class(result, "tariff_classes")
  testthat::expect_s3_class(result, "constructtariffclasses")
  testthat::expect_type(result$class_boundaries, "double")
  testthat::expect_equal(result$class_boundaries, result$splits)
  testthat::expect_s3_class(result$assigned_groups, "factor")
  testthat::expect_equal(result$assigned_groups, result$tariff_classes)
  testthat::expect_equal(length(result$assigned_groups), length(fit$x_obs))
  testthat::expect_true(all(result$class_boundaries >= min(fit$x_obs, na.rm = TRUE)))
  testthat::expect_true(all(result$class_boundaries <= max(fit$x_obs, na.rm = TRUE)))
  testthat::expect_equal(result$risk_factor, "age_policyholder")
  testthat::expect_equal(result$model_type, "frequency")
})

testthat::test_that("decimal risk factors do not create spurious split points", {
  data <- MTPL
  data$age_policyholder_half <- data$age_policyholder + 0.5

  fit <- risk_factor_gam(data, claim_count = "nclaims",
                        risk_factor = "age_policyholder_half",
                        exposure = "exposure")
  result <- derive_tariff_groups(fit)

  testthat::expect_true(all(result$class_boundaries >= min(fit$x_obs, na.rm = TRUE)))
  testthat::expect_true(all(result$class_boundaries <= max(fit$x_obs, na.rm = TRUE)))
})

testthat::test_that("autoplot can show confidence intervals", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")
  result <- derive_tariff_groups(fit)

  plot <- autoplot(result, confidence = TRUE)
  built <- ggplot2::ggplot_build(plot)

  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_gte(length(built$data), 3)
})

testthat::test_that("print and as.vector methods expose splits", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")
  result <- derive_tariff_groups(fit)

  testthat::expect_output(print(result), "Tariff group boundaries")
  testthat::expect_equal(as.vector(result), result$class_boundaries)
})

testthat::test_that("add_tariff_groups adds assigned groups to portfolio data", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")
  result <- derive_tariff_groups(fit)

  out <- add_tariff_groups(MTPL, result, name = "age_policyholder_group")

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_s3_class(out$age_policyholder_group, "factor")
  testthat::expect_equal(out$age_policyholder_group, result$assigned_groups)
  testthat::expect_error(
    add_tariff_groups(out, result, name = "age_policyholder_group"),
    "already exists"
  )
  testthat::expect_error(
    add_tariff_groups(MTPL[1:10, ], result, name = "age_policyholder_group"),
    "same number of rows"
  )
})

testthat::test_that("deprecated evtree argument names still work", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")

  result <- testthat::expect_warning(
    derive_tariff_groups(
      fit,
      alpha = 0,
      niterations = 1000,
      ntrees = 50
    ),
    "deprecated"
  )

  testthat::expect_s3_class(result, "tariff_classes")
})

testthat::test_that("construct_tariff_classes remains available as deprecated alias", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")

  result <- testthat::expect_warning(
    construct_tariff_classes(fit),
    "deprecated"
  )

  testthat::expect_s3_class(result, "tariff_groups")
})

testthat::test_that("invalid control arguments fail clearly", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")

  testthat::expect_error(
    derive_tariff_groups(fit, complexity = -1),
    "`complexity`"
  )
})
