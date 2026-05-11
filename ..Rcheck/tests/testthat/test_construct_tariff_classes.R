context("derive_tariff_segments")

testthat::test_that("derive_tariff_segments returns tariff segments", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")

  result <- derive_tariff_segments(fit)

  testthat::expect_s3_class(result, "tariff_segments")
  testthat::expect_s3_class(result, "tariff_classes")
  testthat::expect_s3_class(result, "constructtariffclasses")
  testthat::expect_type(result$segment_boundaries, "double")
  testthat::expect_equal(result$segment_boundaries, result$class_boundaries)
  testthat::expect_equal(result$class_boundaries, result$splits)
  testthat::expect_s3_class(result$assigned_segments, "factor")
  testthat::expect_equal(result$assigned_segments, result$assigned_groups)
  testthat::expect_equal(result$assigned_segments, result$tariff_classes)
  testthat::expect_equal(length(result$assigned_segments), length(fit$x_obs))
  testthat::expect_true(all(result$segment_boundaries >= min(fit$x_obs, na.rm = TRUE)))
  testthat::expect_true(all(result$segment_boundaries <= max(fit$x_obs, na.rm = TRUE)))
  testthat::expect_equal(result$risk_factor, "age_policyholder")
  testthat::expect_equal(result$model_type, "frequency")
})

testthat::test_that("decimal risk factors do not create spurious split points", {
  data <- MTPL
  data$age_policyholder_half <- data$age_policyholder + 0.5

  fit <- risk_factor_gam(data, claim_count = "nclaims",
                        risk_factor = "age_policyholder_half",
                        exposure = "exposure")
  result <- derive_tariff_segments(fit)

  testthat::expect_true(all(result$segment_boundaries >= min(fit$x_obs, na.rm = TRUE)))
  testthat::expect_true(all(result$segment_boundaries <= max(fit$x_obs, na.rm = TRUE)))
})

testthat::test_that("autoplot can show confidence intervals", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")
  result <- derive_tariff_segments(fit)

  plot <- autoplot(result, confidence = TRUE)
  built <- ggplot2::ggplot_build(plot)

  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_gte(length(built$data), 3)
})

testthat::test_that("print and as.vector methods expose splits", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")
  result <- derive_tariff_segments(fit)

  testthat::expect_output(print(result), "Tariff segment boundaries")
  testthat::expect_equal(as.vector(result), result$segment_boundaries)
})

testthat::test_that("add_tariff_segments adds assigned segments to portfolio data", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")
  result <- derive_tariff_segments(fit)

  out <- add_tariff_segments(MTPL, result, name = "age_policyholder_segment")

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_s3_class(out$age_policyholder_segment, "factor")
  testthat::expect_equal(out$age_policyholder_segment, result$assigned_segments)
  testthat::expect_error(
    add_tariff_segments(out, result, name = "age_policyholder_segment"),
    "already exists"
  )
  testthat::expect_error(
    add_tariff_segments(MTPL[1:10, ], result, name = "age_policyholder_segment"),
    "same number of rows"
  )
  testthat::expect_true("age_policyholder_segment" %in%
                          names(add_tariff_segments(MTPL, result)))
})

testthat::test_that("deprecated evtree argument names still work", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")

  result <- testthat::expect_warning(
    derive_tariff_segments(
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

  testthat::expect_s3_class(result, "tariff_segments")
})

testthat::test_that("invalid control arguments fail clearly", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")

  testthat::expect_error(
    derive_tariff_segments(fit, complexity = -1),
    "`complexity`"
  )
})
