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
  segment_summary <- summary(result)
  testthat::expect_s3_class(segment_summary, "data.frame")
  testthat::expect_identical(segment_summary, result$segment_summary)
  testthat::expect_true(all(c(
    "segment", "portfolio_records", "risk_factor_values",
    "exposure", "claim_count", "frequency"
  ) %in% names(segment_summary)))
  testthat::expect_equal(
    sum(segment_summary$portfolio_records),
    nrow(MTPL)
  )
  testthat::expect_equal(result$segmentation_penalty, 0)
  testthat::expect_equal(
    segment_summary$frequency,
    segment_summary$claim_count / segment_summary$exposure
  )
})

testthat::test_that("segment summaries contain the observed GAM response", {
  severity_fit <- risk_factor_gam(
    MTPL,
    claim_count = "nclaims",
    risk_factor = "age_policyholder",
    exposure = "exposure",
    claim_amount = "amount",
    model = "severity"
  )
  severity_segments <- suppressWarnings(derive_tariff_segments(
    severity_fit,
    max_iterations = 100,
    population_size = 20
  ))
  severity_summary <- summary(severity_segments)

  testthat::expect_true(all(c(
    "claim_count", "claim_amount", "average_severity"
  ) %in% names(severity_summary)))
  testthat::expect_equal(
    severity_summary$average_severity,
    severity_summary$claim_amount / severity_summary$claim_count
  )

  premium_data <- MTPL
  premium_data$technical_risk_premium <-
    premium_data$amount / premium_data$exposure
  premium_fit <- risk_factor_gam(
    premium_data,
    risk_factor = "age_policyholder",
    exposure = "exposure",
    pure_premium = "technical_risk_premium",
    model = "pure_premium"
  )
  premium_segments <- suppressWarnings(derive_tariff_segments(
    premium_fit,
    max_iterations = 100,
    population_size = 20
  ))
  premium_summary <- summary(premium_segments)

  testthat::expect_true(all(c(
    "exposure", "risk_premium_amount", "risk_premium"
  ) %in% names(premium_summary)))
  testthat::expect_equal(
    premium_summary$risk_premium,
    premium_summary$risk_premium_amount / premium_summary$exposure
  )
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

testthat::test_that("tariff segment autoplot reuses the GAM curve", {
  fit <- risk_factor_gam(
    MTPL,
    claim_count = "nclaims",
    risk_factor = "age_policyholder",
    exposure = "exposure"
  )
  segments <- derive_tariff_segments(fit)

  gam_plot <- autoplot(fit)
  segment_plot <- autoplot(segments)
  curve_only_plot <- autoplot(
    segments,
    show_segments = FALSE,
    x_stepsize = 10
  )

  gam_built <- ggplot2::ggplot_build(gam_plot)
  segment_built <- ggplot2::ggplot_build(segment_plot)
  curve_only_built <- ggplot2::ggplot_build(curve_only_plot)

  testthat::expect_gt(length(segment_built$data), length(curve_only_built$data))
  testthat::expect_identical(length(curve_only_built$data), 1L)
  testthat::expect_equal(curve_only_built$data[[1]]$x, gam_built$data[[1]]$x)
  testthat::expect_equal(curve_only_built$data[[1]]$y, gam_built$data[[1]]$y)
  testthat::expect_error(
    autoplot(segments, show_segments = NA),
    "show_segments"
  )
})

testthat::test_that("print and as.vector methods expose splits", {
  fit <- risk_factor_gam(MTPL, claim_count = "nclaims", risk_factor = "age_policyholder",
                        exposure = "exposure")
  result <- derive_tariff_segments(fit)

  testthat::expect_output(print(result), "Tariff segmentation")
  testthat::expect_equal(as.vector(result), result$segment_boundaries)

  legacy <- result
  class(legacy) <- "tariff_classes"
  testthat::expect_identical(summary(legacy), result$segment_summary)
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
  subset_out <- add_tariff_segments(
    MTPL[1:10, ],
    result,
    name = "age_policyholder_segment"
  )
  testthat::expect_equal(nrow(subset_out), 10L)

  shuffled <- MTPL[rev(seq_len(nrow(MTPL))), ]
  shuffled_out <- add_tariff_segments(
    shuffled,
    result,
    name = "age_policyholder_segment"
  )
  testthat::expect_equal(
    shuffled_out$age_policyholder_segment,
    cut(
      shuffled$age_policyholder,
      breaks = result$segment_boundaries,
      include.lowest = TRUE
    )
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
    derive_tariff_segments(fit, segmentation_penalty = -1),
    "`segmentation_penalty`"
  )
  testthat::expect_error(
    derive_tariff_segments(fit, max_iterations = 10.5),
    "positive whole number"
  )
  testthat::expect_error(
    derive_tariff_segments(fit, population_size = 10.5),
    "positive whole number"
  )
  testthat::expect_error(
    derive_tariff_segments(fit, seed = 1.5),
    "finite whole number"
  )
  deprecated_penalty <- testthat::expect_warning(
    derive_tariff_segments(
      fit,
      complexity = 1,
      max_iterations = 100,
      population_size = 20
    ),
    "deprecated"
  )
  testthat::expect_equal(deprecated_penalty$segmentation_penalty, 1)
  testthat::expect_error(
    suppressWarnings(derive_tariff_segments(
      fit,
      segmentation_penalty = 1,
      complexity = 1
    )),
    "Use only one"
  )
  testthat::expect_error(
    suppressWarnings(derive_tariff_segments(
      fit,
      max_iterations = 100,
      niterations = 100
    )),
    "Use only one"
  )
  testthat::expect_error(
    suppressWarnings(derive_tariff_segments(
      fit,
      population_size = 20,
      ntrees = 20
    )),
    "Use only one"
  )
})

testthat::test_that("the segmentation search is reproducible", {
  fit <- risk_factor_gam(
    MTPL,
    claim_count = "nclaims",
    risk_factor = "age_policyholder",
    exposure = "exposure"
  )

  result1 <- derive_tariff_segments(
    fit,
    segmentation_penalty = 1,
    seed = 42,
    max_iterations = 500,
    population_size = 30
  )
  result2 <- derive_tariff_segments(
    fit,
    segmentation_penalty = 1,
    seed = 42,
    max_iterations = 500,
    population_size = 30
  )

  testthat::expect_equal(
    result1$segment_boundaries,
    result2$segment_boundaries
  )
  testthat::expect_equal(
    sum(summary(result1)$exposure),
    sum(fit$data$exposure)
  )
})

testthat::test_that("derive_tariff_segments validates object contents", {
  fit <- risk_factor_gam(
    MTPL,
    claim_count = "nclaims",
    risk_factor = "age_policyholder",
    exposure = "exposure"
  )

  current_class_only <- fit
  class(current_class_only) <- "risk_factor_gam"
  testthat::expect_s3_class(
    derive_tariff_segments(
      current_class_only,
      max_iterations = 100,
      population_size = 20
    ),
    "tariff_segments"
  )

  incomplete <- fit
  incomplete$prediction <- NULL
  testthat::expect_error(
    derive_tariff_segments(incomplete),
    "Missing component: `prediction`"
  )

  invalid_values <- fit
  invalid_values$x_obs[1] <- Inf
  testthat::expect_error(
    derive_tariff_segments(invalid_values),
    "missing or non-finite"
  )

  invalid_prediction <- fit
  invalid_prediction$data$pred[1] <- NA_real_
  testthat::expect_error(
    derive_tariff_segments(invalid_prediction),
    "missing or non-finite GAM values"
  )
})

testthat::test_that("add_tariff_segments rejects unsupported new values", {
  fit <- risk_factor_gam(
    MTPL,
    claim_count = "nclaims",
    risk_factor = "age_policyholder",
    exposure = "exposure"
  )
  segments <- derive_tariff_segments(fit)
  new_data <- MTPL[1:2, ]
  new_data$age_policyholder[1] <- max(segments$segment_boundaries) + 1

  testthat::expect_error(
    add_tariff_segments(new_data, segments),
    "outside the tariff-segment range"
  )

  missing_column <- new_data
  missing_column$age_policyholder <- NULL
  testthat::expect_error(
    add_tariff_segments(missing_column, segments),
    "was not found in `data`"
  )

  missing_value <- MTPL[1:2, ]
  missing_value$age_policyholder[1] <- NA_real_
  testthat::expect_error(
    add_tariff_segments(missing_value, segments),
    "missing or non-finite"
  )
})
