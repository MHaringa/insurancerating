context("riskfactor_gam")

test_that("frequency model returns riskfactor_gam object", {
  fit <- risk_factor_gam(MTPL,
                        claim_count = "nclaims",
                        risk_factor = "age_policyholder",
                        exposure = "exposure")

  expect_s3_class(fit, "riskfactor_gam")
  expect_s3_class(fit, "risk_factor_gam")
  expect_s3_class(fit, "fitgam")
  expect_equal(fit$model, "frequency")
  expect_named(fit$prediction, c("x", "fitted", "conf_low", "conf_high"))
  expect_true(all(c("pred", "frequency") %in% names(fit$data)))
})

test_that("severity model returns observed severity data", {
  fit <- risk_factor_gam(MTPL,
                        claim_count = "nclaims",
                        risk_factor = "age_policyholder",
                        exposure = "exposure",
                        claim_amount = "amount",
                        model = "severity")

  expect_s3_class(fit, "riskfactor_gam")
  expect_equal(fit$model, "severity")
  expect_true("avg_claimsize" %in% names(fit$data))
})

test_that("pure premium model replaces burning model name", {
  data <- MTPL
  data$pure_premium <- data$amount / data$exposure

  fit <- risk_factor_gam(data,
                        claim_count = "nclaims",
                        risk_factor = "age_policyholder",
                        exposure = "exposure",
                        pure_premium = "pure_premium",
                        model = "pure_premium")

  expect_s3_class(fit, "riskfactor_gam")
  expect_equal(fit$model, "pure_premium")
  expect_true("avg_premium" %in% names(fit$data))
})

test_that("burning model name remains supported with warning", {
  data <- MTPL
  data$pure_premium <- data$amount / data$exposure

  fit <- expect_warning(
    risk_factor_gam(data,
                   claim_count = "nclaims",
                   risk_factor = "age_policyholder",
                   exposure = "exposure",
                   pure_premium = "pure_premium",
                   model = "burning"),
    "burning"
  )

  expect_equal(fit$model, "pure_premium")
})

test_that("required model inputs fail clearly", {
  expect_error(
    risk_factor_gam(MTPL,
                   claim_count = "nclaims",
                   risk_factor = "does_not_exist",
                   exposure = "exposure"),
    "missing"
  )

  expect_error(
    risk_factor_gam(MTPL,
                   claim_count = "nclaims",
                   risk_factor = "age_policyholder",
                   exposure = "exposure",
                   model = "severity"),
    "Required column arguments"
  )

  expect_error(
    risk_factor_gam(MTPL,
                   claim_count = "nclaims",
                   risk_factor = "age_policyholder",
                   exposure = "exposure",
                   model = "pure_premium"),
    "Required column arguments"
  )

  expect_error(
    risk_factor_gam(MTPL,
                   claim_count = "nclaims",
                   risk_factor = "age_policyholder",
                   exposure = "exposure",
                   pure_premium = "does_not_exist",
                   model = "pure_premium"),
    "missing"
  )
})

test_that("round_x groups observed risk factor values", {
  fit <- risk_factor_gam(MTPL,
                        claim_count = "nclaims",
                        risk_factor = "age_policyholder",
                        exposure = "exposure",
                        round_risk_factor = 5)

  expect_true(all(fit$x_obs %% 5 == 0))
})

test_that("deprecated fit_gam wrapper still works", {
  fit <- expect_warning(
    fit_gam(MTPL,
            nclaims = nclaims,
            x = age_policyholder,
            exposure = exposure),
    "deprecated"
  )

  expect_s3_class(fit, "riskfactor_gam")
  expect_equal(fit$model, "frequency")
})

test_that("deprecated riskfactor_gam name and argument aliases still work", {
  fit <- expect_warning(
    riskfactor_gam(MTPL,
                   nclaims = "nclaims",
                   x = "age_policyholder",
                   exposure = "exposure"),
    "deprecated"
  )

  expect_s3_class(fit, "riskfactor_gam")
  expect_equal(fit$model, "frequency")
})

test_that("deprecated risk_factor_gam argument aliases still work", {
  fit <- expect_warning(
    risk_factor_gam(MTPL,
                    nclaims = "nclaims",
                    x = "age_policyholder",
                    exposure = "exposure",
                    round_x = 5),
    "deprecated"
  )

  expect_s3_class(fit, "riskfactor_gam")
  expect_true(all(fit$x_obs %% 5 == 0))
})

test_that("summary, as.data.frame and autoplot methods work", {
  fit <- risk_factor_gam(MTPL,
                        claim_count = "nclaims",
                        risk_factor = "age_policyholder",
                        exposure = "exposure")

  expect_output(summary(fit), "Generalized Additive Model")
  expect_equal(as.data.frame(fit), fit$prediction)

  plot <- autoplot(fit, confidence = TRUE, show_observations = TRUE)
  expect_s3_class(plot, "ggplot")
  expect_gte(length(ggplot2::ggplot_build(plot)$data), 3)
})
