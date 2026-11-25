test_that("Poisson intervals use exact count limits and retain zero-claim groups", {
  d <- data.frame(kind = c("zero", "small", "large", "undefined"),
                  y = c(0, 2, 200, 0), e = c(1, 2, 200, 1))
  m <- glm(y ~ 1 + offset(log(e)), poisson(), d)
  expect_false("Lower" %in% names(calibration_analysis(m, d)))
  result <- calibration_analysis(m, d, by = "kind", conf_level = 0.95)
  zero <- result[result$kind == "zero", ]
  expect_equal(zero$Lower, 0)
  expect_equal(zero$Upper, -log(0.025) / zero$Expected)
  small <- result[result$kind == "small", ]
  large <- result[result$kind == "large", ]
  expect_equal(small$OE, large$OE)
  expect_lt(large$Upper - large$Lower, small$Upper - small$Lower)
  expect_equal(small$Lower, qchisq(0.025, 4) / (2 * small$Expected))
  expect_s3_class(autoplot(result), "ggplot")
  expect_error(calibration_analysis(update(m, weights = e), d, conf_level = 0.95),
               "prior-weight structure")
  none <- transform(d, y = 0)
  fit <- glm(y ~ 1, poisson(), none)
  zero_expected <- .calibration_intervals(
    data.frame(Observed = 0, Expected = 0, OE = NA_real_), fit,
    list(weights = 1, raw_observed = 0), 1L, 0.95, "poisson"
  )
  expect_true(is.na(zero_expected$Upper))
})

test_that("Gamma and Tweedie use fitted variance and precision weights", {
  d <- data.frame(y = c(1.5, 1.7, 2.1, 2.3, 2.9, 3),
                  claims = c(1, 5, 1, 3, 2, 3), x = rep(c(0, 1), each = 3))
  for (family in list(Gamma(link = "log"), mgcv::Tweedie(p = 1.5, link = "log"))) {
    m <- glm(y ~ x, family, d, weights = claims)
    result <- calibration_analysis(m, d, conf_level = 0.90)
    expect_identical(result$Interval_method, "wald")
    variance <- sum(d$claims * family$variance(fitted(m) * result$OE)) *
      summary(m)$dispersion
    expected <- exp(log(result$OE) + c(-1, 1) * qnorm(0.95) *
                      sqrt(variance) / sum(d$claims * d$y))
    expect_equal(c(result$Lower, result$Upper), expected)
    expect_error(calibration_analysis(m, d, conf_level = 0.95, interval_method = "poisson"),
                 "No supported calibration interval")
  }
})
