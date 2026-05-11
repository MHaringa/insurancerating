library(insurancerating)
context("overdispersion")

test_that("check_overdispersion returns expected statistics", {
  x <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
           data = MTPL2)
  x_disp <- check_overdispersion(x)

  pearson_chisq <- sum(stats::residuals(x, type = "pearson")^2)
  residual_df <- stats::df.residual(x)

  expect_true(inherits(x_disp, "overdispersion_check"))
  expect_true(inherits(x_disp, "overdispersion"))
  expect_equal(x_disp$pearson_chisq, pearson_chisq)
  expect_equal(x_disp$dispersion_ratio, pearson_chisq / residual_df)
  expect_equal(x_disp$residual_df, residual_df)
  expect_equal(
    x_disp$p_value,
    stats::pchisq(pearson_chisq, df = residual_df, lower.tail = FALSE)
  )

  expect_equal(x_disp$chisq, x_disp$pearson_chisq)
  expect_equal(x_disp$ratio, x_disp$dispersion_ratio)
  expect_equal(x_disp$rdf, x_disp$residual_df)
  expect_equal(x_disp$p, x_disp$p_value)
})

test_that("check_overdispersion validates input", {
  expect_error(
    check_overdispersion(data.frame(x = 1)),
    "`object` must be a fitted glm object.",
    fixed = TRUE
  )

  gamma_model <- glm(amount ~ area, family = Gamma(link = "log"),
                     data = subset(MTPL2, amount > 0))
  expect_error(
    check_overdispersion(gamma_model),
    "`object` must be fitted with a Poisson family.",
    fixed = TRUE
  )

  saturated_data <- data.frame(
    y = c(0, 1, 2),
    group = factor(c("a", "b", "c"))
  )
  saturated_model <- glm(y ~ group, family = poisson(),
                         data = saturated_data)

  expect_error(
    check_overdispersion(saturated_model),
    "`object` must have positive residual degrees of freedom.",
    fixed = TRUE
  )
})

test_that("print.overdispersion_check uses unrounded p-value for conclusion", {
  x <- structure(
    list(
      pearson_chisq = 1,
      dispersion_ratio = 1,
      residual_df = 1,
      p_value = 0.0499,
      chisq = 1,
      ratio = 1,
      rdf = 1,
      p = 0.0499
    ),
    class = c("overdispersion_check", "overdispersion")
  )

  expect_output(
    expect_message(
      print(x, digits = 1),
      "Overdispersion detected.",
      fixed = TRUE
    ),
    "Dispersion ratio",
    fixed = TRUE
  )
})

test_that("print.overdispersion remains compatible with old objects", {
  x <- structure(
    list(chisq = 1, ratio = 1, rdf = 1, p = 0.5),
    class = "overdispersion"
  )

  expect_output(
    expect_message(
      print(x),
      "No overdispersion detected.",
      fixed = TRUE
    ),
    "Dispersion ratio",
    fixed = TRUE
  )
})
