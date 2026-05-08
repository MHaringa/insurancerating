library(insurancerating)
context("residuals")

test_that("check_residuals returns a residual_check object", {
  model <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
               data = MTPL2)

  result <- check_residuals(model, n_simulations = 20)

  expect_true(inherits(result, "residual_check"))
  expect_true(inherits(result, "check_residuals"))
  expect_s3_class(result$qq_data, "data.frame")
  expect_named(result$qq_data, c("x", "y"))
  expect_type(result$scaled_residuals, "double")
  expect_equal(nrow(result$qq_data), length(result$scaled_residuals))
  expect_true(is.na(result$p_value) || is.numeric(result$p_value))

  expect_equal(result$df, result$qq_data)
  expect_equal(result$p.val, result$p_value)
})

test_that("check_residuals validates inputs", {
  expect_error(
    check_residuals(data.frame(x = 1)),
    "`object` must be a fitted glm object.",
    fixed = TRUE
  )

  model <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
               data = MTPL2)

  expect_error(
    check_residuals(model, n_simulations = 0),
    "`n_simulations` must be a positive whole number.",
    fixed = TRUE
  )
  expect_error(
    check_residuals(model, n_simulations = NA_real_),
    "`n_simulations` must be a positive whole number.",
    fixed = TRUE
  )
  expect_error(
    check_residuals(model, n_simulations = 1.5),
    "`n_simulations` must be a positive whole number.",
    fixed = TRUE
  )
})

test_that("print.check_residuals works with new and old objects", {
  result <- structure(
    list(
      qq_data = data.frame(x = c(0.25, 0.75), y = c(0.3, 0.8)),
      scaled_residuals = c(0.3, 0.8),
      p_value = 0.5,
      df = data.frame(x = c(0.25, 0.75), y = c(0.3, 0.8)),
      p.val = 0.5
    ),
    class = c("residual_check", "check_residuals")
  )

  expect_output(
    expect_message(
      print(result),
      "No significant deviations detected.",
      fixed = TRUE
    ),
    "Simulation-based residual check",
    fixed = TRUE
  )

  old_result <- structure(
    list(df = data.frame(x = c(0.25, 0.75), y = c(0.3, 0.8)), p.val = 0.01),
    class = "check_residuals"
  )

  expect_output(
    expect_message(
      print(old_result),
      "Deviations detected: residuals differ from expected distribution.",
      fixed = TRUE
    ),
    "Kolmogorov-Smirnov p-value",
    fixed = TRUE
  )
})

test_that("autoplot.check_residuals returns a ggplot", {
  result <- structure(
    list(
      qq_data = data.frame(x = seq(0.01, 0.99, length.out = 20),
                           y = seq(0.01, 0.99, length.out = 20)),
      scaled_residuals = seq(0.01, 0.99, length.out = 20),
      p_value = 0.5,
      df = data.frame(x = seq(0.01, 0.99, length.out = 20),
                      y = seq(0.01, 0.99, length.out = 20)),
      p.val = 0.5
    ),
    class = c("residual_check", "check_residuals")
  )

  plot <- autoplot(result, show_message = FALSE, max_points = 5)

  expect_s3_class(plot, "ggplot")
  expect_equal(nrow(plot$data), 5)
})

test_that("autoplot.check_residuals validates inputs", {
  result <- structure(
    list(qq_data = data.frame(x = 0.5, y = 0.5), p_value = 0.5),
    class = c("residual_check", "check_residuals")
  )

  expect_error(
    insurancerating:::autoplot.check_residuals(data.frame(x = 1)),
    "Input must be of class 'check_residuals'.",
    fixed = TRUE
  )
  expect_error(
    autoplot(result, show_message = NA),
    "`show_message` must be TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    autoplot(result, show_message = FALSE, max_points = 0),
    "`max_points` must be a positive whole number or Inf.",
    fixed = TRUE
  )
})
