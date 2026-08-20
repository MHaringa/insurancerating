calibration_fixture <- function() {
  portfolio <- data.frame(
    claims = c(0, 1, 2, 1, 3, 2, 4, 1),
    exposure = c(1, 1, 2, 1, 2, 1, 3, 1),
    region = factor(rep(c("A", "B"), 4)),
    vehicle = factor(rep(c("small", "large"), each = 4))
  )
  mod_initial <- glm(
    claims ~ region + vehicle + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  restrictions <- data.frame(
    region = c("A", "B"),
    region_restricted = c(1, 1.15)
  )
  mod_refined <- mod_initial |>
    prepare_refinement(data = portfolio) |>
    add_restriction(restrictions) |>
    refit(intercept_only = TRUE)

  list(
    data = portfolio,
    restrictions = restrictions,
    initial = mod_initial,
    refined = mod_refined
  )
}

testthat::test_that("calibration changes only the overall prediction level", {
  fixture <- calibration_fixture()
  refined <- fixture$refined
  original_coefficients <- stats::coef(refined)
  original_predictions <- stats::predict(refined, type = "response")

  unchanged <- calibrate_model(refined, factor = 1)
  increased <- calibrate_model(refined, factor = 1.05)
  decreased <- calibrate_model(refined, factor = 0.95)

  testthat::expect_equal(
    stats::predict(unchanged, type = "response"),
    original_predictions
  )
  testthat::expect_equal(
    unname(stats::predict(increased, type = "response") / original_predictions),
    rep(1.05, length(original_predictions))
  )
  testthat::expect_equal(
    unname(stats::predict(decreased, type = "response") / original_predictions),
    rep(0.95, length(original_predictions))
  )
  testthat::expect_equal(
    stats::coef(increased)[-1],
    original_coefficients[-1]
  )
  testthat::expect_equal(
    unname(stats::coef(increased)[1]),
    unname(original_coefficients[1]) + log(1.05)
  )
  testthat::expect_equal(stats::coef(refined), original_coefficients)
  testthat::expect_equal(
    stats::predict(refined, type = "response"),
    original_predictions
  )
})

testthat::test_that("calibration metadata and fitted components are consistent", {
  fixture <- calibration_fixture()
  calibrated <- calibrate_model(fixture$refined, factor = 1.10)

  testthat::expect_s3_class(calibrated, "calibrated_model")
  testthat::expect_s3_class(calibrated, "glm")
  testthat::expect_equal(attr(calibrated, "calibration_factor"), 1.10)
  testthat::expect_equal(
    attr(calibrated, "calibration_intercept"),
    attr(calibrated, "calibration_original_intercept") + log(1.10)
  )
  testthat::expect_equal(
    calibrated$fitted.values,
    calibrated$family$linkinv(calibrated$linear.predictors)
  )
  expected_deviance <- sum(calibrated$family$dev.resids(
    calibrated$y,
    calibrated$fitted.values,
    calibrated$prior.weights
  ))
  testthat::expect_equal(calibrated$deviance, expected_deviance)

  printed <- paste(capture.output(print(calibrated)), collapse = "\n")
  testthat::expect_match(printed, "Calibrated refined generalized linear model")
  testthat::expect_match(printed, "Calibration factor: 1.1")
})

testthat::test_that("rating_table uses the calibrated intercept only", {
  fixture <- calibration_fixture()
  calibrated <- calibrate_model(fixture$refined, factor = 1.10)
  refined_table <- as.data.frame(rating_table(fixture$refined, exposure = FALSE))
  calibrated_table <- as.data.frame(rating_table(calibrated, exposure = FALSE))

  estimate_refined <- names(refined_table)[3]
  estimate_calibrated <- names(calibrated_table)[3]
  intercept_refined <- refined_table$risk_factor == "(Intercept)"
  intercept_calibrated <- calibrated_table$risk_factor == "(Intercept)"

  testthat::expect_equal(
    calibrated_table[[estimate_calibrated]][intercept_calibrated],
    refined_table[[estimate_refined]][intercept_refined] * 1.10
  )
  testthat::expect_equal(
    calibrated_table[[estimate_calibrated]][!intercept_calibrated],
    refined_table[[estimate_refined]][!intercept_refined]
  )
})

testthat::test_that("add_prediction uses the calibrated model state", {
  fixture <- calibration_fixture()
  calibrated <- calibrate_model(fixture$refined, factor = 1.05)
  prediction_data <- fixture$refined$data

  refined_prediction <- add_prediction(
    prediction_data,
    fixture$refined,
    predictions = "refined"
  )$refined
  calibrated_prediction <- add_prediction(
    prediction_data,
    calibrated,
    predictions = "calibrated"
  )$calibrated

  testthat::expect_equal(
    calibrated_prediction / refined_prediction,
    rep(1.05, nrow(prediction_data))
  )
})

testthat::test_that("calibration enforces the final workflow position", {
  fixture <- calibration_fixture()
  calibrated <- calibrate_model(fixture$refined, factor = 1.05)

  testthat::expect_error(
    calibrate_model(fixture$initial, factor = 1.05),
    "returned by `refit"
  )
  testthat::expect_error(
    calibrate_model(calibrated, factor = 1.02),
    "already been calibrated"
  )
  testthat::expect_error(
    prepare_refinement(calibrated),
    "cannot be applied after calibration"
  )
  testthat::expect_error(
    add_restriction(calibrated, fixture$restrictions),
    "cannot be applied after calibration"
  )
})

testthat::test_that("calibration factors and model links are validated", {
  fixture <- calibration_fixture()
  invalid <- list(0, -1, NA_real_, Inf, c(1, 1.1), "1.05")
  for (factor in invalid) {
    testthat::expect_error(
      calibrate_model(fixture$refined, factor = factor),
      "one finite numeric value greater than 0"
    )
  }

  gaussian_model <- glm(
    claims ~ region,
    family = gaussian(link = "identity"),
    data = fixture$data
  )
  class(gaussian_model) <- c("refitrestricted", class(gaussian_model))
  testthat::expect_error(
    calibrate_model(gaussian_model, factor = 1.05),
    "requires a refined GLM with a log link"
  )
})
