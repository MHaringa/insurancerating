resampling_fixture <- function() {
  data.frame(
    claims = c(0, 2, 1, 4, 2, 3, 4, 1, 3, 2, 5, 3),
    severity = c(180, 150, 160, 300, 250, 375, 280, 220, 340, 400, 390, 500),
    z = seq_len(12),
    exposure = c(0.5, 1, 2, 0.75, 1.5, 2.5, 1, 2, 1.25, 1, 3, 2),
    volume = c(4, 1, 2, 3, 2, 1, 3, 5, 2, 1, 2, 6)
  )
}

test_that("deprecated bootstrap_performance forwards named arguments and warns", {
  portfolio <- resampling_fixture()
  model <- glm(claims ~ z + offset(log(exposure)), poisson(), data = portfolio)
  set.seed(61)
  expected <- validate_performance(model, portfolio, n_resamples = 3,
                                    show_progress = FALSE)
  set.seed(61)
  expect_warning(
    actual <- bootstrap_performance(data = portfolio, model = model,
                                     n_resamples = 3, show_progress = FALSE),
    "validate_performance",
    class = "deprecatedWarning"
  )
  expect_identical(actual, expected)
})

test_that("the performance alias accepts old argument names with one warning", {
  portfolio <- resampling_fixture()
  model <- glm(claims ~ z, poisson(), data = portfolio)
  set.seed(31)
  expected <- validate_performance(model, portfolio, n_resamples = 3,
                                   sample_fraction = 0.75, show_progress = FALSE)
  set.seed(31)
  warnings <- list()
  actual <- withCallingHandlers(
    bootstrap_performance(model = model, data = portfolio, n = 3, frac = 0.75,
                          show_progress = FALSE),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  expect_s3_class(warnings[[1L]], "deprecatedWarning")
  expect_match(conditionMessage(warnings[[1L]]), "validate_performance")
  expect_identical(actual, expected)
})

test_that("model validation exports only the requested primary and alias names", {
  exports <- getNamespaceExports("insurancerating")
  expect_true(all(c(
    "model_performance", "validate_performance", "coefficient_stability",
    "calibration_analysis", "check_residuals", "check_overdispersion",
    "bootstrap_performance", "bootstrap_coefficients"
  ) %in% exports))
  expect_false(any(c("resample_performance", "check_calibration", "bootstrap_rmse") %in% exports))
})

test_that("resampling scores the same training, out-of-bag and held-out rows", {
  portfolio <- resampling_fixture()
  model <- glm(claims ~ z, poisson(), data = portfolio,
               offset = log(exposure), weights = 2 * volume)
  metrics <- c("deviance", "rmse", "mae")
  for (sampling in c("bootstrap", "split")) {
    for (fraction in c(1, 0.6)) {
      set.seed(92)
      result <- validate_performance(
        model, portfolio, n_resamples = 3, sample_fraction = fraction,
        sampling = sampling, metrics = metrics, show_progress = FALSE
      )
      set.seed(92)
      legacy <- validate_performance(
        model, portfolio, n_resamples = 3, sample_fraction = fraction,
        sampling = sampling, show_progress = FALSE
      )
      expect_named(result$scores, metrics)
      expect_named(result$original_scores, metrics)
      expect_identical(result$metric, metrics)
      expect_identical(result$scores$rmse, legacy$rmse_bs)
      expect_identical(result$rmse_bs, legacy$rmse_bs)
      expect_identical(result$rmse_mod, legacy$rmse_mod)

      set.seed(92)
      for (replicate in seq_len(3)) {
        rows <- seq_len(nrow(portfolio))
        train_rows <- sample(rows, floor(fraction * length(rows)),
                             replace = sampling == "bootstrap")
        eval_rows <- if (fraction < 1) setdiff(rows, train_rows) else train_rows
        train <- portfolio[train_rows, , drop = FALSE]
        assessment <- portfolio[eval_rows, , drop = FALSE]
        fitted <- glm(claims ~ z, poisson(), data = train,
                      offset = log(exposure), weights = 2 * volume)
        mu <- predict(fitted, assessment, type = "response")
        y <- assessment$claims
        weight <- 2 * assessment$volume
        term <- ifelse(y == 0, 0, y * log(y / mu))
        expected_deviance <- 2 * sum(weight * (term - y + mu))
        expect_equal(result$scores$deviance[replicate], expected_deviance)
        expect_equal(result$scores$rmse[replicate], sqrt(mean((y - mu)^2)))
        expect_equal(result$scores$mae[replicate], mean(abs(y - mu)))
        expect_equal(legacy$rmse_bs[replicate], sqrt(mean((y - mu)^2)))
      }
    }
  }
  expect_equal(result$original_scores[["deviance"]], stats::deviance(model))
  expect_equal(result$original_scores[["mae"]],
               mean(abs(portfolio$claims - fitted(model))))
})

test_that("Gamma deviance uses evaluation weights and predictions", {
  portfolio <- resampling_fixture()
  model <- glm(severity ~ z, Gamma(link = "log"), data = portfolio,
               weights = volume)
  set.seed(51)
  result <- validate_performance(
    model, portfolio, n_resamples = 1, sample_fraction = 0.75,
    sampling = "split", show_progress = FALSE, metrics = "deviance"
  )
  set.seed(51)
  train_rows <- sample(seq_len(nrow(portfolio)), 9)
  train <- portfolio[train_rows, ]
  assessment <- portfolio[-train_rows, ]
  refitted <- glm(severity ~ z, Gamma(link = "log"), data = train,
                  weights = volume)
  mu <- predict(refitted, assessment, type = "response")
  ratio <- assessment$severity / mu

  expect_equal(result$scores$deviance,
               2 * sum(assessment$volume * (ratio - 1 - log(ratio))))
  expect_false(isTRUE(all.equal(result$scores$deviance, stats::deviance(refitted))))
  expect_equal(result$original_scores[["deviance"]], stats::deviance(model))
  expect_null(result$rmse_bs)
})

test_that("metric selection is validated and works with print and plot methods", {
  portfolio <- resampling_fixture()
  model <- glm(claims ~ z + offset(log(exposure)), poisson(), data = portfolio)
  for (invalid in list(NULL, NA_character_, 1, "auc", c("mae", "mae"))) {
    expect_error(validate_performance(model, portfolio, metrics = invalid), "`metrics`")
  }
  run <- function() {
    set.seed(231)
    validate_performance(model, portfolio, n_resamples = 5,
                         metrics = c("mae", "rmse", "deviance"), show_progress = FALSE)
  }
  result <- run()
  expect_identical(result, run())
  expect_identical(as.vector(result), result$scores$mae)
  expect_output(print(result), "mae")
  expect_identical(autoplot(result)$data$x, result$scores$mae)
  expect_identical(autoplot(result, metric = "deviance")$data$x, result$scores$deviance)
  expect_error(autoplot(result, metric = "auc"), "`metric`")
})

test_that("grouped binomial scores use proportions and trial-weighted deviance", {
  portfolio <- data.frame(
    successes = c(2, 4, 1, 0, 2, 1),
    failures = c(2, 1, 2, 5, 3, 2),
    weight = c(1, 2, 4, 3, 2, 1)
  )
  model <- glm(cbind(successes, failures) ~ 1, binomial(),
               data = portfolio, weights = weight)
  set.seed(8)
  metrics <- c("rmse", "mae", "deviance")
  result <- validate_performance(
    model, portfolio, n_resamples = 1, sample_fraction = 0.5,
    sampling = "split", show_progress = FALSE, metrics = metrics
  )
  set.seed(8)
  train_rows <- sample(seq_len(nrow(portfolio)), 3)
  train <- portfolio[train_rows, ]
  assessment <- portfolio[-train_rows, ]
  mu <- with(train, sum(weight * successes) / sum(weight * (successes + failures)))
  with(assessment, {
    trials <- successes + failures
    error <- successes / trials - mu
    expect_equal(result$scores$rmse, sqrt(mean(error^2)))
    expect_equal(result$scores$mae, mean(abs(error)))
    term <- ifelse(successes == 0, 0, successes * log(successes / (trials * mu))) +
      ifelse(failures == 0, 0, failures * log(failures / (trials * (1 - mu))))
    expect_equal(result$scores$deviance, 2 * sum(weight * term))
  })

  comparison <- model_performance(model)
  expect_equal(result$original_scores[["rmse"]], comparison$RMSE)
  expect_equal(result$original_scores[["deviance"]], comparison$Residual_Deviance)
  portfolio$year <- ifelse(seq_len(nrow(portfolio)) %in% train_rows, 2020L, 2021L)
  rolling <- validate_performance(model, portfolio, sampling = "rolling", time = "year",
                                  initial = 1, metrics = metrics, show_progress = FALSE)
  expect_equal(rolling[metrics], result$scores)
})

test_that("resampling can score a binomial factor without a stored model frame", {
  portfolio <- data.frame(claim = factor(c("no", "yes", "yes", "yes")))
  model <- glm(claim ~ 1, binomial(), data = portfolio, model = FALSE)
  result <- validate_performance(model, portfolio, sampling = "split",
                                 n_resamples = 1, show_progress = FALSE)
  expected <- sqrt(mean((c(0, 1, 1, 1) - 0.75)^2))
  expect_equal(unname(result$rmse_bs), expected)
  expect_equal(result$rmse_mod, expected)
})

test_that("resampling plots explain the evaluation rows and reference score", {
  portfolio <- resampling_fixture()
  model <- glm(claims ~ z, poisson(), data = portfolio)
  for (sampling in c("bootstrap", "split")) {
    for (fraction in c(1, 0.6)) {
      result <- validate_performance(
        model, portfolio, n_resamples = 3, sample_fraction = fraction,
        sampling = sampling, metrics = c("rmse", "deviance"), show_progress = FALSE
      )
      evaluation <- if (fraction == 1) "training rows" else if (sampling == "split") {
        "held-out rows"
      } else {
        "out-of-bag rows"
      }
      plot <- autoplot(result)
      expect_identical(plot$labels$x, "RMSE")
      expect_match(plot$labels$subtitle, evaluation)
      expect_match(plot$labels$caption, "original model")
      deviance_plot <- autoplot(result, metric = "deviance")
      expect_identical(deviance_plot$labels$x, "Predictive deviance")
      expect_match(deviance_plot$labels$caption, "evaluation row count and weights")
    }
  }
})
