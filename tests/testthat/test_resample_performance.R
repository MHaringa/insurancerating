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
  expected <- resample_performance(model, portfolio, n_resamples = 3,
                                    show_progress = FALSE)
  set.seed(61)
  expect_warning(
    actual <- bootstrap_performance(data = portfolio, model = model,
                                     n_resamples = 3, show_progress = FALSE),
    "resample_performance",
    class = "deprecatedWarning"
  )
  expect_identical(actual, expected)
})

test_that("resampling scores the same training, out-of-bag and held-out rows", {
  portfolio <- resampling_fixture()
  model <- glm(claims ~ z, poisson(), data = portfolio,
               offset = log(exposure), weights = 2 * volume)
  metrics <- c("deviance", "rmse", "mae")
  for (sampling in c("bootstrap", "split")) {
    for (fraction in c(1, 0.6)) {
      set.seed(92)
      result <- resample_performance(
        model, portfolio, n_resamples = 3, sample_fraction = fraction,
        sampling = sampling, metrics = metrics, show_progress = FALSE
      )
      set.seed(92)
      legacy <- resample_performance(
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
  result <- resample_performance(
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
    expect_error(resample_performance(model, portfolio, metrics = invalid), "`metrics`")
  }
  run <- function() {
    set.seed(231)
    resample_performance(model, portfolio, n_resamples = 5,
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

test_that("predictive deviance includes trial counts for grouped binomial models", {
  portfolio <- data.frame(
    successes = c(2, 4, 1, 0, 2, 1),
    failures = c(2, 1, 2, 5, 3, 2),
    weight = c(1, 2, 4, 3, 2, 1)
  )
  model <- glm(cbind(successes, failures) ~ 1, binomial(),
               data = portfolio, weights = weight)
  set.seed(8)
  result <- resample_performance(
    model, portfolio, n_resamples = 1, sample_fraction = 0.5,
    sampling = "split", show_progress = FALSE, metrics = "deviance"
  )
  set.seed(8)
  train_rows <- sample(seq_len(nrow(portfolio)), 3)
  train <- portfolio[train_rows, ]
  assessment <- portfolio[-train_rows, ]
  mu <- with(train, sum(weight * successes) / sum(weight * (successes + failures)))
  with(assessment, {
    trials <- successes + failures
    term <- ifelse(successes == 0, 0, successes * log(successes / (trials * mu))) +
      ifelse(failures == 0, 0, failures * log(failures / (trials * (1 - mu))))
    expect_equal(result$scores$deviance, 2 * sum(weight * term))
  })
})
