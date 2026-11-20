rolling_portfolio <- function() {
  year <- rep(2020:2025, c(3, 2, 4, 3, 5, 2))
  n <- length(year)
  data.frame(year = year, x = seq_len(n) / 5,
             claims = rep(c(0, 2, 1, 5, 4), length.out = n),
             severity = 100 + seq_len(n) * 2 + rep(c(5, 12, 1), length.out = n),
             exposure = rep(c(0.3, 1.1, 0.7), length.out = n),
             case_weight = rep(c(2, 1, 3), length.out = n))
}

test_that("rolling windows expand chronologically without changing the RNG", {
  data <- rolling_portfolio()
  model <- glm(claims ~ x + offset(log(exposure)), poisson(), data)
  set.seed(7)
  shuffled <- data[sample(seq_len(nrow(data))), ]
  rng <- .Random.seed
  roll <- resample_performance(model, shuffled, method = "rolling", time = "year",
                               initial = 3, show_progress = FALSE)
  expect_identical(.Random.seed, rng)
  expect_identical(roll$iteration, 1:3)
  expect_identical(roll$train_start, rep(2020L, 3))
  expect_identical(roll$train_end, 2022:2024)
  expect_identical(roll$test_start, 2023:2025)
  expect_identical(roll$test_end, roll$test_start)
  expect_identical(roll$n_train, c(9L, 12L, 17L))
  expect_identical(roll$n_test, c(3L, 5L, 2L))
  expect_identical(roll, resample_performance(
    model, shuffled, sampling = "rolling", time = "year", initial = 3,
    show_progress = FALSE
  ))
})

test_that("rolling horizons use distinct periods and retain dates", {
  data <- rolling_portfolio()
  data$period <- as.Date(paste0(data$year, "-01-01"))
  model <- glm(claims ~ x + offset(log(exposure)), poisson(), data)
  roll <- resample_performance(model, data, sampling = "rolling", time = "period",
                               initial = 3, horizon = 2, show_progress = FALSE)
  expect_identical(roll$iteration, 1:2)
  expect_identical(roll$test_start, as.Date(c("2023-01-01", "2024-01-01")))
  expect_identical(roll$test_end, as.Date(c("2024-01-01", "2025-01-01")))
  expect_identical(roll$n_test, c(8L, 7L))
  expect_true(all(roll$train_end < roll$test_start))
})

test_that("rolling Poisson and Gamma scores use weighted test deviance and test predictions", {
  data <- rolling_portfolio()
  poisson_model <- glm(claims ~ x, poisson(), data, offset = log(exposure),
                       weights = case_weight)
  gamma_model <- glm(severity ~ x, Gamma(link = "log"), data, weights = case_weight)
  for (model in list(poisson_model, gamma_model)) {
    scores <- resample_performance(model, data, sampling = "rolling", time = "year",
                                   initial = 3, metrics = c("rmse", "mae", "deviance"),
                                   show_progress = FALSE)
    for (i in seq_len(nrow(scores))) {
      training <- data[data$year <= scores$train_end[i], ]
      testing <- data[data$year == scores$test_start[i], ]
      if (model$family$family == "poisson") {
        refit <- glm(claims ~ x, poisson(), training, offset = log(exposure),
                     weights = case_weight)
        observed <- testing$claims
      } else {
        refit <- glm(severity ~ x, Gamma(link = "log"), training, weights = case_weight)
        observed <- testing$severity
      }
      expected <- stats::predict(refit, testing, type = "response")
      expect_equal(scores$rmse[i], sqrt(mean((observed - expected)^2)))
      expect_equal(scores$mae[i], mean(abs(observed - expected)))
      expect_equal(scores$deviance[i],
                   sum(refit$family$dev.resids(observed, expected, testing$case_weight)))
    }
  }
})

test_that("rolling preprocessing is estimated within each training window", {
  data <- rolling_portfolio()
  model <- glm(severity ~ 0 + scale(x), data = data)
  roll <- resample_performance(model, data, method = "rolling", time = "year",
                               initial = 3, show_progress = FALSE)
  train <- data[data$year <= 2022, ]
  test <- data[data$year == 2023, ]
  centre <- mean(train$x)
  spread <- stats::sd(train$x)
  standardized <- (train$x - centre) / spread
  beta <- sum(standardized * train$severity) / sum(standardized^2)
  predicted <- beta * (test$x - centre) / spread
  expect_equal(roll$rmse[1L], sqrt(mean((test$severity - predicted)^2)))
})

test_that("rolling rejects invalid periods and never borrows unseen factor levels", {
  data <- rolling_portfolio()
  model <- glm(claims ~ x + offset(log(exposure)), poisson(), data)
  run <- function(data, ...) resample_performance(
    model, data, method = "rolling", time = "year", show_progress = FALSE, ...
  )
  expect_error(run(transform(data, year = NA_integer_), initial = 3), "missing periods")
  expect_error(run(data, initial = 5, horizon = 2), "Insufficient distinct periods")
  expect_error(run(data, initial = 0), "positive whole number")
  expect_error(run(data, initial = 3, horizon = 1.5), "positive whole number")
  expect_error(run(data, initial = 3, n_resamples = 5), "determines windows from periods")
  expect_error(run(data, initial = 3, sampling = "split"), "not both")

  data$region <- factor(ifelse(data$year >= 2023, "C", rep(c("A", "B"), length.out = nrow(data))))
  model <- glm(claims ~ region + offset(log(exposure)), poisson(), data)
  expect_error(run(data, initial = 3), "Rolling window 1.*new level.*C")
})
