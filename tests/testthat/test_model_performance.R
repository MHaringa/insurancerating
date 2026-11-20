library(insurancerating)
context("model_performance")

test_that("model_performance appends deviance and retains Poisson count metrics", {
  portfolio <- data.frame(
    claims = c(0, 2, 1, 4, 2, 3),
    exposure = c(0.5, 1, 2, 0.75, 1.5, 2.5),
    area = rep(c("A", "B"), each = 3)
  )
  base_model <- glm(claims ~ 1 + offset(log(exposure)),
                    family = poisson(), data = portfolio)
  area_model <- glm(claims ~ area + offset(log(exposure)),
                    family = poisson(), data = portfolio)
  result <- model_performance(base_model, area_model)
  models <- list(base_model, area_model)

  expect_identical(class(result), c("model_performance", "data.frame"))
  expect_named(result, c("Model", "AIC", "BIC", "RMSE", "Residual_Deviance"))
  expect_identical(result$Model, c("base_model", "area_model"))
  expect_identical(result$AIC, vapply(models, stats::AIC, numeric(1)))
  expect_identical(result$BIC, vapply(models, stats::BIC, numeric(1)))
  expect_identical(result$RMSE, vapply(models, rmse, numeric(1), data = portfolio))
  expect_identical(result$Residual_Deviance,
                   vapply(models, stats::deviance, numeric(1)))

  count_errors <- portfolio$claims - stats::fitted(area_model)
  expect_equal(result$RMSE[2L], sqrt(mean(count_errors^2)))
  expect_false(isTRUE(all.equal(
    result$RMSE[2L], sqrt(mean((count_errors / portfolio$exposure)^2))
  )))
})

test_that("model_performance reports residual deviance and retains Gamma metrics", {
  claims <- data.frame(
    severity = c(120, 210, 160, 300, 250, 375),
    area = rep(c("A", "B"), each = 3)
  )
  model <- glm(severity ~ area, family = Gamma(link = "log"), data = claims)
  result <- model_performance(model)

  expect_identical(result$Residual_Deviance, stats::deviance(model))
  expect_identical(result$AIC, stats::AIC(model))
  expect_identical(result$BIC, stats::BIC(model))
  expect_identical(result$RMSE, rmse(model, data = claims))
  expect_equal(result$RMSE, sqrt(mean((claims$severity - stats::fitted(model))^2)))
})

test_that("in-sample RMSE uses the rows kept by subset and na.action", {
  portfolio <- data.frame(
    claims = c(1, 2, 4, 8, 10, 100),
    exposure = c(0.5, 1, NA, 2, 3, 1)
  )
  for (keep_model in c(TRUE, FALSE)) {
    model <- glm(claims ~ 1, poisson(), data = portfolio, offset = log(exposure),
                 subset = claims < 100, na.action = na.exclude,
                 model = keep_model, y = keep_model)
    selected <- portfolio[c(1, 2, 4, 5), ]
    mu <- selected$exposure * sum(selected$claims) / sum(selected$exposure)
    expected <- sqrt(mean((selected$claims - mu)^2))
    expect_equal(model_performance(model)$RMSE, expected)
    expect_equal(rmse(model), expected)
    expect_equal(rmse(model, selected), expected)
    expect_gt(rmse(model, portfolio), expected)
  }
})

test_that("binomial factor RMSE follows the fitted response reference", {
  portfolio <- data.frame(
    claim = factor(c("no", "yes", "no", "yes", "yes", "no")),
    weight = c(2, 1, 1, 0, 1, 1)
  )
  model <- glm(claim ~ 1, binomial(), data = portfolio, weights = weight)
  response <- as.numeric(portfolio$claim == "yes")
  expected <- sqrt(mean((response - 2 / 6)^2))
  expect_equal(model_performance(model)$RMSE, expected)
  expect_equal(rmse(model, portfolio), expected)
  portfolio$claim <- factor(portfolio$claim, levels = c("yes", "no"))
  expect_equal(rmse(model, portfolio), expected)
  expect_equal(rmse(model, droplevels(portfolio[portfolio$claim == "yes", ])), 1 - 2 / 6)
})
