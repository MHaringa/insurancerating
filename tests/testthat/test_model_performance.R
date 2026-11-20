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
