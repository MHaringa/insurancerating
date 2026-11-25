test_that("period coefficients keep offsets, weights and the original reference", {
  d <- data.frame(year = rep(2022:2024, each = 8),
                  region = factor(rep(rep(c("A", "B"), each = 4), 3)),
                  nclaims = rep(c(1, 2, 3, 2, 3, 4, 5, 4), 3),
                  exposure = rep(c(1, 0.5), 12), w = rep(1:3, 8))
  model <- glm(nclaims ~ region + offset(log(exposure)), poisson(), d, weights = w)
  ev <- d[!(d$year == 2024 & d$region == "A"), ]
  result <- coefficient_stability(model, data = ev, method = "period", period = "year")
  one <- glm(nclaims ~ region + offset(log(exposure)), poisson(),
             d[d$year == 2022, ], weights = w)
  first <- subset(result$estimates, period == 2022)
  expect_equal(first$estimate, unname(coef(one)))
  expect_equal(first$std_error, unname(summary(one)$coefficients[, 2]))
  expect_equal(first$exposure, rep(6, 2))
  expect_true(all(is.na(subset(result$estimates, period == 2024)$estimate)))
  expect_true(all(subset(result$estimates, period == 2024)$status == "not_estimable"))
  expect_s3_class(autoplot.period_coefficients(result, scale = "relativity"), "ggplot")
  expect_error(coefficient_stability(model, method = "period", period = "missing"), "period")
})
