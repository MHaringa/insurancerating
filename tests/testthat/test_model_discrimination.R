test_that("concentration Gini measures signed ranking performance", {
  y <- c(100, 200, 300, 800, 1100)
  x <- discrimination_analysis(observed = y, predicted = list(
    Perfect = y, Constant = rep(500, 5), Reversed = rev(y), Transformed = y^2
  ))
  expect_identical(x$summary$Model, c("Perfect", "Constant", "Reversed", "Transformed"))
  expect_equal(x$summary$Gini, c(0.416, 0, -0.416, 0.416))
  expect_equal(x$summary$Normalized_Gini, c(1, 0, -1, 1))
  expect_equal(x$lorenz$X, seq(0, 1, .2))
  expect_equal(x$lorenz$Loss_share, c(0, cumsum(y) / sum(y)))
})

test_that("Gini and lift respect exposure and keep tied rates together", {
  y <- c(2, 6, 20)
  p <- c(4, 12, 10)
  e <- c(1, 3, 2)
  result <- function(i) discrimination_analysis(
    observed = y[i], predicted = p[i], exposure = e[i], n_groups = 10
  )
  a <- result(1:3)
  expect_equal(a$concentration$X, c(0, 4/6, 1))
  expect_equal(a$summary$Gini, 4/6 - 8/28)
  expect_equal(a$lift$Exposure, c(4, 2))
  expect_equal(a$lift$Observed_lift, c(2, 10) / (28/6))
  expect_equal(a$lift$Expected_lift, c(4, 5) / (26/6))
  expect_equal(a, result(c(3, 2, 1)))
  replicated <- discrimination_analysis(
    observed = rep(y / e, e), predicted = rep(p / e, e)
  )
  expect_equal(replicated$summary$Gini, a$summary$Gini)
  expect_equal(replicated$summary$Normalized_Gini, a$summary$Normalized_Gini)
  constant <- discrimination_analysis(observed = y, predicted = 0.1 * e, exposure = e)
  expect_identical(constant$summary$Gini, 0)
})

test_that("ordered Lorenz uses reference prediction shares and is scale invariant", {
  assess <- function(p, r) discrimination_analysis(
    observed = c(1, 7), predicted = p, reference = r
  )
  a <- assess(c(1, 6), c(1, 3))
  expect_equal(a$ordered_lorenz$X, c(0, .25, 1))
  expect_equal(a$ordered_lorenz$Loss_share, c(0, .125, 1))
  expect_equal(a$summary$Relative_Gini, .125)
  expect_equal(assess(c(5, 30), c(2, 6))$summary$Relative_Gini, .125)
  expect_equal(assess(c(1, 3), c(1, 3))$summary$Relative_Gini, 0)
  expect_equal(assess(c(2, 6), c(1, 3))$summary$Relative_Gini, 0)
  expect_equal(assess(c(1, 3), c(1, 6))$summary$Relative_Gini, -1/56)
})

test_that("invalid discrimination inputs and undefined benchmarks are explicit", {
  assess <- function(y, p, ...) discrimination_analysis(observed = y, predicted = p, ...)
  zero <- assess(c(0, 0), c(1, 2))
  expect_true(is.na(zero$summary$Gini))
  expect_true(all(is.na(zero$lift$Observed_lift)))
  expect_true(is.na(assess(c(1, 1), c(1, 2))$summary$Normalized_Gini))
  expect_error(assess(c(1, 2), c(1, NA)), "Predicted totals")
  expect_error(assess(c(1, 2), c(1, -1)), "Predicted totals")
  expect_error(assess(c(1, 2), c(1, 2), exposure = c(1, 0)), "strictly positive")
  expect_error(assess(c(1, 2), c(1, 2), reference = c(1, 0)), "must be positive")
  expect_error(assess(c(1, 2), c(1, 2), n_groups = 1.5), "positive whole")
  expect_error(assess(c(1, 2), list()), "at least one prediction")
})

test_that("GLM discrimination is consistent with direct exposure-adjusted totals", {
  d <- data.frame(y = c(1, 2, 3, 2, 5, 8), e = c(1, 2, 1, .5, 1, 1),
                  x = rep(c("a", "b"), each = 3))
  a <- glm(y ~ x + offset(log(e)), poisson(), d)
  b <- glm(y ~ offset(log(e)), poisson(), d)
  result <- discrimination_analysis(list(A = a, B = b), d, reference = b)
  direct <- discrimination_analysis(
    observed = d$y, predicted = list(A = fitted(a), B = fitted(b)),
    exposure = d$e, reference = fitted(b)
  )
  expect_equal(result, direct)
  expect_s3_class(autoplot.discrimination_result(result), "ggplot")
  expect_s3_class(autoplot.discrimination_result(result, type = "ordered_lorenz"), "ggplot")
  expect_s3_class(autoplot.discrimination_result(result, type = "lift"), "ggplot")
  expect_error(discrimination_analysis(glm(y ~ x, data = d), d), "supports Poisson")
})

test_that("performance tables and rolling metrics use the same Gini definition", {
  d <- data.frame(period = rep(2020:2023, each = 6),
                  y = rep(c(1, 2, 3, 2, 5, 8), 4) + rep(0:3, each = 6),
                  e = rep(c(1, 2, 1, .5, 1, 1), 4),
                  x = rep(rep(c("a", "b"), each = 3), 4))
  a <- glm(y ~ x + offset(log(e)), poisson(), d)
  table <- model_performance(a, metrics = c("gini", "normalized_gini"))
  direct <- discrimination_analysis(a, d)$summary
  expect_equal(table$Gini, direct$Gini)
  expect_equal(table$Normalized_Gini, direct$Normalized_Gini)
  expect_equal(table[names(model_performance(a))], model_performance(a),
               ignore_attr = TRUE)
  set.seed(708)
  result <- validate_performance(a, d, n_resamples = 2, show_progress = FALSE,
                                 metrics = c("gini", "normalized_gini"))
  expect_equal(unname(result$original_scores), c(direct$Gini, direct$Normalized_Gini))
  rolling <- validate_performance(a, d, sampling = "rolling", time = "period",
                                  initial = 2, metrics = c("gini", "normalized_gini"))
  earlier <- glm(y ~ x + offset(log(e)), poisson(), d[d$period <= 2021, ])
  first <- discrimination_analysis(earlier, d[d$period == 2022, ])$summary
  expect_equal(rolling$gini[1L], first$Gini)
  expect_equal(rolling$normalized_gini[1L], first$Normalized_Gini)
  expect_error(model_performance(a, metrics = "mae"), "distinct values")
})
