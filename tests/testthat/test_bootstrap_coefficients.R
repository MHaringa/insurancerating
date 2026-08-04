context("bootstrap_coefficients")

.coefficient_bootstrap_model <- function() {
  portfolio <- data.frame(
    claims = c(0, 1, 0, 2, 1, 3, 0, 1, 2, 0, 1, 1),
    sector = factor(rep(c("Industry", "Retail", "Services"), each = 4)),
    insured_amount = seq(100, 1200, by = 100),
    exposure = rep(1, 12)
  )
  glm(
    claims ~ sector + insured_amount + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
}

test_that("bootstrap_coefficients resamples model data reproducibly", {
  model <- .coefficient_bootstrap_model()

  x <- bootstrap_coefficients(
    model,
    n_resamples = 12,
    seed = 123,
    show_progress = FALSE
  )
  y <- bootstrap_coefficients(
    model,
    n_resamples = 12,
    seed = 123,
    show_progress = FALSE
  )

  expect_s3_class(x, "bootstrap_coefficients")
  expect_equal(dim(x$replicates), c(12, length(coef(model))))
  expect_identical(colnames(x$replicates), names(coef(model)))
  expect_equal(x$replicates, y$replicates)
  expect_equal(x$n_observations, nobs(model))
  expect_false("data" %in% names(formals(bootstrap_coefficients)))
})

test_that("summary supports link and exponentiated coefficient scales", {
  model <- .coefficient_bootstrap_model()
  x <- bootstrap_coefficients(
    model,
    n_resamples = 10,
    seed = 44,
    show_progress = FALSE
  )

  link <- summary(x, scale = "link")
  exponentiated <- summary(x, scale = "exponentiated")
  relativity <- summary(x, scale = "relativity")

  expect_s3_class(link, "data.frame")
  expect_named(
    link,
    c(
      "term", "estimate", "bootstrap_mean", "bias", "bootstrap_se",
      "lower", "upper", "n_successful", "n_requested", "success_rate"
    )
  )
  expect_equal(link$estimate, unname(coef(model)))
  expect_equal(exponentiated$estimate, exp(link$estimate))
  expect_identical(exponentiated, relativity)
  expect_true(all(link$n_requested == 10))
  expect_true(all(link$n_successful <= link$n_requested))
  expect_equal(link$success_rate, link$n_successful / link$n_requested)
})

test_that("factor coefficients remain aligned when levels are absent", {
  portfolio <- data.frame(
    claims = c(0, 1, 0, 2, 1, 0, 3, 1),
    segment = factor(c(rep("A", 5), rep("B", 2), "C")),
    exposure = 1
  )
  model <- glm(
    claims ~ segment + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )

  x <- bootstrap_coefficients(
    model,
    n_resamples = 20,
    seed = 7,
    show_progress = FALSE
  )

  expect_identical(colnames(x$replicates), names(coef(model)))
  expect_true(any(is.na(x$replicates[, "segmentC"])))
})

test_that("rows omitted by the original model are not resampled", {
  portfolio <- data.frame(
    claims = c(0, 1, 2, 0, 1, 3),
    age = c(20, 30, NA, 50, 60, 70),
    exposure = 1
  )
  model <- glm(
    claims ~ age + offset(log(exposure)),
    family = poisson(),
    data = portfolio,
    na.action = na.omit
  )

  x <- bootstrap_coefficients(
    model,
    n_resamples = 5,
    seed = 2,
    show_progress = FALSE
  )

  expect_equal(x$n_observations, 5)
  expect_equal(x$n_observations, nobs(model))
})

test_that("failed GLM refits are retained without stopping", {
  reject_duplicates <- function(x) {
    if (anyDuplicated(x)) stop("duplicate portfolio rows")
    x
  }
  portfolio <- data.frame(
    response = seq_len(15),
    row_key = seq_len(15)
  )
  model <- glm(response ~ reject_duplicates(row_key), data = portfolio)

  expect_message(
    x <- bootstrap_coefficients(
      model,
      n_resamples = 8,
      seed = 1,
      show_progress = FALSE
    ),
    "completed [0-7] of 8"
  )

  expect_s3_class(x, "bootstrap_coefficients")
  expect_lt(x$n_successful, x$n_resamples)
  expect_true(any(!x$successful_fit))
  result <- summary(x)
  expect_true(all(result$n_requested == 8))
  expect_true(all(result$n_successful <= 8))
})

test_that("bootstrap_coefficients validates its public arguments", {
  model <- .coefficient_bootstrap_model()

  expect_error(bootstrap_coefficients(mtcars), "fitted `glm`")
  expect_error(
    bootstrap_coefficients(model, n_resamples = 0),
    "positive whole number"
  )
  expect_error(
    bootstrap_coefficients(model, n_resamples = 1.5),
    "positive whole number"
  )
  expect_error(
    bootstrap_coefficients(model, seed = -1),
    "whole number between"
  )
  expect_error(
    bootstrap_coefficients(model, show_progress = NA),
    "TRUE or FALSE"
  )
})

test_that("print and gt methods present bootstrap coefficient results", {
  model <- .coefficient_bootstrap_model()
  x <- bootstrap_coefficients(
    model,
    n_resamples = 4,
    seed = 9,
    show_progress = FALSE
  )

  expect_output(print(x), "Bootstrap coefficient stability")

  skip_if_not_installed("gt")
  table <- as_gt(
    x,
    scale = "relativity",
    locale = "en-US",
    estimate_decimals = 2,
    success_decimals = 0
  )
  expect_s3_class(table, "gt_tbl")
})
