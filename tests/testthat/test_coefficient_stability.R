context("coefficient_stability")

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

test_that("coefficient_stability resamples model data reproducibly", {
  model <- .coefficient_bootstrap_model()

  x <- coefficient_stability(
    model,
    n_resamples = 12,
    seed = 123,
    show_progress = FALSE
  )
  y <- coefficient_stability(
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
  expect_false("data" %in% names(formals(coefficient_stability)))
})

test_that("deprecated bootstrap_coefficients preserves arguments and warns once", {
  model <- .coefficient_bootstrap_model()
  expected <- coefficient_stability(object = model, n_resamples = 8, seed = 17,
                                    show_progress = FALSE)
  warnings <- list()
  actual <- withCallingHandlers(
    bootstrap_coefficients(object = model, n_resamples = 8, seed = 17,
                           show_progress = FALSE),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  expect_s3_class(warnings[[1L]], "deprecatedWarning")
  expect_match(conditionMessage(warnings[[1L]]), "coefficient_stability")
  expect_identical(actual$call[[1L]], quote(bootstrap_coefficients))
  actual$call[[1L]] <- quote(coefficient_stability)
  expect_identical(actual, expected)
})

test_that("summary supports link and exponentiated coefficient scales", {
  model <- .coefficient_bootstrap_model()
  x <- coefficient_stability(
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

  x <- coefficient_stability(
    model,
    n_resamples = 20,
    seed = 7,
    show_progress = FALSE
  )

  expect_identical(colnames(x$replicates), names(coef(model)))
  expect_true(any(is.na(x$replicates[, "segmentC"])))

  set.seed(7)
  dropped_c <- replicate(20, {
    drawn <- portfolio$segment[sample.int(nrow(portfolio), replace = TRUE)]
    all(c("A", "B") %in% drawn) && !"C" %in% drawn
  })
  comparable <- dropped_c & x$successful_fit
  expect_true(any(comparable))
  expect_true(all(is.finite(x$replicates[comparable, "segmentB"])))
})

test_that("a changed reference never contributes to a coefficient interval", {
  portfolio <- data.frame(
    region = factor(c("A", rep("B", 5), rep("C", 5))),
    claims = c(4, rep(1, 5), rep(2, 5))
  )
  model <- glm(claims ~ region, poisson(), data = portfolio)
  set.seed(3)
  drawn <- sample.int(nrow(portfolio), replace = TRUE)
  expect_setequal(as.character(portfolio$region[drawn]), c("B", "C"))
  refit <- update(model, data = portfolio[drawn, ])
  expect_equal(unname(exp(coef(model)["regionC"])), 0.5)
  expect_equal(unname(exp(coef(refit)["regionC"])), 2)

  result <- coefficient_stability(model, n_resamples = 1, seed = 3, show_progress = FALSE)
  expect_true(result$successful_fit[1L])
  expect_true(all(is.na(result$replicates[1L, ])))
  summary_table <- summary(result, scale = "relativity")
  expect_equal(summary_table$n_successful[summary_table$term == "regionC"], 0)
  expect_true(is.na(summary_table$upper[summary_table$term == "regionC"]))
})

test_that("changed contrasts invalidate interaction margins but not unrelated terms", {
  portfolio <- data.frame(
    region = factor(rep(c("A", "B", "C"), c(2, 5, 5))),
    age = c(1, 4, 1:5, 1:5),
    weight_band = c(2, 5, 4, 8, 2, 1, 3, 0, 2, 3, 7, 5),
    claims = c(4, 3, 6, 4, 5, 6, 8, 6, 12, 10, 9, 13)
  )
  model <- glm(claims ~ region * age + weight_band, poisson(), data = portfolio)
  set.seed(41)
  missing_reference <- replicate(30, {
    drawn <- portfolio$region[sample.int(nrow(portfolio), replace = TRUE)]
    all(c("B", "C") %in% drawn) && !"A" %in% drawn
  })
  result <- coefficient_stability(model, n_resamples = 30, seed = 41,
                                  show_progress = FALSE)
  affected <- missing_reference & result$successful_fit
  expect_true(any(affected))
  expect_true(all(is.na(result$replicates[affected, c("age", "regionC:age")])))
  expect_true(any(is.finite(result$replicates[affected, "weight_band"])))
})

test_that("contrasts other than treatment can change when any level drops out", {
  portfolio <- data.frame(
    region = factor(c(rep("A", 5), rep("B", 5), "C")),
    claims = c(rep(4, 5), rep(1, 5), 2)
  )
  model <- glm(claims ~ region, poisson(), data = portfolio,
               contrasts = list(region = "contr.sum"))
  set.seed(51)
  changed <- replicate(15, {
    drawn <- portfolio$region[sample.int(nrow(portfolio), replace = TRUE)]
    all(c("A", "B") %in% drawn) && !"C" %in% drawn
  })
  result <- coefficient_stability(model, n_resamples = 15, seed = 51,
                                  show_progress = FALSE)
  affected <- changed & result$successful_fit
  expect_true(any(affected))
  expect_true(all(is.na(result$replicates[affected, "region1"])))
  expect_true(any(is.finite(result$replicates[!changed, "region1"])))
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

  x <- coefficient_stability(
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
    x <- coefficient_stability(
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

  expect_error(coefficient_stability(mtcars), "fitted `glm`")
  expect_error(
    coefficient_stability(model, n_resamples = 0),
    "positive whole number"
  )
  expect_error(
    coefficient_stability(model, n_resamples = 1.5),
    "positive whole number"
  )
  expect_error(
    coefficient_stability(model, seed = -1),
    "whole number between"
  )
  expect_error(
    coefficient_stability(model, show_progress = NA),
    "TRUE or FALSE"
  )
})

test_that("print and gt methods present bootstrap coefficient results", {
  model <- .coefficient_bootstrap_model()
  x <- coefficient_stability(
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
