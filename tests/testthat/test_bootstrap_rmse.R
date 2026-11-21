context("validate_performance")

test_that("validate_performance returns the legacy RMSE performance object", {
  mod1 <- glm(nclaims ~ age_policyholder,
              data = MTPL,
              offset = log(exposure),
              family = poisson())

  x <- validate_performance(
    mod1,
    MTPL,
    n_resamples = 10,
    metric = "rmse",
    show_progress = FALSE
  )

  expect_s3_class(x, "bootstrap_performance")
  expect_named(x, c("rmse_bs", "rmse_mod", "metric", "sampling", "n_resamples",
                    "sample_fraction", "n", "frac"))
  expect_equal(length(x$rmse_bs), 10)
  expect_equal(x$metric, "rmse")
  expect_equal(x$sampling, "bootstrap")
  expect_equal(x$n_resamples, 10)
  expect_equal(x$sample_fraction, 1)
  expect_equal(x$n, 10)
  expect_equal(x$frac, 1)
  expect_true(is.numeric(x$rmse_mod))
})

test_that("validate_performance supports split sampling", {
  mod1 <- glm(nclaims ~ age_policyholder,
              data = MTPL,
              offset = log(exposure),
              family = poisson())

  x <- validate_performance(
    mod1,
    MTPL,
    n_resamples = 5,
    sample_fraction = 0.8,
    sampling = "split",
    show_progress = FALSE
  )

  expect_s3_class(x, "bootstrap_performance")
  expect_equal(length(x$rmse_bs), 5)
  expect_equal(x$sampling, "split")
  expect_false(anyNA(x$rmse_bs))
})

test_that("validate_performance supports supplied model RMSE", {
  mod1 <- glm(nclaims ~ age_policyholder,
              data = MTPL,
              offset = log(exposure),
              family = poisson())

  x <- validate_performance(
    mod1,
    MTPL,
    n_resamples = 3,
    rmse_model = 123,
    show_progress = FALSE
  )

  expect_equal(x$rmse_mod, 123)
})

test_that("validate_performance handles character levels absent from training sample", {
  data <- data.frame(
    y = rep(c(0, 1), 8),
    rating_factor = rep(c("a", "b", "c", "d"), each = 4)
  )
  mod1 <- glm(y ~ rating_factor, data = data, family = poisson())

  set.seed(1)
  expect_silent(
    x <- validate_performance(
      mod1,
      data,
      n_resamples = 3,
      sample_fraction = 0.5,
      sampling = "split",
      show_progress = FALSE
    )
  )
  expect_equal(length(x$rmse_bs), 3)
})

test_that("validate_performance validates inputs", {
  mod1 <- glm(nclaims ~ age_policyholder,
              data = MTPL,
              offset = log(exposure),
              family = poisson())

  expect_error(
    validate_performance(mod1, MTPL, n_resamples = 0, show_progress = FALSE),
    "`n_resamples`"
  )
  expect_error(
    validate_performance(mod1, MTPL, n_resamples = 1.5, show_progress = FALSE),
    "`n_resamples`"
  )
  expect_error(
    validate_performance(mod1, MTPL, sample_fraction = NA_real_, show_progress = FALSE),
    "`sample_fraction`"
  )
  expect_error(
    validate_performance(mod1, MTPL, metric = "mae", show_progress = FALSE),
    "`metric`"
  )
  expect_error(
    validate_performance(mod1, MTPL, sampling = "cv", show_progress = FALSE),
    "'arg' should be one of"
  )
  expect_error(
    validate_performance(mod1, MTPL, show_progress = NA),
    "`show_progress`"
  )
  expect_error(
    validate_performance(mod1, MTPL, rmse_model = Inf, show_progress = FALSE),
    "`rmse_model`"
  )
  expect_error(
    validate_performance(mod1, MTPL[0, ], show_progress = FALSE),
    "`data`"
  )
})

test_that("saved bootstrap_rmse results retain their S3 methods", {
  mod1 <- glm(nclaims ~ age_policyholder,
              data = MTPL,
              offset = log(exposure),
              family = poisson())

  x <- validate_performance(mod1, MTPL, n_resamples = 3, show_progress = FALSE)
  saved <- structure(x, class = c("bootstrap_rmse", class(x)))

  expect_identical(as.vector(saved), as.vector(x))
  expect_identical(autoplot(saved)$data, autoplot(x)$data)
  expect_identical(capture.output(print(saved)), capture.output(print(x)))
})

test_that("deprecated resampling argument names remain available", {
  mod1 <- glm(nclaims ~ age_policyholder,
              data = MTPL,
              offset = log(exposure),
              family = poisson())

  expect_warning(
    x <- validate_performance(mod1, MTPL, n = 3, frac = 0.8,
                              sampling = "split", show_progress = FALSE),
    "deprecated"
  )

  expect_equal(x$n_resamples, 3)
  expect_equal(x$sample_fraction, 0.8)
})

test_that("bootstrap_performance methods work", {
  mod1 <- glm(nclaims ~ age_policyholder,
              data = MTPL,
              offset = log(exposure),
              family = poisson())
  x <- validate_performance(mod1, MTPL, n_resamples = 3, show_progress = FALSE)

  expect_type(as.vector(x), "double")
  expect_equal(length(as.vector(x)), 3)
  expect_s3_class(autoplot(x), "ggplot")
  expect_output(print(x))
})
