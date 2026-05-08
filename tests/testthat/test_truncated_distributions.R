library(insurancerating)
context("truncated distributions")

test_that("truncated random generators validate inputs and stay inside bounds", {
  set.seed(1)

  gamma_draws <- rgammat(20, shape = 2, scale = 1000, lower = 500, upper = 5000)
  lognormal_draws <- rlnormt(20, meanlog = 7, sdlog = 0.5,
                             lower = 500, upper = 5000)

  expect_length(gamma_draws, 20)
  expect_true(all(gamma_draws >= 500 & gamma_draws <= 5000))
  expect_length(lognormal_draws, 20)
  expect_true(all(lognormal_draws >= 500 & lognormal_draws <= 5000))

  expect_error(rgammat(0, 2, 1000, 500, 5000),
               "`n` must be a positive whole number.", fixed = TRUE)
  expect_error(rgammat(5, -1, 1000, 500, 5000),
               "`shape` must be a single positive finite number.", fixed = TRUE)
  expect_error(rlnormt(5, 7, 0, 500, 5000),
               "`sdlog` must be a single positive finite number.", fixed = TRUE)
  expect_error(rlnormt(5, 7, 0.5, 5000, 500),
               "`lower_truncation` must be smaller than `upper_truncation`.",
               fixed = TRUE)
})

test_that("fit_truncated_dist fits gamma severity with new argument names", {
  set.seed(2)
  losses <- rgammat(60, shape = 2, scale = 1000, lower = 500, upper = 5000)

  fit <- fit_truncated_dist(
    losses = losses,
    distribution = "gamma",
    lower_truncation = 500,
    upper_truncation = 5000,
    n_variants = 0,
    n_shape_grid = 2,
    n_scale_grid = 2,
    show_summary = FALSE
  )

  expect_s3_class(fit, "truncated_dist")
  expect_s3_class(fit, "fitdist")
  expect_equal(attr(fit, "losses"), losses)
  expect_equal(attr(fit, "truncated_vec"), losses)
  expect_equal(attr(fit, "lower_truncation"), 500)
  expect_equal(attr(fit, "upper_truncation"), 5000)
  expect_equal(attr(fit, "left"), 500)
  expect_equal(attr(fit, "right"), 5000)
  expect_true(attr(fit, "n_attempts") >= attr(fit, "n_success"))
})

test_that("fit_truncated_dist validates core inputs", {
  expect_error(
    fit_truncated_dist(losses = character(), show_summary = FALSE),
    "`losses` must be a non-empty numeric vector.",
    fixed = TRUE
  )
  expect_error(
    fit_truncated_dist(losses = c(NA_real_, Inf), show_summary = FALSE),
    "`losses` must contain at least one finite observation.",
    fixed = TRUE
  )
  expect_error(
    fit_truncated_dist(
      losses = c(100, 200),
      lower_truncation = 100,
      show_summary = FALSE
    ),
    "All `losses` must lie strictly inside the truncation interval.",
    fixed = TRUE
  )
  expect_error(
    fit_truncated_dist(
      losses = c(100, 200),
      lower_truncation = NA_real_,
      show_summary = FALSE
    ),
    "`lower_truncation` and `upper_truncation` must be numeric scalars.",
    fixed = TRUE
  )
  expect_error(
    fit_truncated_dist(losses = c(100, 200), n_shape_grid = 0,
                       show_summary = FALSE),
    "`n_shape_grid` must be a positive whole number.",
    fixed = TRUE
  )
  expect_error(
    fit_truncated_dist(losses = c(100, 200), show_progress = NA,
                       show_summary = FALSE),
    "`show_progress` must be TRUE or FALSE.",
    fixed = TRUE
  )
})

test_that("fit_truncated_dist validates start values", {
  losses <- c(600, 700, 800, 900)

  expect_error(
    fit_truncated_dist(
      losses = losses,
      distribution = "gamma",
      start_values = list(shape = c(1, 2)),
      show_summary = FALSE
    ),
    "`start_values` must contain scale.",
    fixed = TRUE
  )
  expect_error(
    fit_truncated_dist(
      losses = losses,
      distribution = "lognormal",
      start_values = list(meanlog = 7, sdlog = -1),
      show_summary = FALSE
    ),
    "Scale, shape, and standard deviation start values must be positive.",
    fixed = TRUE
  )
})

test_that("deprecated fit_truncated_dist argument names still work", {
  set.seed(3)
  losses <- rgammat(30, shape = 2, scale = 1000, lower = 500, upper = 5000)

  expect_warning(
    fit <- fit_truncated_dist(
      y = losses,
      dist = "gamma",
      left = 500,
      right = 5000,
      trace = FALSE,
      report = FALSE,
      n_variants = 0,
      n_shape_grid = 2,
      n_scale_grid = 2
    ),
    "deprecated"
  )

  expect_s3_class(fit, "truncated_dist")
})

test_that("autoplot.truncated_dist supports new and deprecated arguments", {
  set.seed(4)
  losses <- rgammat(40, shape = 2, scale = 1000, lower = 500, upper = 5000)
  fit <- fit_truncated_dist(
    losses = losses,
    distribution = "gamma",
    lower_truncation = 500,
    upper_truncation = 5000,
    n_variants = 0,
    n_shape_grid = 2,
    n_scale_grid = 2,
    show_summary = FALSE
  )

  plot <- autoplot(
    fit,
    ecdf_geom = "step",
    x_label = "claim amount",
    y_label = "cumulative share",
    y_limits = c(0, 1),
    x_limits = c(0, 6000),
    show_title = FALSE,
    digits = 1,
    truncation_digits = 0
  )
  expect_s3_class(plot, "ggplot")

  expect_warning(
    old_plot <- autoplot(
      fit,
      geom_ecdf = "point",
      xlab = "severity",
      ylab = "cdf",
      ylim = c(0, 1),
      xlim = c(0, 6000),
      print_title = FALSE,
      print_dig = 1,
      print_trunc = 0
    ),
    "deprecated"
  )
  expect_s3_class(old_plot, "ggplot")
})

test_that("autoplot.truncated_dist validates arguments", {
  object <- structure(list(), class = "truncated_dist")

  expect_error(
    autoplot(data.frame(x = 1)),
    "Objects of class",
    fixed = TRUE
  )
  expect_error(
    insurancerating:::autoplot.truncated_dist(object),
    "`object` does not contain the required truncation attributes.",
    fixed = TRUE
  )
  object <- structure(
    list(distname = "truncated_gamma", estimate = c(shape = 2, scale = 1000)),
    class = "truncated_dist",
    lower_truncation = 500,
    upper_truncation = 5000,
    losses = c(600, 700, 800)
  )
  expect_error(
    autoplot(object, show_title = NA),
    "`show_title` must be TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    autoplot(object, y_limits = c(1, 0)),
    "`y_limits` must be a numeric vector of length 2 in increasing order.",
    fixed = TRUE
  )
})
