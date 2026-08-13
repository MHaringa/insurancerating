smoothing_scale_refinement <- function(scale = "relativity",
                                       smoothing = "increasing_concave") {
  amount <- rep(seq(100, 600, by = 50), each = 80)
  exposure <- rep(c(0.5, 1, 1.5, 2), length.out = length(amount))
  band <- cut(amount, breaks = c(50, 200, 350, 500, 650),
              include.lowest = TRUE)
  level_effect <- c(1, 1.35, 1.65, 1.85)[as.integer(band)]
  position <- rep(seq_len(80), times = length(unique(amount)))
  claims <- as.integer(position <= round(8 * exposure * level_effect))
  data <- data.frame(claims, exposure, amount, band)
  model <- glm(
    claims ~ band + offset(log(exposure)),
    family = poisson(), data = data
  )
  prepare_refinement(model, data) |>
    add_smoothing(
      model_variable = "band",
      source_variable = "amount",
      breaks = seq(50, 650, by = 25),
      smoothing = smoothing,
      k = 4,
      weights = "exposure",
      scale = scale
    )
}

testthat::test_that("relativity is the backwards-compatible smoothing scale", {
  implicit <- smoothing_scale_refinement()
  explicit <- smoothing_scale_refinement(scale = "relativity")
  testthat::expect_identical(implicit$steps[[1L]]$scale, "relativity")
  testthat::expect_equal(
    preview_refinement(implicit)$state$new_line,
    preview_refinement(explicit)$state$new_line
  )
  testthat::expect_error(
    smoothing_scale_refinement(scale = "link"),
    "relativity.*log_relativity"
  )
})

testthat::test_that("log-relativity smoothing is positive increasing and concave", {
  refinement <- smoothing_scale_refinement(scale = "log_relativity")
  state <- preview_refinement(refinement)$state
  line <- state$new_line
  x <- line$amount
  y <- line$yhat
  log_slopes <- diff(log(y)) / diff(x)

  testthat::expect_true(all(y > 0))
  testthat::expect_true(all(log_slopes >= -1e-8))
  testthat::expect_true(all(diff(log_slopes) <= 1e-7))
  testthat::expect_identical(
    state$smoothing_states$step_1$scale, "log_relativity"
  )
})

testthat::test_that("log-relativity smoothing rejects non-positive inputs", {
  grouped <- data.frame(
    risk_factor = "band",
    start_ = c(0, 10, 20),
    end_ = c(10, 20, 30),
    avg_ = c(5, 15, 25),
    estimate = c(1, 0, 1.2)
  )
  testthat::expect_error(
    insurancerating:::fit_smoothing_curve(
      grouped,
      x_org = "amount",
      breaks = c(0, 10, 20, 30),
      smoothing = "micv",
      k = 3,
      weights = rep(1, 3),
      scale = "log_relativity"
    ),
    "positive finite fitted relativities"
  )
})

testthat::test_that("log concavity implies diminishing fixed-step changes", {
  refinement <- smoothing_scale_refinement(scale = "log_relativity")
  line <- preview_refinement(refinement)$state$new_line
  for (increment in c(25, 50, 100)) {
    values <- insurancerating:::.incremental_premium_change(
      line, increment, scale = "log_relativity"
    )$incremental_change
    testthat::expect_true(all(diff(values) <= 1e-7))
  }
})

testthat::test_that("edit_smoothing inherits its scale and cannot reinterpret it", {
  refinement <- smoothing_scale_refinement(scale = "log_relativity")
  edited <- refinement |>
    edit_smoothing(
      model_variable = "band", from = 200, to = 500,
      adjustment = 1.001, transition = "linear"
    )
  testthat::expect_identical(edited$steps[[2L]]$edit$scale, "log_relativity")
  testthat::expect_identical(
    preview_refinement(edited)$state$smoothing_states$step_1$scale,
    "log_relativity"
  )
  testthat::expect_error(
    edit_smoothing(
      refinement, model_variable = "band", from = 200, to = 500,
      adjustment = 1.001, scale = "relativity"
    ),
    "cannot change the smoothing scale"
  )
})

testthat::test_that("historical smoothing states retain their own scale", {
  refinement <- smoothing_scale_refinement(scale = "log_relativity") |>
    edit_smoothing(
      model_variable = "band", from = 200, to = 500,
      adjustment = 1.001, transition = "linear"
    )
  first <- preview_refinement(refinement, upto = 1)$state$
    smoothing_states$step_1
  second <- preview_refinement(refinement, upto = 2)$state$
    smoothing_states$step_1
  testthat::expect_identical(first$scale, "log_relativity")
  testthat::expect_identical(second$scale, "log_relativity")
  testthat::expect_s3_class(
    premium_change(refinement, at = c(100, 150, 200), steps = c(1, 2)),
    "premium_change"
  )
})

testthat::test_that("log-scale shape is invariant to positive rebasing", {
  refinement <- smoothing_scale_refinement(scale = "log_relativity")
  line <- preview_refinement(refinement)$state$new_line
  shifted <- line
  shifted$yhat <- 7 * shifted$yhat
  testthat::expect_silent(
    insurancerating:::.validate_adjusted_smoothing_shape(
      shifted$amount, shifted$yhat, "increasing_concave",
      scale = "log_relativity"
    )
  )
  a <- insurancerating:::.incremental_premium_change(
    line, 50, scale = "log_relativity"
  )$incremental_change
  b <- insurancerating:::.incremental_premium_change(
    shifted, 50, scale = "log_relativity"
  )$incremental_change
  testthat::expect_equal(a, b)
})

testthat::test_that("incremental-change plot uses the effective log curve", {
  refinement <- smoothing_scale_refinement(scale = "log_relativity")
  plot <- ggplot2::autoplot(
    refinement, type = "incremental_change", step = 50
  )
  line <- preview_refinement(refinement)$state$new_line
  expected <- insurancerating:::.incremental_premium_change(
    line, 50, at = plot$data$x, percent = TRUE,
    scale = "log_relativity"
  )
  testthat::expect_equal(
    plot$data$incremental_change, expected$incremental_change
  )
  testthat::expect_true(all(diff(plot$data$incremental_change) <= 1e-6))
})

testthat::test_that("scale is visible in refinement audit metadata", {
  refinement <- smoothing_scale_refinement(scale = "log_relativity")
  audit <- audit_refinement(refit(refinement, intercept_only = TRUE))
  testthat::expect_match(audit$steps$details[1L], "scale = log_relativity")
})

testthat::test_that("refit uses ordinary positive smoothed relativities", {
  refinement <- smoothing_scale_refinement(scale = "log_relativity")
  state <- preview_refinement(refinement)$state
  fitted <- refit(refinement, intercept_only = TRUE)
  table <- rating_table(fitted)
  smooth_rows <- table$risk_factor == "amount_smooth"
  testthat::expect_true(any(smooth_rows))
  estimate <- table[[grep("^est_", names(table), value = TRUE)[1L]]]
  testthat::expect_true(all(estimate[smooth_rows] > 0))
  expected <- state$new_rf$yhat
  actual <- estimate[smooth_rows]
  testthat::expect_equal(
    actual / actual[1L], expected / expected[1L], tolerance = 1e-6
  )
})

testthat::test_that("removed development-only constraint arguments are unsupported", {
  testthat::expect_false("premium_change" %in% names(formals(add_smoothing)))
  testthat::expect_false("premium_change_step" %in% names(formals(add_smoothing)))
  testthat::expect_false("premium_change" %in% names(formals(edit_smoothing)))
  testthat::expect_false("premium_change_step" %in% names(formals(edit_smoothing)))
})
