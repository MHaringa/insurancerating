premium_change_constraint_refinement <- function(constraint = TRUE) {
  age <- rep(seq(20, 70, by = 5), each = 100)
  exposure <- rep(c(0.5, 1, 1.5), length.out = length(age))
  position <- rep(seq_len(100), times = length(unique(age)))
  claims <- as.integer(position <= round(12 + 0.1 * (age - 20)))
  data <- data.frame(claims = claims, exposure = exposure, age = age)
  data$age_band <- cut(
    data$age, breaks = c(15, 30, 45, 60, 75), include.lowest = TRUE
  )
  model <- glm(
    claims ~ age_band + offset(log(exposure)),
    family = poisson(), data = data
  )
  args <- list(
    model = prepare_refinement(model, data),
    model_variable = "age_band",
    source_variable = "age",
    breaks = seq(15, 75, by = 5),
    smoothing = "poly",
    degree = 1,
    weights = "exposure"
  )
  if (isTRUE(constraint)) {
    args$premium_change <- "non_increasing"
    args$premium_change_step <- 5
  }
  do.call(add_smoothing, args)
}

testthat::test_that("premium-change constraints are optional and validated", {
  unconstrained <- premium_change_constraint_refinement(FALSE)
  testthat::expect_null(unconstrained$steps[[1L]]$premium_change)
  testthat::expect_error(
    add_smoothing(
      prepare_refinement(unconstrained$base$model, unconstrained$base$data),
      model_variable = "age_band", source_variable = "age",
      breaks = seq(15, 75, by = 5), smoothing = "poly", degree = 1,
      premium_change = "non_increasing"
    ),
    "premium_change_step.*required"
  )
})

testthat::test_that("add_smoothing enforces non-increasing percentage changes", {
  refinement <- premium_change_constraint_refinement()
  state <- preview_refinement(refinement, 1L)$state
  line <- state$smoothing_states$step_1$current_line
  check <- insurancerating:::.check_premium_change_constraint(line, 5)

  testthat::expect_true(check$valid)
  testthat::expect_true(all(diff(check$values) <= check$tolerance))
  testthat::expect_identical(
    refinement$steps[[1L]]$premium_change, "non_increasing"
  )
  testthat::expect_identical(refinement$steps[[1L]]$premium_change_step, 5)
})

testthat::test_that("constraint calculation uses a ratio and the supplied step", {
  line <- data.frame(x = 0:20, yhat = (0:20 + 10)^2)
  names(line)[1L] <- "amount"
  values_2 <- insurancerating:::.incremental_premium_change(line, 2)
  values_5 <- insurancerating:::.incremental_premium_change(line, 5)

  expected <- ((values_2$x + 2 + 10)^2 / (values_2$x + 10)^2) - 1
  testthat::expect_equal(values_2$incremental_change, expected)
  testthat::expect_false(isTRUE(all.equal(
    values_2$incremental_change, values_5$incremental_change
  )))
  testthat::expect_true(max(values_5$x + 5) <= 20)
})

testthat::test_that("edit_smoothing inherits, replaces and removes constraints", {
  initial <- premium_change_constraint_refinement()
  edited <- initial |>
    edit_smoothing(
      model_variable = "age_band", from = 30, to = 60,
      adjustment = 1.01, transition = "linear"
    )
  edited_state <- preview_refinement(edited, 2L)$state
  edited_smoothing <- edited_state$smoothing_states$step_1
  testthat::expect_identical(
    edited_smoothing$premium_change, "non_increasing"
  )
  testthat::expect_true(
    insurancerating:::.check_premium_change_constraint(
      edited_smoothing$current_line, 5
    )$valid
  )

  replaced <- edited |>
    edit_smoothing(
      model_variable = "age_band",
      premium_change = "non_increasing",
      premium_change_step = 10
    )
  replaced_state <- preview_refinement(replaced, 3L)$state
  testthat::expect_identical(
    replaced_state$smoothing_states$step_1$premium_change_step, 10
  )

  removed <- replaced |>
    edit_smoothing(model_variable = "age_band", premium_change = "none")
  removed_state <- preview_refinement(removed, 4L)$state
  testthat::expect_null(
    removed_state$smoothing_states$step_1$premium_change
  )
})

testthat::test_that("edit_smoothing can constrain only a selected range", {
  unconstrained <- premium_change_constraint_refinement(FALSE)
  original_line <- preview_refinement(unconstrained, 1L)$state$new_line
  ranged <- unconstrained |>
    edit_smoothing(
      model_variable = "age_band",
      from = 35,
      premium_change = "non_increasing",
      premium_change_step = 5
    )
  ranged_state <- preview_refinement(ranged, 2L)$state
  ranged_smoothing <- ranged_state$smoothing_states$step_1

  before <- original_line$age < 35
  testthat::expect_equal(
    ranged_smoothing$current_line$yhat[
      match(original_line$age[before], ranged_smoothing$current_line$age)
    ],
    original_line$yhat[before]
  )
  testthat::expect_true(
    insurancerating:::.check_premium_change_constraint(
      ranged_smoothing$current_line, 5, from = 35
    )$valid
  )
  testthat::expect_identical(ranged_smoothing$premium_change_from, 35)
  testthat::expect_null(ranged_smoothing$premium_change_to)

  upper <- unconstrained |>
    edit_smoothing(
      model_variable = "age_band",
      to = 55,
      premium_change = "non_increasing",
      premium_change_step = 5
    )
  upper_smoothing <- preview_refinement(upper, 2L)$state$smoothing_states$step_1
  testthat::expect_true(
    insurancerating:::.check_premium_change_constraint(
      upper_smoothing$current_line, 5, to = 55
    )$valid
  )
  testthat::expect_null(upper_smoothing$premium_change_from)
  testthat::expect_identical(upper_smoothing$premium_change_to, 55)
})

testthat::test_that("premium-change constraint ranges are validated", {
  refinement <- premium_change_constraint_refinement(FALSE)
  outside <- refinement |>
    edit_smoothing(
      model_variable = "age_band", from = 10,
      premium_change = "non_increasing", premium_change_step = 5
    )
  testthat::expect_error(
    preview_refinement(outside, 2L),
    "must lie inside the supported smoothing range"
  )

  too_narrow <- refinement |>
    edit_smoothing(
      model_variable = "age_band", from = 35, to = 38,
      premium_change = "non_increasing", premium_change_step = 5
    )
  testthat::expect_error(
    preview_refinement(too_narrow, 2L),
    "too large for the selected premium-change range"
  )
})

testthat::test_that("incremental-change plots use the constrained effective curve", {
  refinement <- premium_change_constraint_refinement()
  plot <- ggplot2::autoplot(
    refinement, type = "incremental_change", step = 5
  )
  state <- preview_refinement(refinement, 1L)$state
  line <- state$smoothing_states$step_1$current_line
  expected <- insurancerating:::.incremental_premium_change(
    line, 5, at = plot$data$x, percent = TRUE
  )

  testthat::expect_equal(
    plot$data$incremental_change, expected$incremental_change
  )
  testthat::expect_true(all(diff(plot$data$incremental_change) <= 1e-6))
})

testthat::test_that("premium-change constraint is visible in audit metadata", {
  refinement <- premium_change_constraint_refinement()
  audit <- audit_refinement(refit(refinement, intercept_only = TRUE))
  testthat::expect_match(audit$steps$details[1L], "non-increasing")
  testthat::expect_match(audit$steps$details[1L], "increment = 5")
})

testthat::test_that("constraint preserves feasible increasing-concave structure", {
  x <- seq(0, 100, by = 5)
  line <- data.frame(amount = x, yhat = 1 + sqrt(x + 1) / 10)
  result <- insurancerating:::.enforce_premium_change_constraint(
    smooth = line, line = line,
    new_rf = data.frame(
      risk_factor = "amount_smooth", level = as.character(x), yhat = line$yhat
    ),
    source_variable = "amount", constraint = "non_increasing", step = 10,
    smoothing = "increasing_concave"
  )

  slopes <- diff(result$line$yhat) / diff(result$line$amount)
  testthat::expect_true(all(slopes >= -1e-10))
  testthat::expect_true(all(diff(slopes) <= 1e-10))
  testthat::expect_true(
    insurancerating:::.check_premium_change_constraint(result$line, 10)$valid
  )
})
