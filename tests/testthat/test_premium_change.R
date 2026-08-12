premium_change_objects <- function() {
  age <- rep(seq(20, 70, by = 5), each = 12)
  zone <- factor(rep(c("Urban", "Rural"), length.out = length(age)))
  exposure <- rep(c(0.5, 1, 1.5), length.out = length(age))
  tenure <- rep(seq(1, 12), length.out = length(age))
  expected <- exposure * exp(-2 + 0.025 * (age - 20) +
                               0.0004 * (age - 45)^2 +
                               0.15 * (zone == "Urban"))
  claims <- as.integer(expected > stats::median(expected)) +
    as.integer(seq_along(age) %% 11L == 0L)
  portfolio <- data.frame(
    claims = claims,
    exposure = exposure,
    age = age,
    tenure = tenure,
    zone = zone
  )
  portfolio$age_band <- cut(
    portfolio$age,
    breaks = c(15, 30, 45, 60, 75),
    include.lowest = TRUE
  )
  portfolio$tenure_band <- cut(
    portfolio$tenure,
    breaks = c(0, 4, 8, 12),
    include.lowest = TRUE
  )
  model <- glm(
    claims ~ age_band + tenure_band + zone + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  initial <- prepare_refinement(model, portfolio) |>
    add_smoothing(
      model_variable = "age_band",
      source_variable = "age",
      breaks = seq(15, 75, by = 5),
      smoothing = "poly",
      degree = 2,
      weights = "exposure"
    )
  edited <- initial |>
    edit_smoothing(
      model_variable = "age_band",
      from = 20,
      to = 60,
      adjustment = 1.05,
      transition = "linear"
    )
  carried <- initial |>
    add_restriction(data.frame(
      zone = "Urban",
      zone_restricted = 1.10
    ))
  list(
    portfolio = portfolio,
    model = model,
    initial = initial,
    edited = edited,
    carried = carried
  )
}

testthat::test_that("premium_change evaluates the effective smoothing line", {
  objects <- premium_change_objects()
  result <- premium_change(objects$initial, at = c(20, 25, 30))
  state <- preview_refinement(objects$initial, 1)$state$smoothing_states$step_1
  expected_from <- insurancerating:::.premium_change_evaluate(
    state$current_line,
    result$from
  )
  expected_to <- insurancerating:::.premium_change_evaluate(
    state$current_line,
    result$to
  )

  testthat::expect_s3_class(result, "premium_change")
  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(result, c(
    "variable", "basis", "step_id", "step_label", "from", "to",
    "relativity_from", "relativity_to", "premium_change"
  ))
  testthat::expect_equal(result$relativity_from, expected_from)
  testthat::expect_equal(result$relativity_to, expected_to)
  testthat::expect_equal(
    result$premium_change,
    expected_to / expected_from - 1
  )
})

testthat::test_that("segment basis uses effective tariff interval relativities", {
  objects <- premium_change_objects()
  result <- premium_change(
    objects$edited,
    at = c(20, 25, 30),
    basis = "segments"
  )
  state <- preview_refinement(objects$edited, 2)$state$smoothing_states$step_1
  expected_from <- insurancerating:::.premium_change_evaluate_segments(
    state$current_new,
    result$from
  )
  expected_to <- insurancerating:::.premium_change_evaluate_segments(
    state$current_new,
    result$to
  )

  testthat::expect_identical(unique(result$basis), "segments")
  testthat::expect_identical(attr(result, "basis"), "segments")
  testthat::expect_equal(result$relativity_from, expected_from)
  testthat::expect_equal(result$relativity_to, expected_to)
  testthat::expect_equal(result$premium_change,
                         expected_to / expected_from - 1)
  testthat::expect_output(print(result), "Basis: Tariff segments")
})

testthat::test_that("curve and segment bases answer distinct questions", {
  objects <- premium_change_objects()
  curve <- premium_change(objects$edited, at = c(22, 27), basis = "curve")
  segments <- premium_change(
    objects$edited,
    at = c(22, 27),
    basis = "segments"
  )

  testthat::expect_identical(unique(curve$basis), "curve")
  testthat::expect_false(isTRUE(all.equal(
    curve$premium_change,
    segments$premium_change
  )))
  testthat::expect_output(print(curve), "Basis: Effective smoothing curve")
})

testthat::test_that("premium_change uses edits and selected history states", {
  objects <- premium_change_objects()
  current <- premium_change(objects$edited, at = c(20, 25, 30))
  comparison <- premium_change(
    objects$edited,
    at = c(20, 25, 30),
    steps = c(1, 2)
  )

  testthat::expect_identical(unique(current$step_id), "step_2")
  testthat::expect_identical(unique(comparison$step_label),
                             c("Step 1", "Step 2"))
  testthat::expect_equal(
    current$premium_change,
    comparison$premium_change[comparison$step_id == "step_2"]
  )
  testthat::expect_false(isTRUE(all.equal(
    comparison$premium_change[comparison$step_id == "step_1"],
    comparison$premium_change[comparison$step_id == "step_2"]
  )))
  wide <- insurancerating:::.premium_change_wide(comparison)
  testthat::expect_named(wide,
                         c("from", "to", "Step 1", "Step 2", "Difference"))
  testthat::expect_equal(wide$Difference, wide$`Step 2` - wide$`Step 1`)
})

testthat::test_that("all states carry an unchanged smoothing forward", {
  objects <- premium_change_objects()
  result <- premium_change(objects$carried, at = c(20, 25), steps = "all")

  testthat::expect_identical(unique(result$step_label), c("Step 1", "Step 2"))
  testthat::expect_equal(
    result$premium_change[result$step_label == "Step 1"],
    result$premium_change[result$step_label == "Step 2"]
  )
})

testthat::test_that("automatic points remain inside the doubling range", {
  objects <- premium_change_objects()
  result <- premium_change(objects$initial)

  testthat::expect_lte(length(unique(result$from)), 6L)
  testthat::expect_gte(length(unique(result$from)), 5L)
  testthat::expect_true(all(result$from > 0))
  testthat::expect_true(all(result$to <= 75))
})

testthat::test_that("premium_change never extrapolates", {
  objects <- premium_change_objects()
  testthat::expect_error(
    premium_change(objects$initial, at = 40),
    "never extrapolates"
  )
  testthat::expect_error(
    premium_change(objects$initial, at = 0),
    "greater than zero"
  )
  testthat::expect_error(
    premium_change(objects$initial, basis = "points"),
    "arg"
  )
})

testthat::test_that("premium_change validates smoothing selection and steps", {
  objects <- premium_change_objects()
  second <- objects$initial |>
    add_smoothing(
      model_variable = "tenure_band",
      source_variable = "tenure",
      breaks = c(0, 4, 8, 12),
      smoothing = "poly",
      degree = 1
    )
  testthat::expect_error(premium_change(second), "multiple smoothed variables")
  testthat::expect_s3_class(
    premium_change(second, variable = "age", at = 20),
    "premium_change"
  )
  testthat::expect_error(
    premium_change(objects$initial, steps = 2),
    "refinement history"
  )
  plain <- prepare_refinement(objects$model, objects$portfolio)
  testthat::expect_error(premium_change(plain), "does not contain smoothing")
})

testthat::test_that("premium changes are invariant to common rebasing", {
  objects <- premium_change_objects()
  line <- preview_refinement(objects$initial, 1)$state$
    smoothing_states$step_1$current_line
  rebased <- line
  rebased$yhat <- 2.5 * rebased$yhat
  at <- c(20, 25, 30)
  original <- insurancerating:::.premium_change_evaluate(line, 2 * at) /
    insurancerating:::.premium_change_evaluate(line, at) - 1
  changed <- insurancerating:::.premium_change_evaluate(rebased, 2 * at) /
    insurancerating:::.premium_change_evaluate(rebased, at) - 1
  testthat::expect_equal(changed, original)
})

testthat::test_that("premium_change has concise print and gt methods", {
  objects <- premium_change_objects()
  one <- premium_change(objects$initial, at = c(20, 25))
  two <- premium_change(objects$edited, at = c(20, 25), steps = c(1, 2))

  testthat::expect_output(print(one), "Premium change for age")
  testthat::expect_output(print(one), "Premium change")
  testthat::expect_output(print(two), "Difference")

  testthat::skip_if_not_installed("gt")
  testthat::expect_s3_class(as_gt(one), "gt_tbl")
  testthat::expect_s3_class(as_gt(two), "gt_tbl")
})
