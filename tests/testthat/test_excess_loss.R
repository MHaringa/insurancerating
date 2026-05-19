context("excess_loss")

excess_loss_data <- data.frame(
  segment = rep(c("Industry", "Retail", "Office"), each = 6),
  claim_amount = c(
    1000, 25000, 120000, 8000, 45000, 170000,
    2000, 30000, 90000, 150000, 6000, 35000,
    1500, 12000, 18000, 22000, 30000, 40000
  ),
  earned_exposure = c(rep(1, 12), rep(2, 6)),
  base_premium = rep(1000, 18),
  include_in_loading = rep(c(TRUE, TRUE, FALSE), each = 6)
)

test_that("assess_excess_threshold calculates threshold diagnostics", {
  x <- assess_excess_threshold(
    excess_loss_data,
    claim_amount = "claim_amount",
    exposure = "earned_exposure",
    thresholds = c(50000, 100000)
  )

  expect_s3_class(x, "excess_threshold_assessment")
  expect_equal(x$capped_loss[x$threshold == 100000],
               sum(pmin(excess_loss_data$claim_amount, 100000)))
  expect_equal(x$excess_loss[x$threshold == 100000],
               sum(pmax(excess_loss_data$claim_amount - 100000, 0)))
  expect_true(all(c(
    "n_excess_claims",
    "excess_loss_ratio",
    "pure_premium_before",
    "pure_premium_after",
    "premium_impact"
  ) %in% names(x)))
  expect_s3_class(autoplot(x), "ggplot")
})

test_that("assess_excess_threshold works by group", {
  x <- assess_excess_threshold(
    excess_loss_data,
    claim_amount = "claim_amount",
    exposure = "earned_exposure",
    thresholds = c(50000, 100000),
    group = "segment"
  )

  expect_true("group" %in% names(x))
  expect_equal(names(x)[1:2], c("threshold", "group"))
  expect_equal(nrow(x), 6)
  expect_output(print(x), "Excess threshold assessment")
  expect_true("premium_impact" %in% names(summary(x)))
  expect_s3_class(autoplot(x, y = "n_excess_claims"), "ggplot")
})

test_that("calculate_excess_loss returns deterministic decomposition", {
  x <- calculate_excess_loss(
    excess_loss_data,
    claim_amount = "claim_amount",
    threshold = 100000
  )

  expect_s3_class(x, "excess_loss_decomposition")
  expect_equal(nrow(x), nrow(excess_loss_data))
  expect_equal(x$capped_claim_amount, pmin(excess_loss_data$claim_amount, 100000))
  expect_equal(x$excess_claim_amount, pmax(excess_loss_data$claim_amount - 100000, 0))
  expect_equal(x$is_excess_claim, excess_loss_data$claim_amount > 100000)
})

test_that("allocate_excess_loss supports observed portfolio pooling", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    weight = "earned_exposure",
    method = "observed",
    pooling = "portfolio"
  )

  expected_loading <- sum(decomposed$excess_claim_amount) /
    sum(decomposed$earned_exposure)

  expect_s3_class(allocation, "excess_loss_allocation")
  expect_equal(allocation$data$allocated_loading, rep(expected_loading, nrow(decomposed)))
  expect_equal(sum(allocation$data$allocated_excess_loss),
               expected_loading * sum(decomposed$earned_exposure))
})

test_that("allocate_excess_loss supports group pooling", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    weight = "earned_exposure",
    group = "segment",
    method = "observed",
    pooling = "group"
  )
  s <- summary(allocation, compare_to_empirical = TRUE)
  industry <- s[s$group == "Industry", ]
  expected <- sum(decomposed$excess_claim_amount[decomposed$segment == "Industry"]) /
    sum(decomposed$earned_exposure[decomposed$segment == "Industry"])

  expect_equal(industry$group_loading, expected)
  expect_equal(industry$credibility, 1)
  expect_true("empirical_excess_loss" %in% names(s))
})

test_that("allocate_excess_loss supports partial pooling", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    weight = "earned_exposure",
    group = "segment",
    method = "observed",
    pooling = "partial",
    credibility = 0.4
  )
  s <- summary(allocation)
  expected <- 0.4 * s$group_loading + 0.6 * s$portfolio_loading

  expect_equal(s$credibility, rep(0.4, nrow(s)))
  expect_equal(s$allocated_loading, expected)
})

test_that("automatic credibility is between zero and one", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    weight = "earned_exposure",
    group = "segment",
    pooling = "partial"
  )
  s <- summary(allocation)

  expect_true(all(s$credibility >= 0 & s$credibility <= 1))
})

test_that("include column restricts the allocation basis", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    weight = "earned_exposure",
    include = "include_in_loading",
    group = "segment",
    method = "observed",
    pooling = "portfolio"
  )

  expect_true(all(allocation$data$allocated_loading[!decomposed$include_in_loading] == 0))
  expect_true(all(allocation$data$allocated_excess_loss[!decomposed$include_in_loading] == 0))
})

test_that("bootstrap allocation is reproducible through set.seed", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  set.seed(123)
  x <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    weight = "earned_exposure",
    group = "segment",
    method = "bootstrap",
    pooling = "partial",
    n_boot = 25
  )
  set.seed(123)
  y <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    weight = "earned_exposure",
    group = "segment",
    method = "bootstrap",
    pooling = "partial",
    n_boot = 25
  )

  expect_equal(summary(x), summary(y))
  expect_true("bootstrap_loading_mean" %in% names(summary(x)))
})

test_that("bootstrap severity noise and tail threshold are validated", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)

  expect_error(
    allocate_excess_loss(
      decomposed,
      excess_amount = "excess_claim_amount",
      weight = "earned_exposure",
      method = "observed",
      severity_noise = "lognormal"
    ),
    "`severity_noise`"
  )
  expect_error(
    allocate_excess_loss(
      decomposed,
      excess_amount = "excess_claim_amount",
      weight = "earned_exposure",
      method = "observed",
      threshold = 100000,
      tail_fit_threshold = 50000
    ),
    "`tail_fit_threshold`"
  )
  expect_silent(
    allocate_excess_loss(
      decomposed,
      excess_amount = "excess_claim_amount",
      weight = "earned_exposure",
      method = "bootstrap",
      threshold = 100000,
      tail_fit_threshold = 50000,
      severity_noise = "lognormal",
      severity_noise_sd = 0.10,
      n_boot = 10
    )
  )
})

test_that("add_excess_loading adds premium columns", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    weight = "earned_exposure"
  )
  out <- add_excess_loading(decomposed, allocation)

  expect_equal(out$base_premium, decomposed$base_premium)
  expect_equal(out$excess_loading, allocation$data$allocated_loading)
  expect_equal(out$loaded_premium, out$base_premium + out$excess_loading)
})

test_that("print, summary and autoplot methods work", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    weight = "earned_exposure",
    group = "segment",
    method = "observed",
    pooling = "partial"
  )

  expect_output(print(allocation), "Excess loss allocation")
  expect_s3_class(autoplot(allocation), "ggplot")
})

test_that("standard evaluation inputs are validated", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)

  expect_error(
    assess_excess_threshold(excess_loss_data, claim_amount = "missing",
                            thresholds = 100000),
    "Column"
  )
  expect_error(
    calculate_excess_loss(excess_loss_data, claim_amount = "claim_amount",
                          threshold = 0),
    "`threshold`"
  )
  expect_error(
    allocate_excess_loss(decomposed, excess_amount = "missing",
                         weight = "earned_exposure"),
    "Column"
  )
})
