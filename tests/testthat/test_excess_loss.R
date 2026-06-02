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
    allocation_weight = "earned_exposure",
    method = "observed",
    allocation = "portfolio"
  )

  expected_loading <- sum(decomposed$excess_claim_amount) /
    sum(decomposed$earned_exposure)

  expect_s3_class(allocation, "excess_loss_allocation")
  expect_equal(allocation$data$allocated_loading, rep(expected_loading, nrow(decomposed)))
  expect_equal(sum(allocation$data$allocated_excess_loss),
               expected_loading * sum(decomposed$earned_exposure))
})

test_that("allocate_excess_loss supports risk-factor allocation", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "observed",
    allocation = "risk_factor"
  )
  s <- summary(allocation, compare_to_empirical = TRUE)
  industry <- s[s$group == "Industry", ]
  expected <- sum(decomposed$excess_claim_amount[decomposed$segment == "Industry"]) /
    sum(decomposed$earned_exposure[decomposed$segment == "Industry"])

  expect_equal(industry$group_loading, expected)
  expect_equal(industry$credibility, 1)
  expect_true("empirical_excess_loss" %in% names(s))
})

test_that("allocate_excess_loss supports partial allocation", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "observed",
    allocation = "partial",
    credibility = 0.4,
    preserve_total_excess = FALSE
  )
  s <- summary(allocation)
  expected <- 0.4 * s$group_loading + 0.6 * s$portfolio_loading

  expect_equal(s$credibility, rep(0.4, nrow(s)))
  expect_equal(s$allocated_loading, expected)
})

test_that("preserve_total_excess rescales partial allocation to the allocated burden", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "observed",
    allocation = "partial",
    credibility = 0.4,
    preserve_total_excess = TRUE
  )

  expect_equal(
    sum(allocation$data$allocated_excess_loss[allocation$data$included]),
    sum(decomposed$excess_claim_amount)
  )
})

test_that("automatic credibility is transparent and auditable", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    allocation = "partial"
  )
  s <- summary(allocation)

  expect_true(all(c(
    "group", "weight", "n_claims", "n_excess_claims",
    "historical_excess_loss", "excess_loss_ratio",
    "credibility_basis", "credibility_experience", "credibility_threshold",
    "group_loading", "portfolio_loading",
    "credibility", "allocated_loading", "allocated_excess_loss"
  ) %in% names(s)))
  expect_equal(s$credibility_basis, rep("claims", nrow(s)))
  expect_equal(s$credibility_experience, s$n_claims)
  expect_equal(s$credibility, s$n_claims / (s$n_claims + 50))
  expect_true(all(s$credibility >= 0 & s$credibility <= 1))
})

test_that("credibility_basis controls the experience measure", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  by_excess <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    allocation = "partial",
    credibility_basis = "excess_claims",
    credibility_threshold = 5,
    preserve_total_excess = FALSE
  )
  by_weight <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    allocation = "partial",
    credibility_basis = "allocation_weight",
    credibility_threshold = 5,
    preserve_total_excess = FALSE
  )
  s_excess <- summary(by_excess)
  s_weight <- summary(by_weight)

  expect_equal(s_excess$credibility_experience, s_excess$n_excess_claims)
  expect_equal(s_weight$credibility_experience, s_weight$weight)
  expect_equal(s_excess$credibility,
               s_excess$n_excess_claims / (s_excess$n_excess_claims + 5))
})

test_that("allocation_subset column restricts the allocation basis", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    allocation_subset = "include_in_loading",
    risk_factor = "segment",
    method = "observed",
    allocation = "portfolio"
  )

  expect_true(all(allocation$data$allocated_loading[!decomposed$include_in_loading] == 0))
  expect_true(all(allocation$data$allocated_excess_loss[!decomposed$include_in_loading] == 0))
})

test_that("bootstrap allocation is reproducible through bootstrap_seed", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  x <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "bootstrap",
    allocation = "partial",
    n_bootstrap = 25,
    bootstrap_seed = 123
  )
  y <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "bootstrap",
    allocation = "partial",
    n_bootstrap = 25,
    bootstrap_seed = 123
  )

  expect_equal(summary(x), summary(y))
  expect_true("bootstrap_loading_mean" %in% names(summary(x)))
})

test_that("bootstrap severity noise and preserve_total_excess are validated", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)

  expect_error(
    allocate_excess_loss(
      decomposed,
      excess_amount = "excess_claim_amount",
      allocation_weight = "earned_exposure",
      method = "observed",
      severity_noise = "lognormal"
    ),
    "`severity_noise`"
  )
  expect_error(
    allocate_excess_loss(
      decomposed,
      excess_amount = "excess_claim_amount",
      allocation_weight = "earned_exposure",
      preserve_total_excess = NA
    ),
    "`preserve_total_excess`"
  )
  expect_silent(
    allocate_excess_loss(
      decomposed,
      excess_amount = "excess_claim_amount",
      allocation_weight = "earned_exposure",
      method = "bootstrap",
      severity_noise = "lognormal",
      severity_noise_sd = 0.10,
      n_bootstrap = 10
    )
  )
})

test_that("manual credibility and credibility_scale are validated", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)

  expect_error(
    allocate_excess_loss(
      decomposed,
      excess_amount = "excess_claim_amount",
      allocation_weight = "earned_exposure",
      risk_factor = "segment",
      allocation = "partial",
      credibility_threshold = 0
    ),
    "`credibility_threshold`"
  )
  x <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    allocation = "partial",
    credibility_threshold = 50,
    credibility_scale = 0.5
  )
  y <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    allocation = "partial",
    credibility_threshold = 50,
    preserve_total_excess = FALSE
  )

  expect_equal(summary(x)$credibility, summary(y)$credibility * 0.5)

  manual <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    allocation = "partial",
    credibility = 0.4,
    preserve_total_excess = FALSE
  )
  expect_equal(summary(manual)$credibility, rep(0.4, nrow(summary(manual))))
})

test_that("apply_excess_loading adds premium columns", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure"
  )
  out <- apply_excess_loading(decomposed, allocation)

  expect_equal(out$base_premium, decomposed$base_premium)
  expect_equal(out$allocated_excess_loss, allocation$data$allocated_excess_loss)
  expect_equal(out$allocated_loading, allocation$data$allocated_loading)
  expect_equal(out$excess_loading, allocation$data$allocated_excess_loss)
  expect_equal(out$loaded_premium, out$base_premium + out$allocated_excess_loss)
})

test_that("apply_excess_loading can return rates", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure"
  )
  out <- apply_excess_loading(
    decomposed,
    allocation,
    weight = "earned_exposure",
    output = "rate"
  )

  expect_equal(out$base_rate, decomposed$base_premium / decomposed$earned_exposure)
  expect_equal(out$allocated_loading, allocation$data$allocated_loading)
  expect_equal(out$loaded_rate, out$base_rate + out$allocated_loading)
})

test_that("print, summary and autoplot methods work", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "excess_claim_amount",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "observed",
    allocation = "partial"
  )

  expect_output(print(allocation), "Excess loss allocation")
  expect_s3_class(autoplot(allocation), "ggplot")
  expect_s3_class(autoplot(allocation, top_n = 2, show_labels = TRUE), "ggplot")
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
                         allocation_weight = "earned_exposure"),
    "Column"
  )
})
