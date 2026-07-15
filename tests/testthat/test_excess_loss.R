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

  expect_s3_class(x, "threshold_assessment")
  expect_equal(x$capped_loss[x$threshold == 100000],
               sum(pmin(excess_loss_data$claim_amount, 100000)))
  expect_equal(x$excess_loss[x$threshold == 100000],
               sum(pmax(excess_loss_data$claim_amount - 100000, 0)))
  expect_true(all(c(
    "earned_exposure",
    "n_excess_records",
    "pure_premium_before",
    "pure_premium_after",
    "premium_reduction",
    "premium_reduction_ratio"
  ) %in% names(x)))
  expect_equal(names(x), c(
    "threshold",
    "earned_exposure",
    "n_claims",
    "n_excess_records",
    "total_loss",
    "capped_loss",
    "excess_loss",
    "pure_premium_before",
    "pure_premium_after",
    "premium_reduction",
    "premium_reduction_ratio"
  ))
  expect_null(getS3method("autoplot", "threshold_assessment", optional = TRUE))
})

test_that("assess_excess_threshold works by group", {
  x <- assess_excess_threshold(
    excess_loss_data,
    claim_amount = "claim_amount",
    exposure = "earned_exposure",
    thresholds = c(50000, 100000),
    group = "segment"
  )

  expect_true("segment" %in% names(x))
  expect_false("group" %in% names(x))
  expect_equal(names(x)[1:3], c("segment", "threshold", "earned_exposure"))
  expect_equal(x$segment, rep(c("Industry", "Office", "Retail"), each = 2))
  expect_equal(x$threshold, rep(c(50000, 100000), times = 3))
  expect_equal(nrow(x), 6)
  expect_output(print(x), "premium_reduction")
  expect_identical(summary(x), summary.data.frame(x))
  expect_null(getS3method("autoplot", "threshold_assessment", optional = TRUE))
})

test_that("assess_excess_threshold uses claim_count and fallback claim counts", {
  portfolio <- data.frame(
    policy_id = 1:10,
    sector = rep(c("Industry", "Retail"), each = 5),
    claim_count = c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1),
    claim_amount = c(
      0, 25000, 120000, 50000, 175000,
      0, 40000, 90000, 150000, 300000
    ),
    policy_years = rep(1, 10)
  )

  with_count <- assess_excess_threshold(
    data = portfolio,
    claim_amount = "claim_amount",
    thresholds = 100000,
    exposure = "policy_years",
    group = "sector",
    claim_count = "claim_count"
  )
  fallback <- assess_excess_threshold(
    data = portfolio,
    claim_amount = "claim_amount",
    thresholds = 100000,
    exposure = "policy_years",
    group = "sector"
  )

  expect_equal(names(with_count), c(
    "sector",
    "threshold",
    "policy_years",
    "n_claims",
    "n_excess_records",
    "total_loss",
    "capped_loss",
    "excess_loss",
    "pure_premium_before",
    "pure_premium_after",
    "premium_reduction",
    "premium_reduction_ratio"
  ))
  expect_equal(with_count$n_claims, c(4, 4))
  expect_equal(fallback$n_claims, c(4, 4))
  expect_equal(with_count$n_excess_records, c(2, 2))
  expect_equal(with_count$premium_reduction,
               with_count$pure_premium_before - with_count$pure_premium_after)
  expect_true(all(with_count$premium_reduction >= 0))
  expect_equal(with_count$premium_reduction_ratio,
               with_count$premium_reduction / with_count$pure_premium_before)
})

test_that("as_gt presents threshold assessments", {
  skip_if_not_installed("gt")

  portfolio <- data.frame(
    policy_id = 1:10,
    sector = rep(c("Industry", "Retail"), each = 5),
    claim_count = c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1),
    claim_amount = c(
      0, 25000, 120000, 50000, 175000,
      0, 40000, 90000, 150000, 300000
    ),
    policy_years = rep(1, 10)
  )

  grouped <- assess_excess_threshold(
    data = portfolio,
    claim_amount = "claim_amount",
    thresholds = c(25000, 100000),
    exposure = "policy_years",
    group = "sector",
    claim_count = "claim_count"
  )
  ungrouped <- assess_excess_threshold(
    data = portfolio,
    claim_amount = "claim_amount",
    thresholds = c(25000, 100000),
    exposure = "policy_years",
    claim_count = "claim_count"
  )

  tbl <- as_gt(grouped)
  expect_s3_class(tbl, "gt_tbl")
  expect_equal(tbl[["_locale"]]$locale, "nl-NL")
  expect_true("sector" %in% names(tbl[["_data"]]))
  expect_true("policy_years" %in% names(tbl[["_data"]]))
  expect_true("n_claims" %in% names(tbl[["_data"]]))
  expect_true("premium_reduction" %in% names(tbl[["_data"]]))
  expect_false("total_loss" %in% names(tbl[["_data"]]))
  expect_equal(attr(grouped, "group"), "sector")
  tbl_html <- as.character(gt::as_raw_html(tbl))
  expect_match(tbl_html, "Count")
  expect_match(tbl_html, "Above threshold")
  expect_match(tbl_html, "Risk premium")
  expect_match(tbl_html, "#F7E94D")
  expect_no_match(tbl_html, "Excess threshold assessment")
  expect_no_match(tbl_html, "Google")
  expect_no_match(tbl_html, "Inter")

  no_color_html <- as.character(gt::as_raw_html(
    as_gt(grouped, color_last_column = FALSE)
  ))
  expect_no_match(no_color_html, "#F7E94D")

  titled_tbl <- as_gt(grouped, title = "Threshold review", subtitle = "Large losses")
  titled_html <- as.character(gt::as_raw_html(titled_tbl))
  expect_match(titled_html, "Threshold review")
  expect_match(titled_html, "Large losses")

  tbl_en <- as_gt(grouped, locale = "en-US")
  expect_s3_class(tbl_en, "gt_tbl")
  expect_equal(tbl_en[["_locale"]]$locale, "en-US")

  expect_s3_class(as_gt(ungrouped), "gt_tbl")
  section_settings <- expand.grid(
    claims = c(TRUE, FALSE),
    loss = c(TRUE, FALSE),
    premium = c(TRUE, FALSE)
  )
  section_settings <- section_settings[
    section_settings$claims | section_settings$loss | section_settings$premium,
  ]
  for (i in seq_len(nrow(section_settings))) {
    expect_s3_class(
      as_gt(
        grouped,
        claims = section_settings$claims[i],
        loss = section_settings$loss[i],
        premium = section_settings$premium[i]
      ),
      "gt_tbl"
    )
  }
  expect_s3_class(as_gt(grouped, claims = FALSE), "gt_tbl")
  expect_false("n_claims" %in% names(as_gt(grouped, claims = FALSE)[["_data"]]))
  expect_s3_class(as_gt(grouped, loss = TRUE), "gt_tbl")
  expect_true("total_loss" %in% names(as_gt(grouped, loss = TRUE)[["_data"]]))
  expect_s3_class(as_gt(grouped, premium = FALSE), "gt_tbl")
  expect_false("premium_reduction" %in% names(as_gt(grouped, premium = FALSE)[["_data"]]))
  expect_s3_class(as_gt(
    grouped,
    loss_decimals = 1,
    premium_decimals = 2,
    ratio_decimals = 2
  ), "gt_tbl")
  expect_error(
    as_gt(grouped, claims = FALSE, loss = FALSE, premium = FALSE),
    "At least one"
  )
  expect_null(getS3method("autoplot", "threshold_assessment", optional = TRUE))
})

test_that("assess_excess_threshold uses record exposure when exposure is omitted", {
  portfolio <- data.frame(
    sector = rep(c("Industry", "Retail"), each = 5),
    claim_count = c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1),
    claim_amount = c(
      0, 25000, 120000, 50000, 175000,
      0, 40000, 90000, 150000, 300000
    )
  )

  x <- assess_excess_threshold(
    portfolio,
    claim_amount = "claim_amount",
    thresholds = c(100000, 25000),
    group = "sector",
    claim_count = "claim_count"
  )

  expect_equal(names(x)[1:3], c("sector", "threshold", "exposure"))
  expect_equal(x$exposure, rep(5, 4))
  expect_equal(x$sector, rep(c("Industry", "Retail"), each = 2))
  expect_equal(x$threshold, rep(c(100000, 25000), times = 2))
  expect_false(anyNA(x$pure_premium_before))
})

test_that("assess_excess_threshold validates claim_count and handles zero loss", {
  portfolio <- data.frame(
    claim_amount = c(0, 0),
    claim_count = c(0, 0),
    bad_claim_count = c(0, NA),
    policy_years = c(1, 1)
  )

  x <- assess_excess_threshold(
    portfolio,
    claim_amount = "claim_amount",
    thresholds = 100000,
    exposure = "policy_years",
    claim_count = "claim_count"
  )

  expect_equal(x$premium_reduction_ratio, 0)
  expect_error(
    assess_excess_threshold(
      portfolio,
      claim_amount = "claim_amount",
      thresholds = 100000,
      claim_count = "bad_claim_count"
    ),
    "`claim_count`"
  )
})

test_that("calculate_excess_loss returns deterministic decomposition", {
  x <- calculate_excess_loss(
    excess_loss_data,
    claim_amount = "claim_amount",
    threshold = 100000
  )

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), nrow(excess_loss_data))
  expect_equal(x$claim_amount_capped, pmin(excess_loss_data$claim_amount, 100000))
  expect_equal(x$claim_amount_excess, pmax(excess_loss_data$claim_amount - 100000, 0))
  expect_equal(x$claim_amount_is_excess, excess_loss_data$claim_amount > 100000)
})

test_that("calculate_excess_loss derives output names from claim_amount", {
  portfolio <- data.frame(
    policy_id = 1:4,
    n_claims = c(0, 1, 1, 0),
    incurred_loss = c(0, 120000, 30000, 0)
  )

  x <- calculate_excess_loss(
    portfolio,
    claim_amount = "incurred_loss",
    threshold = 100000
  )

  expect_true(all(c(
    "incurred_loss_capped",
    "incurred_loss_excess",
    "incurred_loss_is_excess"
  ) %in% names(x)))
  expect_false(any(c(
    "claim_amount_capped",
    "claim_amount_excess",
    "claim_amount_is_excess"
  ) %in% names(x)))
  expect_equal(x$incurred_loss_capped, c(0, 100000, 30000, 0))
  expect_equal(x$incurred_loss_excess, c(0, 20000, 0, 0))
  expect_equal(x$incurred_loss_is_excess, c(FALSE, TRUE, FALSE, FALSE))
})

test_that("allocate_excess_loss supports observed portfolio pooling", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    method = "observed",
    allocation = "portfolio"
  )

  expected_loading <- sum(decomposed$claim_amount_excess) /
    sum(decomposed$earned_exposure)

  expect_s3_class(allocation, "excess_allocation")
  expect_s3_class(allocation, "data.frame")
  expect_equal(allocation$allocated_excess_loading, rep(expected_loading, nrow(decomposed)))
  expect_equal(sum(allocation$allocated_excess_loss),
               expected_loading * sum(decomposed$earned_exposure))
})

test_that("allocate_excess_loss enriches calculate_excess_loss output", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "observed",
    allocation = "partial"
  )

  expect_s3_class(allocation, "excess_allocation")
  expect_equal(names(allocation)[seq_along(names(decomposed))], names(decomposed))
  expect_equal(allocation$segment, decomposed$segment)
  expect_equal(seq_len(nrow(allocation)), seq_len(nrow(decomposed)))
  expect_false("row_id" %in% names(allocation))
  expect_false("group" %in% names(allocation))
  expect_true("segment_excess_loading" %in% names(allocation))
  expect_true(all(c(
    "allocation_included", "portfolio_excess_loading", "credibility",
    "allocated_excess_loading", "allocated_excess_loss"
  ) %in% names(allocation)))
  expect_equal(attr(allocation, "excess_amount"), "claim_amount_excess")
  expect_equal(attr(allocation, "risk_factor"), "segment")
  expect_equal(
    allocation$allocated_excess_loss,
    allocation$allocated_excess_loading * allocation$earned_exposure
  )
  expect_equal(
    sum(allocation$allocated_excess_loss[allocation$allocation_included]),
    sum(allocation$claim_amount_excess)
  )

  expect_error(
    allocate_excess_loss(
      data.frame(claim_amount = c(0, 120000), earned_exposure = c(1, 1)),
      allocation_weight = "earned_exposure"
    ),
    "`excess_amount` is NULL"
  )
  with_collision <- decomposed
  with_collision$allocated_excess_loss <- 0
  expect_error(
    allocate_excess_loss(
      with_collision,
      allocation_weight = "earned_exposure"
    ),
    "already exist"
  )
})

test_that("allocate_excess_loss supports risk-factor allocation", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "observed",
    allocation = "risk_factor"
  )
  s <- summary(allocation, compare_to_empirical = TRUE)
  industry <- s[s$segment == "Industry", ]
  expected <- sum(decomposed$claim_amount_excess[decomposed$segment == "Industry"]) /
    sum(decomposed$earned_exposure[decomposed$segment == "Industry"])

  expect_equal(industry$segment_excess_loading, expected)
  expect_equal(industry$credibility, 1)
  expect_false("empirical_excess_loss" %in% names(s))
})

test_that("allocate_excess_loss supports partial allocation", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "observed",
    allocation = "partial",
    credibility = 0.4,
    preserve_total_excess = FALSE
  )
  s <- summary(allocation)
  expected <- 0.4 * s$segment_excess_loading + 0.6 * s$portfolio_excess_loading

  expect_equal(s$credibility, rep(0.4, nrow(s)))
  expect_equal(s$allocated_excess_loading, expected)
})

test_that("preserve_total_excess rescales partial allocation to the allocated burden", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "observed",
    allocation = "partial",
    credibility = 0.4,
    preserve_total_excess = TRUE
  )

  expect_equal(
    sum(allocation$allocated_excess_loss[allocation$allocation_included]),
    sum(decomposed$claim_amount_excess)
  )
})

test_that("automatic credibility is transparent and auditable", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    allocation = "partial"
  )
  s <- summary(allocation)

  expect_true(all(c(
    "segment", "earned_exposure", "claim_count", "excess_claim_count",
    "observed_excess_loss", "observed_excess_loading",
    "segment_excess_loading", "portfolio_excess_loading",
    "credibility", "allocated_excess_loading", "allocated_excess_loss"
  ) %in% names(s)))
  expect_false(any(c(
    "group", "weight", "n_claims", "n_excess_claims",
    "historical_excess_loss", "excess_loss_ratio",
    "credibility_basis", "credibility_experience", "credibility_threshold",
    "group_loading", "allocated_loading", "empirical_loss",
    "empirical_excess_loss"
  ) %in% names(s)))
  expect_equal(attr(s, "credibility_basis"), "claims")
  expect_equal(attr(s, "credibility_threshold"), 50)
  expect_equal(attr(s, "risk_factor"), "segment")
  expect_equal(attr(s, "allocation_weight"), "earned_exposure")
  expect_equal(s$credibility, s$claim_count / (s$claim_count + 50))
  expect_true(all(s$credibility >= 0 & s$credibility <= 1))
  expect_equal(
    s$observed_excess_loading,
    s$observed_excess_loss / s$earned_exposure
  )
  expect_equal(
    s$allocated_excess_loss,
    s$allocated_excess_loading * s$earned_exposure
  )
})

test_that("allocate_excess_loss uses claim_count and fallback claim counts", {
  portfolio <- data.frame(
    sector = c("Industry", "Industry", "Retail", "Retail"),
    claim_count = c(0, 2, 0, 1),
    claim_amount = c(0, 130000, 0, 160000),
    earned_exposure = c(1, 1, 1, 1)
  )
  decomposed <- calculate_excess_loss(portfolio, "claim_amount", 100000)

  fallback <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "sector",
    allocation = "partial",
    credibility_threshold = 10,
    preserve_total_excess = FALSE
  )
  with_count <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "sector",
    claim_count = "claim_count",
    allocation = "partial",
    credibility_threshold = 10,
    preserve_total_excess = FALSE
  )

  expect_equal(summary(fallback)$claim_count, c(1, 1))
  expect_equal(summary(with_count)$claim_count, c(2, 1))
  expect_equal(summary(fallback)$credibility,
               summary(fallback)$claim_count / (summary(fallback)$claim_count + 10))
  expect_error(
    allocate_excess_loss(
      transform(decomposed, bad_claim_count = c(0, NA, 0, 1)),
      excess_amount = "claim_amount_excess",
      allocation_weight = "earned_exposure",
      risk_factor = "sector",
      claim_count = "bad_claim_count"
    ),
    "`claim_count`"
  )
})

test_that("credibility_basis controls the experience measure", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  by_excess <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    allocation = "partial",
    credibility_basis = "excess_claims",
    credibility_threshold = 5,
    preserve_total_excess = FALSE
  )
  by_weight <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    allocation = "partial",
    credibility_basis = "allocation_weight",
    credibility_threshold = 5,
    preserve_total_excess = FALSE
  )
  s_excess <- summary(by_excess)
  s_weight <- summary(by_weight)

  expect_equal(attr(s_excess, "credibility_basis"), "excess_claims")
  expect_equal(attr(s_weight, "credibility_basis"), "allocation_weight")
  expect_equal(s_excess$credibility,
               s_excess$excess_claim_count / (s_excess$excess_claim_count + 5))
  expect_equal(s_weight$credibility,
               s_weight$earned_exposure / (s_weight$earned_exposure + 5))
})

test_that("summary.excess_allocation adds empirical comparison columns on request", {
  portfolio <- data.frame(
    sector = factor(c("Industry", "Industry", "Retail", "Retail")),
    claim_amount = c(0, 120000, 0, 50000),
    earned_exposure = c(1, 1, 1, 1)
  )
  decomposed <- calculate_excess_loss(portfolio, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    allocation_weight = "earned_exposure",
    risk_factor = "sector",
    allocation = "partial",
    credibility = 0.5,
    preserve_total_excess = TRUE
  )

  s <- summary(allocation)
  s_compare <- summary(allocation, compare_to_empirical = TRUE)

  expect_s3_class(s$sector, "factor")
  expect_false(any(c("allocation_difference", "allocation_difference_ratio") %in% names(s)))
  expect_true(all(c("allocation_difference", "allocation_difference_ratio") %in% names(s_compare)))
  expect_equal(
    s_compare$allocation_difference,
    s_compare$allocated_excess_loss - s_compare$observed_excess_loss
  )
  expect_equal(
    s_compare$allocation_difference_ratio[s_compare$observed_excess_loss > 0],
    s_compare$allocation_difference[s_compare$observed_excess_loss > 0] /
      s_compare$observed_excess_loss[s_compare$observed_excess_loss > 0]
  )
  expect_true(all(is.na(
    s_compare$allocation_difference_ratio[s_compare$observed_excess_loss == 0]
  )))
})

test_that("allocation_subset column restricts the allocation basis", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    allocation_subset = "include_in_loading",
    risk_factor = "segment",
    method = "observed",
    allocation = "portfolio"
  )

  expect_true(all(allocation$allocated_excess_loading[!decomposed$include_in_loading] == 0))
  expect_true(all(allocation$allocated_excess_loss[!decomposed$include_in_loading] == 0))
})

test_that("bootstrap allocation is reproducible through bootstrap_seed", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  x <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "bootstrap",
    allocation = "partial",
    n_bootstrap = 25,
    bootstrap_seed = 123
  )
  y <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "bootstrap",
    allocation = "partial",
    n_bootstrap = 25,
    bootstrap_seed = 123
  )

  expect_equal(summary(x), summary(y))
  expect_false("bootstrap_loading_mean" %in% names(summary(x)))
})

test_that("bootstrap severity noise and preserve_total_excess are validated", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)

  expect_error(
    allocate_excess_loss(
      decomposed,
      excess_amount = "claim_amount_excess",
      allocation_weight = "earned_exposure",
      method = "observed",
      severity_noise = "lognormal"
    ),
    "`severity_noise`"
  )
  expect_error(
    allocate_excess_loss(
      decomposed,
      excess_amount = "claim_amount_excess",
      allocation_weight = "earned_exposure",
      preserve_total_excess = NA
    ),
    "`preserve_total_excess`"
  )
  expect_silent(
    allocate_excess_loss(
      decomposed,
      excess_amount = "claim_amount_excess",
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
      excess_amount = "claim_amount_excess",
      allocation_weight = "earned_exposure",
      risk_factor = "segment",
      allocation = "partial",
      credibility_threshold = 0
    ),
    "`credibility_threshold`"
  )
  x <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    allocation = "partial",
    credibility_threshold = 50,
    credibility_scale = 0.5
  )
  y <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    allocation = "partial",
    credibility_threshold = 50,
    preserve_total_excess = FALSE
  )

  expect_equal(summary(x)$credibility, summary(y)$credibility * 0.5)

  manual <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
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
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure"
  )
  out <- apply_excess_loading(decomposed, allocation)

  expect_equal(out$base_premium, decomposed$base_premium)
  expect_equal(out$allocated_excess_loss, allocation$allocated_excess_loss)
  expect_equal(out$allocated_excess_loading, allocation$allocated_excess_loading)
  expect_equal(out$excess_loading, allocation$allocated_excess_loss)
  expect_equal(out$loaded_premium, out$base_premium + out$allocated_excess_loss)
})

test_that("apply_excess_loading can return rates", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure"
  )
  out <- apply_excess_loading(
    decomposed,
    allocation,
    weight = "earned_exposure",
    output = "rate"
  )

  expect_equal(out$base_rate, decomposed$base_premium / decomposed$earned_exposure)
  expect_equal(out$allocated_excess_loading, allocation$allocated_excess_loading)
  expect_equal(out$loaded_rate, out$base_rate + out$allocated_excess_loading)
})

test_that("print, summary and autoplot methods work", {
  decomposed <- calculate_excess_loss(excess_loss_data, "claim_amount", 100000)
  allocation <- allocate_excess_loss(
    decomposed,
    excess_amount = "claim_amount_excess",
    allocation_weight = "earned_exposure",
    risk_factor = "segment",
    method = "observed",
    allocation = "partial"
  )

  expect_output(print(allocation), "allocated_excess_loss")
  expect_null(getS3method("print", "excess_allocation", optional = TRUE))
  expect_s3_class(summary(allocation), "data.frame")
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
