context("excess_loss")

excess_loss_data <- data.frame(
  segment = rep(c("A", "B", "C"), each = 6),
  claim_amount = c(
    1000, 25000, 120000, 8000, 45000, 170000,
    2000, 30000, 90000, 150000, 6000, 35000,
    1500, 12000, 18000, 22000, 30000, 40000
  ),
  earned_exposure = c(rep(1, 12), rep(2, 6)),
  earned_premium = rep(10000, 18),
  excess_factor = c(rep(1.4, 6), rep(0.6, 6), rep(0, 6))
)

calculate_base_excess <- function(...) {
  calculate_excess_loss(
    data = excess_loss_data,
    claim_amount = "claim_amount",
    exposure = "earned_exposure",
    fit_threshold = 20000,
    excess_threshold = 100000,
    method = "empirical",
    allocation_method = "exposure",
    allocation_by = "segment",
    allocation_levels = c("A", "B"),
    allocation_weights = "earned_exposure",
    ...
  )
}

test_that("assess_excess_thresholds calculates threshold diagnostics", {
  x <- assess_excess_thresholds(
    excess_loss_data,
    claim_amount = "claim_amount",
    exposure = "earned_exposure",
    thresholds = c(50000, 100000),
    premium = "earned_premium"
  )

  expect_s3_class(x, "excess_threshold_assessment")
  expect_equal(x$capped_loss[x$threshold == 100000],
               sum(pmin(excess_loss_data$claim_amount, 100000)))
  expect_equal(x$excess_loss[x$threshold == 100000],
               sum(pmax(excess_loss_data$claim_amount - 100000, 0)))
  expect_true("excess_as_premium_pct" %in% names(x))
  expect_s3_class(autoplot(x), "ggplot")
})

test_that("assess_excess_thresholds works by group", {
  x <- assess_excess_thresholds(
    excess_loss_data,
    claim_amount = "claim_amount",
    exposure = "earned_exposure",
    thresholds = c(50000, 100000),
    by = "segment"
  )

  expect_true("group" %in% names(x))
  expect_equal(names(x)[1:2], c("threshold", "group"))
  expect_equal(nrow(x), 6)
  expect_output(print(x), "Excess threshold assessment")
  expect_true("excess_per_exposure" %in% names(summary(x)))
  expect_s3_class(autoplot(x, y = "claims_above"), "ggplot")
})

test_that("calculate_excess_loss returns an excess loss vector", {
  x <- calculate_base_excess()

  expect_type(x, "double")
  expect_s3_class(x, "excess_loss_vector")
  expect_equal(class(x), c("excess_loss_vector", "numeric"))
  expect_equal(length(x), nrow(excess_loss_data))
})

test_that("empirical total excess equals observed excess", {
  x <- calculate_base_excess()

  expect_equal(
    attr(x, "total_excess"),
    sum(pmax(excess_loss_data$claim_amount - 100000, 0))
  )
  expect_equal(sum(attr(x, "amount_vector")), attr(x, "total_excess"))
})

test_that("output amount, share, factor and base work", {
  amount <- calculate_base_excess(output = "amount")
  share <- calculate_base_excess(output = "share")
  factor <- calculate_base_excess(output = "factor")
  base <- calculate_base_excess(output = "base")

  expect_equal(as.numeric(amount), attr(amount, "amount_vector"))
  expect_equal(as.numeric(share), attr(share, "share_vector"))
  expect_equal(as.numeric(factor), attr(factor, "factor_vector"))
  expect_equal(as.numeric(base), attr(base, "base_vector"))
  expect_equal(sum(share), 1)
})

test_that("allocation_method exposure gives factor one for selected rows", {
  x <- calculate_base_excess()
  selected <- excess_loss_data$segment %in% c("A", "B")

  expect_true(all(attr(x, "factor_vector")[selected] == 1))
  expect_true(all(attr(x, "amount_vector")[!selected] == 0))
  expect_true(all(attr(x, "share_vector")[!selected] == 0))
  expect_true(all(attr(x, "factor_vector")[!selected] == 0))
  expect_true(all(attr(x, "base_vector")[!selected] == 0))
})

test_that("allocation_method factor uses supplied factor", {
  x <- calculate_excess_loss(
    data = excess_loss_data,
    claim_amount = "claim_amount",
    exposure = "earned_exposure",
    fit_threshold = 20000,
    excess_threshold = 100000,
    method = "manual",
    manual_excess = 12345,
    allocation_method = "factor",
    allocation_factor = "excess_factor",
    allocation_weights = "earned_exposure"
  )

  expect_equal(attr(x, "total_excess"), 12345)
  expect_equal(attr(x, "factor_vector"), excess_loss_data$excess_factor)
})

test_that("manual method requires manual_excess", {
  expect_error(
    calculate_excess_loss(
      data = excess_loss_data,
      claim_amount = "claim_amount",
      exposure = "earned_exposure",
      fit_threshold = 20000,
      excess_threshold = 100000,
      method = "manual",
      allocation_method = "factor",
      allocation_factor = "excess_factor"
    ),
    "`manual_excess`"
  )
})

test_that("bootstrap is reproducible with seed", {
  args <- list(
    data = excess_loss_data,
    claim_amount = "claim_amount",
    exposure = "earned_exposure",
    fit_threshold = 20000,
    excess_threshold = 100000,
    method = "bootstrap",
    allocation_method = "exposure",
    allocation_by = "segment",
    allocation_levels = c("A", "B"),
    allocation_weights = "earned_exposure",
    bootstrap_samples = 50,
    bootstrap_seed = 123
  )

  x <- do.call(calculate_excess_loss, args)
  y <- do.call(calculate_excess_loss, args)

  expect_equal(attr(x, "bootstrap_samples_vector"),
               attr(y, "bootstrap_samples_vector"))
})

test_that("bootstrap_smooth is reproducible with seed", {
  args <- list(
    data = excess_loss_data,
    claim_amount = "claim_amount",
    exposure = "earned_exposure",
    fit_threshold = 20000,
    excess_threshold = 100000,
    method = "bootstrap",
    allocation_method = "bootstrap_excess",
    allocation_by = "segment",
    allocation_levels = c("A", "B"),
    allocation_weights = "earned_exposure",
    bootstrap_samples = 50,
    bootstrap_smooth = TRUE,
    bootstrap_seed = 456
  )

  x <- do.call(calculate_excess_loss, args)
  y <- do.call(calculate_excess_loss, args)

  expect_equal(attr(x, "bootstrap_samples_vector"),
               attr(y, "bootstrap_samples_vector"))
  expect_true("bootstrap_mean" %in% names(summary(x)))
})

test_that("add_excess_loss accepts only excess_loss_vector", {
  expect_error(
    add_excess_loss(excess_loss_data, rep(0, nrow(excess_loss_data))),
    "`x`"
  )
})

test_that("add_excess_loss uses attributes and adds columns", {
  x <- calculate_base_excess()
  y <- x
  y[] <- 999

  out <- add_excess_loss(
    excess_loss_data,
    y,
    name = "large_loss",
    include = c("amount", "share", "factor", "base")
  )

  expect_equal(out$large_loss, attr(x, "amount_vector"))
  expect_equal(out$large_loss_share, attr(x, "share_vector"))
  expect_equal(out$large_loss_factor, attr(x, "factor_vector"))
  expect_equal(out$large_loss_base, attr(x, "base_vector"))
})

test_that("add_excess_loss validates overwrite and include", {
  x <- calculate_base_excess()
  with_col <- excess_loss_data
  with_col$excess_loss <- 1

  expect_error(add_excess_loss(with_col, x), "already exist")
  expect_silent(add_excess_loss(with_col, x, overwrite = TRUE))
  expect_silent(add_excess_loss(excess_loss_data, x,
                                include = c("amount", "share", "factor", "base")))
})

test_that("allocation_factor works for vector, data and summary", {
  x <- calculate_base_excess()

  expect_equal(allocation_factor(x, type = "vector"), attr(x, "factor_vector"))
  expect_equal(allocation_factor(x, type = "data"), attr(x, "allocation_data"))
  expect_equal(allocation_factor(x, type = "summary"), attr(x, "allocation_summary"))
})

test_that("standard evaluation columns and allocation factor are validated", {
  expect_error(
    calculate_excess_loss(
      data = excess_loss_data,
      claim_amount = "missing",
      exposure = "earned_exposure",
      fit_threshold = 20000,
      excess_threshold = 100000,
      method = "empirical",
      allocation_method = "exposure",
      allocation_by = "segment",
      allocation_levels = c("A", "B")
    ),
    "Column"
  )

  bad <- excess_loss_data
  bad$excess_factor[1] <- NA_real_
  expect_error(
    calculate_excess_loss(
      data = bad,
      claim_amount = "claim_amount",
      exposure = "earned_exposure",
      fit_threshold = 20000,
      excess_threshold = 100000,
      method = "manual",
      manual_excess = 1000,
      allocation_method = "factor",
      allocation_factor = "excess_factor"
    ),
    "`allocation_factor`"
  )
})

test_that("print, summary and autoplot methods work", {
  x <- calculate_excess_loss(
    data = excess_loss_data,
    claim_amount = "claim_amount",
    exposure = "earned_exposure",
    fit_threshold = 20000,
    excess_threshold = 100000,
    method = "bootstrap",
    allocation_method = "historical_excess",
    allocation_by = "segment",
    allocation_levels = c("A", "B"),
    allocation_weights = "earned_exposure",
    bootstrap_samples = 25,
    bootstrap_seed = 123
  )
  s <- summary(x)

  expect_true(all(c(
    "allocation_group",
    "n",
    "exposure",
    "allocation_weight",
    "allocation_factor",
    "allocation_base",
    "allocated_excess",
    "allocated_share"
  ) %in% names(s)))
  expect_output(print(x), "Excess loss vector")
  expect_s3_class(autoplot(x), "ggplot")
  expect_s3_class(autoplot(x, by = "segment"), "ggplot")
  expect_s3_class(autoplot(x, type = "histogram"), "ggplot")
})
