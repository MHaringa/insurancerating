context("excess_loss")

portfolio_data <- data.frame(
  policy_id = 1:10,
  sector = rep(c("Industry", "Retail"), each = 5),
  claim_count = c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1),
  claim_amount = c(
    0, 25000, 120000, 50000, 175000,
    0, 40000, 90000, 150000, 300000
  ),
  policy_years = rep(1, 10)
)

test_that("assess_excess_threshold calculates portfolio diagnostics", {
  x <- assess_excess_threshold(
    portfolio_data,
    claim_amount = "claim_amount",
    thresholds = c(50000, 100000),
    exposure = "policy_years",
    group = "sector",
    claim_count = "claim_count"
  )

  expect_s3_class(x, "threshold_assessment")
  expect_equal(names(x), c(
    "sector", "threshold", "policy_years", "n_claims",
    "n_excess_records", "total_loss", "capped_loss", "excess_loss",
    "pure_premium_before", "pure_premium_after", "premium_reduction",
    "premium_reduction_ratio"
  ))
  expect_equal(x$sector, rep(c("Industry", "Retail"), each = 2))
  expect_equal(x$threshold, rep(c(50000, 100000), times = 2))
  expect_equal(x$n_claims, rep(c(4, 4), each = 2))
  expect_equal(x$premium_reduction,
               x$pure_premium_before - x$pure_premium_after)
  expect_equal(
    x$premium_reduction_ratio,
    x$premium_reduction / x$pure_premium_before
  )
})

test_that("assess_excess_threshold supports inferred counts and record exposure", {
  x <- assess_excess_threshold(
    portfolio_data[c("sector", "claim_amount")],
    claim_amount = "claim_amount",
    thresholds = c(100000, 25000),
    group = "sector"
  )

  expect_equal(names(x)[1:3], c("sector", "threshold", "exposure"))
  expect_equal(x$exposure, rep(5, 4))
  expect_equal(x$n_claims, rep(c(4, 4), each = 2))
  expect_equal(x$threshold, rep(c(100000, 25000), times = 2))
})

test_that("assess_excess_threshold handles zero loss and validates claim counts", {
  zero <- data.frame(
    claim_amount = c(0, 0),
    claim_count = c(0, 0),
    bad_claim_count = c(0, NA),
    policy_years = c(1, 1)
  )
  x <- assess_excess_threshold(
    zero,
    claim_amount = "claim_amount",
    thresholds = 100000,
    exposure = "policy_years",
    claim_count = "claim_count"
  )

  expect_equal(x$premium_reduction_ratio, 0)
  expect_error(
    assess_excess_threshold(
      zero,
      claim_amount = "claim_amount",
      thresholds = 100000,
      claim_count = "bad_claim_count"
    ),
    "`claim_count`"
  )
})

test_that("as_gt presents threshold assessments", {
  skip_if_not_installed("gt")
  x <- assess_excess_threshold(
    portfolio_data,
    claim_amount = "claim_amount",
    thresholds = c(50000, 100000),
    exposure = "policy_years",
    group = "sector",
    claim_count = "claim_count"
  )

  tbl <- as_gt(x)
  expect_s3_class(tbl, "gt_tbl")
  expect_equal(tbl[["_locale"]]$locale, "nl-NL")
  expect_true(all(c("sector", "policy_years", "n_claims") %in%
                    names(tbl[["_data"]])))
  expect_false("total_loss" %in% names(tbl[["_data"]]))
  html <- as.character(gt::as_raw_html(tbl))
  expect_match(html, "Risk premium")
  expect_match(html, "#F7E94D")
  expect_no_match(html, "Excess threshold assessment")

  expect_s3_class(as_gt(x, loss = TRUE, locale = "en-US"), "gt_tbl")
  expect_true("total_loss" %in% names(as_gt(x, loss = TRUE)[["_data"]]))
  expect_no_match(
    as.character(gt::as_raw_html(as_gt(x, color_last_column = FALSE))),
    "#F7E94D"
  )
})

test_that("redistribute_excess_loss redistributes excess per claim", {
  portfolio <- data.frame(
    policy_id = 1:4,
    claim_count = c(0, 1, 2, 1),
    claim_amount = c(0, 120000, 60000, 220000)
  )
  x <- redistribute_excess_loss(
    portfolio,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count"
  )

  expect_s3_class(x, "data.frame")
  expect_s3_class(x, "excess_redistribution")
  expect_equal(names(x)[seq_along(names(portfolio))], names(portfolio))
  expect_equal(x$claim_amount_capped, c(0, 100000, 60000, 100000))
  expect_equal(x$claim_amount_excess, c(0, 20000, 0, 120000))
  expect_equal(x$claim_amount_is_excess, c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(x$claim_amount_redistributed_excess, c(0, 35000, 70000, 35000))
  expect_equal(x$claim_amount_adjusted, c(0, 135000, 130000, 135000))
  expect_equal(x$claim_amount_adjusted_average, c(0, 135000, 65000, 135000))
  expect_equal(sum(x$claim_amount_adjusted), sum(x$claim_amount))
  expect_equal(x$claim_amount_redistributed_excess[x$claim_count == 0], 0)
  expect_identical(attr(x, "output"), "redistributed_claim")
})

test_that("default output is backward compatible with redistributed claims", {
  default <- redistribute_excess_loss(
    portfolio_data,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    risk_factor = "sector",
    redistribution_method = "partial"
  )
  explicit <- redistribute_excess_loss(
    portfolio_data,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    risk_factor = "sector",
    redistribution_method = "partial",
    output = "redistributed_claim"
  )

  expect_equal(default, explicit)
  expect_equal(
    explicit$claim_amount_adjusted,
    explicit$claim_amount_capped +
      explicit$claim_amount_redistributed_excess
  )
})

test_that("excess loading is returned per unit of redistribution weight", {
  x <- redistribute_excess_loss(
    portfolio_data,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    redistribution_weight = "policy_years",
    risk_factor = "sector",
    redistribution_method = "partial",
    output = "excess_loading"
  )
  total_excess <- sum(pmax(portfolio_data$claim_amount - 100000, 0))

  expect_identical(attr(x, "output"), "excess_loading")
  expect_identical(attr(x, "redistribution_weight_label"), "policy_years")
  expect_true(all(c("allocated_excess_loss", "excess_loading") %in% names(x)))
  expect_false(any(c(
    "claim_amount_adjusted", "claim_amount_adjusted_average"
  ) %in% names(x)))
  expect_equal(
    x$allocated_excess_loss,
    x$excess_loading * x$policy_years
  )
  expect_equal(sum(x$allocated_excess_loss), total_excess)
  expect_equal(sum(x$excess_loading * x$policy_years), total_excess)
  expect_true(all(x$allocated_excess_loss[x$claim_count == 0] > 0))
})

test_that("claim-count loading is expressed per claim", {
  x <- redistribute_excess_loss(
    portfolio_data,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    output = "excess_loading"
  )

  expect_equal(
    sum(x$excess_loading * x$claim_count),
    sum(pmax(portfolio_data$claim_amount - 100000, 0))
  )
  expect_equal(x$excess_loading[x$claim_count == 0], c(0, 0))
})

test_that("risk-factor loadings reconcile within risk-factor levels", {
  x <- redistribute_excess_loss(
    portfolio_data,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    redistribution_weight = "policy_years",
    risk_factor = "sector",
    redistribution_method = "risk_factor",
    output = "excess_loading"
  )
  allocated_by_sector <- tapply(
    x$allocated_excess_loss, x$sector, sum
  )
  observed_by_sector <- tapply(
    pmax(x$claim_amount - 100000, 0), x$sector, sum
  )

  expect_equal(allocated_by_sector, observed_by_sector)
  expect_true(all(vapply(
    split(x$excess_loading, x$sector),
    function(z) length(unique(z)) == 1L,
    logical(1)
  )))
})

test_that("zero redistribution weights receive zero loading", {
  portfolio <- transform(
    portfolio_data,
    loading_exposure = c(0, rep(1, 9))
  )
  x <- redistribute_excess_loss(
    portfolio,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    redistribution_weight = "loading_exposure",
    output = "excess_loading"
  )

  expect_equal(x$allocated_excess_loss[1], 0)
  expect_equal(x$excess_loading[1], 0)
  expect_equal(
    sum(x$allocated_excess_loss),
    sum(pmax(portfolio$claim_amount - 100000, 0))
  )
})

test_that("both outputs preserve excess under every redistribution method", {
  methods <- c("portfolio", "risk_factor", "partial")
  outputs <- c("redistributed_claim", "excess_loading")
  total_excess <- sum(pmax(portfolio_data$claim_amount - 100000, 0))

  for (method in methods) {
    for (output in outputs) {
      args <- list(
        data = portfolio_data,
        claim_amount = "claim_amount",
        threshold = 100000,
        claim_count = "claim_count",
        redistribution_weight = if (
          identical(output, "excess_loading")
        ) "policy_years" else NULL,
        risk_factor = if (identical(method, "portfolio")) NULL else "sector",
        redistribution_method = method,
        output = output
      )
      x <- do.call(redistribute_excess_loss, args)
      allocated <- attr(x, "allocated_excess_loss_vector")
      expect_equal(sum(allocated), total_excess, info = paste(method, output))
    }
  }
})

test_that("sparse groups illustrate the two actuarial interpretations", {
  portfolio <- data.frame(
    sector = c("Sparse", "Established"),
    claim_count = c(1, 1),
    claim_amount = c(10000, 150000),
    earned_exposure = c(1, 1)
  )
  redistributed <- redistribute_excess_loss(
    portfolio,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    redistribution_method = "portfolio",
    output = "redistributed_claim"
  )
  loading <- redistribute_excess_loss(
    portfolio,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    redistribution_weight = "earned_exposure",
    redistribution_method = "portfolio",
    output = "excess_loading"
  )

  expect_equal(redistributed$claim_amount_adjusted[1], 35000)
  expect_equal(loading$claim_amount_capped[1], 10000)
  expect_equal(loading$excess_loading[1], 25000)
})

test_that("redistribute_excess_loss infers one claim per positive row", {
  portfolio <- data.frame(
    incurred_loss = c(0, 120000, 30000, 0)
  )
  x <- redistribute_excess_loss(
    portfolio,
    claim_amount = "incurred_loss",
    threshold = 100000
  )

  expect_true(all(c(
    "incurred_loss_capped", "incurred_loss_excess",
    "incurred_loss_is_excess", "incurred_loss_redistributed_excess",
    "incurred_loss_adjusted", "incurred_loss_adjusted_average"
  ) %in% names(x)))
  expect_equal(x$incurred_loss_redistributed_excess, c(0, 10000, 10000, 0))
  expect_equal(sum(x$incurred_loss_adjusted), sum(x$incurred_loss))
})

test_that("risk-factor redistribution uses only risk-factor experience", {
  x <- redistribute_excess_loss(
    portfolio_data,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    risk_factor = "sector",
    redistribution_method = "risk_factor"
  )

  industry <- x$sector == "Industry"
  retail <- x$sector == "Retail"
  expect_equal(
    sum(x$claim_amount_redistributed_excess[industry]),
    sum(x$claim_amount_excess[industry])
  )
  expect_equal(
    sum(x$claim_amount_redistributed_excess[retail]),
    sum(x$claim_amount_excess[retail])
  )
  expect_equal(sum(x$claim_amount_adjusted), sum(x$claim_amount))
})

test_that("partial redistribution blends group and portfolio experience", {
  x <- redistribute_excess_loss(
    portfolio_data,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    risk_factor = "sector",
    redistribution_method = "partial",
    credibility = 0.4
  )
  s <- attr(x, "redistribution_summary")

  expect_equal(s$sector_credibility, rep(0.4, nrow(s)))
  expect_equal(
    s$blended_excess_loading,
    0.4 * s$sector_excess_loading + 0.6 * s$portfolio_excess_loading,
    tolerance = 1e-8,
    scale = 1
  )
  expect_equal(
    x$blended_excess_loading,
    x$sector_credibility * x$sector_excess_loading +
      (1 - x$sector_credibility) * x$portfolio_excess_loading
  )
  expect_equal(
    x$final_redistribution_loading,
    ifelse(
      x$claim_count > 0,
      x$blended_excess_loading * x$redistribution_scaling_factor,
      0
    )
  )
  expect_equal(
    x$claim_amount_redistributed_excess,
    x$final_redistribution_loading * x$claim_count
  )
  expect_equal(sum(x$claim_amount_adjusted), sum(x$claim_amount))
})

test_that("calculation details can be omitted without losing the audit", {
  compact <- redistribute_excess_loss(
    portfolio_data,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    risk_factor = "sector",
    redistribution_method = "partial",
    credibility = 0.4,
    calculation_details = FALSE
  )
  detail_columns <- c(
    "sector_excess_loading", "sector_credibility",
    "portfolio_excess_loading", "blended_excess_loading",
    "redistribution_scaling_factor", "final_redistribution_loading"
  )

  expect_false(any(detail_columns %in% names(compact)))
  expect_true(all(detail_columns %in% names(summary(compact))))
  expect_equal(
    compact$claim_amount_redistributed_excess,
    attr(compact, "final_redistribution_loading_vector") *
      compact$claim_count
  )
})

test_that("automatic credibility and scaling remain transparent", {
  x <- redistribute_excess_loss(
    portfolio_data,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    risk_factor = "sector",
    redistribution_method = "partial",
    credibility_basis = "claims",
    credibility_threshold = 4,
    credibility_scale = 0.5
  )
  s <- attr(x, "redistribution_summary")

  expect_equal(s$sector_credibility, (s$claim_count / (s$claim_count + 4)) * 0.5)
})

test_that("weights and eligibility control who receives redistribution", {
  portfolio <- data.frame(
    claim_count = c(1, 1, 1, 0),
    claim_amount = c(200000, 50000, 150000, 0),
    insured_amount = c(50000, 100000, 200000, 500000)
  )
  portfolio$receives <- portfolio$insured_amount >= 100000
  portfolio$redistribution_weight <-
    portfolio$claim_count * portfolio$insured_amount

  x <- redistribute_excess_loss(
    portfolio,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    redistribution_weight = "redistribution_weight",
    receives_redistribution = "receives"
  )

  expect_equal(x$claim_amount_redistributed_excess, c(0, 50000, 100000, 0))
  expect_equal(x$claim_amount_redistributed_excess[1], 0)
  expect_equal(x$claim_amount_excess[1], 100000)
  expect_equal(sum(x$claim_amount_redistributed_excess),
               sum(x$claim_amount_excess))
  expect_equal(sum(x$claim_amount_adjusted), sum(x$claim_amount))
})

test_that("redistribute_excess selects which large losses enter the pool", {
  portfolio <- data.frame(
    claim_count = c(1, 1, 1),
    claim_amount = c(200000, 150000, 50000),
    redistribute = c(FALSE, TRUE, TRUE)
  )
  x <- redistribute_excess_loss(
    portfolio,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    redistribute_excess = "redistribute"
  )

  expect_equal(x$claim_amount_excess, c(100000, 50000, 0))
  expect_equal(x$claim_amount_capped, c(200000, 100000, 50000))
  expect_equal(sum(x$claim_amount_redistributed_excess), 50000)
  expect_equal(
    x$claim_amount_redistributed_excess,
    rep(50000 / 3, 3)
  )
  expect_equal(sum(x$claim_amount_adjusted), sum(x$claim_amount))
})

test_that("summary audits contributed, received and shifted loss", {
  x <- redistribute_excess_loss(
    portfolio_data,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    risk_factor = "sector",
    redistribution_method = "partial",
    credibility = 0.4
  )
  s <- summary(x)

  expect_equal(names(s), c(
    "sector", "n_records", "claim_count", "redistribution_weight",
    "n_excess_records", "n_redistributed_excess_records", "observed_loss",
    "retained_loss", "observed_excess_loss",
    "redistributed_excess_contributed",
    "sector_excess_loading", "sector_credibility",
    "portfolio_excess_loading", "blended_excess_loading",
    "redistribution_scaling_factor", "final_redistribution_loading",
    "allocated_excess_loss", "average_excess_loading",
    "redistributed_excess_received", "net_loss_shift", "adjusted_loss",
    "observed_average_claim", "adjusted_average_claim"
  ))
  expect_equal(sum(s$net_loss_shift), 0, tolerance = 1e-8)
  expect_equal(s$adjusted_loss, s$observed_loss + s$net_loss_shift)
  expect_equal(s$observed_average_claim,
               s$observed_loss / s$claim_count)
  expect_equal(s$adjusted_average_claim,
               s$adjusted_loss / s$claim_count)
  expect_equal(
    s$blended_excess_loading,
    s$sector_credibility * s$sector_excess_loading +
      (1 - s$sector_credibility) * s$portfolio_excess_loading
  )
  expect_equal(
    s$final_redistribution_loading,
    s$blended_excess_loading * s$redistribution_scaling_factor
  )
  expect_equal(
    s$redistributed_excess_received,
    s$final_redistribution_loading * s$redistribution_weight
  )
  expect_output(print(x), "claim_amount_adjusted")
})

test_that("summary supports portfolio audits by another column", {
  x <- redistribute_excess_loss(
    portfolio_data,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    redistribution_method = "portfolio"
  )
  portfolio_summary <- summary(x)
  sector_summary <- summary(x, by = "sector")

  expect_false("sector" %in% names(portfolio_summary))
  expect_equal(nrow(portfolio_summary), 1)
  expect_true("sector" %in% names(sector_summary))
  expect_equal(sum(sector_summary$net_loss_shift), 0, tolerance = 1e-8)
  expect_error(summary(x, by = "missing"), "Column")
})

test_that("risk-factor redistribution reconciles within each group", {
  x <- redistribute_excess_loss(
    portfolio_data,
    claim_amount = "claim_amount",
    threshold = 100000,
    claim_count = "claim_count",
    risk_factor = "sector",
    redistribution_method = "risk_factor"
  )
  s <- summary(x)

  expect_equal(s$net_loss_shift, rep(0, nrow(s)), tolerance = 1e-8)
  expect_equal(
    s$redistributed_excess_received,
    s$redistributed_excess_contributed,
    tolerance = 1e-8
  )
})

test_that("redistribute_excess_loss validates inputs and output collisions", {
  expect_identical(
    names(formals(redistribute_excess_loss)),
    c(
      "data", "claim_amount", "threshold", "claim_count",
      "redistribution_weight", "receives_redistribution",
      "redistribute_excess", "risk_factor", "redistribution_method",
      "credibility", "credibility_basis",
      "credibility_threshold", "credibility_scale", "calculation_details",
      "output"
    )
  )
  expect_error(
    redistribute_excess_loss(portfolio_data, "claim_amount", 0),
    "`threshold`"
  )
  expect_error(
    redistribute_excess_loss(
      transform(portfolio_data, claim_count = c(-1, rep(1, 9))),
      "claim_amount", 100000, claim_count = "claim_count"
    ),
    "`claim_count`"
  )
  expect_error(
    redistribute_excess_loss(
      portfolio_data, "claim_amount", 100000,
      redistribution_method = "partial"
    ),
    "`risk_factor`"
  )
  collision <- portfolio_data
  collision$claim_amount_adjusted <- 0
  expect_error(
    redistribute_excess_loss(collision, "claim_amount", 100000),
    "already exist"
  )
  expect_error(
    redistribute_excess_loss(
      transform(portfolio_data, bad_weight = NA_real_),
      "claim_amount", 100000,
      redistribution_weight = "bad_weight"
    ),
    "`redistribution_weight`"
  )
  expect_error(
    redistribute_excess_loss(
      transform(portfolio_data, bad_receives = NA),
      "claim_amount", 100000,
      receives_redistribution = "bad_receives"
    ),
    "`receives_redistribution`"
  )
  expect_error(
    redistribute_excess_loss(
      transform(portfolio_data, bad_source = NA),
      "claim_amount", 100000,
      redistribute_excess = "bad_source"
    ),
    "`redistribute_excess`"
  )
  expect_error(
    redistribute_excess_loss(
      portfolio_data, "claim_amount", 100000,
      calculation_details = NA
    ),
    "`calculation_details`"
  )
  expect_error(
    redistribute_excess_loss(
      portfolio_data, "claim_amount", 100000, output = "invalid"
    ),
    "arg"
  )
  expect_error(
    redistribute_excess_loss(
      transform(portfolio_data, zero_weight = 0),
      "claim_amount", 100000,
      redistribution_weight = "zero_weight",
      output = "excess_loading"
    ),
    "positive redistribution weight"
  )
})

test_that("the former multi-step excess API is no longer public", {
  ns <- asNamespace("insurancerating")
  expect_false(exists("calculate_excess_loss", envir = ns, inherits = FALSE))
  expect_false(exists("allocate_excess_loss", envir = ns, inherits = FALSE))
  expect_false(exists("apply_excess_loading", envir = ns, inherits = FALSE))
  expect_null(getS3method("summary", "excess_allocation", optional = TRUE))
  expect_null(getS3method("autoplot", "excess_allocation", optional = TRUE))
})
