context("factor_analysis (SE)")

# --- Basic functionality ------------------------------------------------------

test_that("object is of class univariate and data.frame", {
  x <- factor_analysis(MTPL2, risk_factors = "area", claim_amount = "amount",
                          claim_count = "nclaims", exposure = "exposure",
                          premium = "premium")
  expect_s3_class(x, "factor_analysis")
  expect_s3_class(x, "univariate")
  expect_s3_class(x, "data.frame")
})

test_that("frequency is calculated", {
  x <- factor_analysis(MTPL, risk_factors = "zip", claim_count = "nclaims", exposure = "exposure")
  expect_true("frequency" %in% names(x))
})

test_that("average severity is calculated", {
  x <- factor_analysis(MTPL, risk_factors = "zip", claim_amount = "amount", claim_count = "nclaims")
  expect_true("average_severity" %in% names(x))
})

test_that("risk premium is calculated", {
  x <- factor_analysis(MTPL, risk_factors = "zip", claim_amount = "amount", exposure = "exposure")
  expect_true("risk_premium" %in% names(x))
})

test_that("loss ratio is calculated", {
  x <- factor_analysis(MTPL2, risk_factors = "area", claim_amount = "amount", premium = "premium")
  expect_true("loss_ratio" %in% names(x))
})

test_that("average premium is calculated", {
  x <- factor_analysis(MTPL2, risk_factors = "area", premium = "premium", exposure = "exposure")
  expect_true("average_premium" %in% names(x))
})

# --- Attribute handling -------------------------------------------------------

test_that("correct attribute for `by` if length > 1", {
  x <- factor_analysis(MTPL, risk_factors = "zip", claim_count = "nclaims",
                          exposure = "exposure", group_by = c("bm", "power"))
  expect_equal(as.character(attr(x, "group_by")), c("bm", "power"))
})

test_that("correct attribute for `by` if length = 0", {
  x <- factor_analysis(MTPL, risk_factors = "zip", claim_count = "nclaims", exposure = "exposure")
  expect_equal(attr(x, "group_by"), NULL)
})

test_that("order of `by` variables is preserved", {
  x <- factor_analysis(MTPL, risk_factors = "zip", claim_count = "nclaims",
                          exposure = "exposure", group_by = c("power", "bm"))
  expect_equal(as.character(attr(x, "group_by")), c("power", "bm"))
})

# --- Robustness & validation --------------------------------------------------

test_that("error if both severity and nclaims are missing", {
  expect_error(factor_analysis(MTPL, risk_factors = "zip"),
               regexp = "You did not supply any of the required arguments.")
})

test_that("frequency * exposure equals nclaims", {
  x <- factor_analysis(MTPL, risk_factors = "zip", claim_count = "nclaims", exposure = "exposure")
  expect_true(all.equal(x$frequency * x$exposure, x$nclaims, check.attributes = FALSE))
})

test_that("numeric columns are numeric", {
  x <- factor_analysis(MTPL, risk_factors = "zip", claim_amount = "amount",
                          claim_count = "nclaims", exposure = "exposure")
  num_cols <- c("frequency", "average_severity", "risk_premium")
  expect_true(all(sapply(x[num_cols], is.numeric)))
})

test_that("works with empty data.frame", {
  empty_df <- MTPL[0, ]
  x <- factor_analysis(empty_df, risk_factors = "zip", claim_count = "nclaims", exposure = "exposure")
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 0)
})

test_that("works if x is character instead of factor", {
  mtpl_char <- MTPL
  mtpl_char$zip <- as.character(mtpl_char$zip)
  x <- factor_analysis(mtpl_char, risk_factors = "zip", claim_count = "nclaims", exposure = "exposure")
  expect_true("frequency" %in% names(x))
})

test_that("NA values in exposure handled gracefully", {
  mtpl_na <- MTPL
  mtpl_na$exposure[1:5] <- NA
  expect_silent(
    factor_analysis(mtpl_na, risk_factors = "zip", claim_count = "nclaims", exposure = "exposure")
  )
})

test_that("missing and invalid columns fail clearly", {
  expect_error(
    factor_analysis(MTPL, risk_factors = "missing", claim_count = "nclaims", exposure = "exposure"),
    "Column\\(s\\) not found"
  )
  expect_error(
    factor_analysis(MTPL, risk_factors = "zip", claim_count = "missing", exposure = "exposure"),
    "Column\\(s\\) not found"
  )
  expect_error(
    factor_analysis(MTPL, risk_factors = "zip", claim_count = "nclaims", exposure = "missing"),
    "Column\\(s\\) not found"
  )
  expect_error(
    factor_analysis(MTPL, risk_factors = "zip", claim_count = "nclaims", exposure = "exposure", group_by = "missing"),
    "Column\\(s\\) not found"
  )
})

test_that("metric columns must be numeric", {
  df <- MTPL
  df$nclaims_chr <- as.character(df$nclaims)

  expect_error(
    factor_analysis(df, risk_factors = "zip", claim_count = "nclaims_chr",
                    exposure = "exposure"),
    "Metric column\\(s\\) must be numeric"
  )
})

test_that("deprecated factor_analysis argument names remain available", {
  expect_warning(
    x <- factor_analysis(MTPL, x = "zip", nclaims = "nclaims",
                         exposure = "exposure"),
    "deprecated"
  )

  expect_s3_class(x, "factor_analysis")
  expect_true("frequency" %in% names(x))
})

test_that("division by zero metrics become NA instead of Inf or NaN", {
  df <- data.frame(
    zip = c("a", "b"),
    amount = c(10, 0),
    nclaims = c(0, 0),
    exposure = c(0, 0),
    premium = c(0, 0)
  )

  x <- factor_analysis(
    df,
    risk_factors = "zip",
    claim_amount = "amount",
    claim_count = "nclaims",
    exposure = "exposure",
    premium = "premium"
  )

  metric_cols <- c(
    "frequency",
    "average_severity",
    "risk_premium",
    "loss_ratio",
    "average_premium"
  )
  expect_false(any(is.infinite(as.matrix(x[metric_cols]))))
  expect_false(any(is.nan(as.matrix(x[metric_cols]))))
  expect_true(all(is.na(as.matrix(x[metric_cols]))))
})

test_that("deprecated univariate wrapper remains available", {
  expect_warning(
    x <- univariate(MTPL, x = zip, nclaims = nclaims, exposure = exposure),
    "deprecated"
  )

  expect_s3_class(x, "factor_analysis")
  expect_s3_class(x, "univariate")
})

test_that("autoplot works and show_plots is deprecated", {
  x <- factor_analysis(MTPL, risk_factors = "zip", claim_count = "nclaims", exposure = "exposure")

  expect_s3_class(autoplot(x, metrics = 1, background = FALSE), "ggplot")
  expect_warning(
    p <- autoplot(x, show_plots = 1, background = FALSE),
    "deprecated"
  )
  expect_s3_class(p, "ggplot")
})

test_that("autoplot fails clearly for multiple by variables", {
  x <- factor_analysis(
    MTPL,
    risk_factors = "zip",
    claim_count = "nclaims",
    exposure = "exposure",
    group_by = c("bm", "power")
  )

  expect_error(
    autoplot(x, metrics = 1),
    "at most one `by` variable"
  )
})
