context("univariate_summary (SE)")

# --- Basic functionality ------------------------------------------------------

test_that("object is of class univariate and data.frame", {
  x <- univariate_summary(MTPL2, x = "area", severity = "amount",
                          nclaims = "nclaims", exposure = "exposure",
                          premium = "premium")
  expect_s3_class(x, "univariate")
  expect_s3_class(x, "data.frame")
})

test_that("frequency is calculated", {
  x <- univariate_summary(MTPL, x = "zip", nclaims = "nclaims", exposure = "exposure")
  expect_true("frequency" %in% names(x))
})

test_that("average severity is calculated", {
  x <- univariate_summary(MTPL, x = "zip", severity = "amount", nclaims = "nclaims")
  expect_true("average_severity" %in% names(x))
})

test_that("risk premium is calculated", {
  x <- univariate_summary(MTPL, x = "zip", severity = "amount", exposure = "exposure")
  expect_true("risk_premium" %in% names(x))
})

test_that("loss ratio is calculated", {
  x <- univariate_summary(MTPL2, x = "area", severity = "amount", premium = "premium")
  expect_true("loss_ratio" %in% names(x))
})

test_that("average premium is calculated", {
  x <- univariate_summary(MTPL2, x = "area", premium = "premium", exposure = "exposure")
  expect_true("average_premium" %in% names(x))
})

# --- Attribute handling -------------------------------------------------------

test_that("correct attribute for `by` if length > 1", {
  x <- univariate_summary(MTPL, x = "zip", nclaims = "nclaims",
                          exposure = "exposure", by = c("bm", "power"))
  expect_equal(as.character(attr(x, "by")), c("bm", "power"))
})

test_that("correct attribute for `by` if length = 0", {
  x <- univariate_summary(MTPL, x = "zip", nclaims = "nclaims", exposure = "exposure")
  expect_equal(attr(x, "by"), NULL)
})

test_that("order of `by` variables is preserved", {
  x <- univariate_summary(MTPL, x = "zip", nclaims = "nclaims",
                          exposure = "exposure", by = c("power", "bm"))
  expect_equal(as.character(attr(x, "by")), c("power", "bm"))
})

# --- Robustness & validation --------------------------------------------------

test_that("error if both severity and nclaims are missing", {
  expect_error(univariate_summary(MTPL, x = "zip"),
               regexp = "You did not supply any of the required arguments.")
})

test_that("frequency * exposure equals nclaims", {
  x <- univariate_summary(MTPL, x = "zip", nclaims = "nclaims", exposure = "exposure")
  expect_true(all.equal(x$frequency * x$exposure, x$nclaims, check.attributes = FALSE))
})

test_that("numeric columns are numeric", {
  x <- univariate_summary(MTPL, x = "zip", severity = "amount",
                          nclaims = "nclaims", exposure = "exposure")
  num_cols <- c("frequency", "average_severity", "risk_premium")
  expect_true(all(sapply(x[num_cols], is.numeric)))
})

test_that("works with empty data.frame", {
  empty_df <- MTPL[0, ]
  x <- univariate_summary(empty_df, x = "zip", nclaims = "nclaims", exposure = "exposure")
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 0)
})

test_that("works if x is character instead of factor", {
  mtpl_char <- MTPL
  mtpl_char$zip <- as.character(mtpl_char$zip)
  x <- univariate_summary(mtpl_char, x = "zip", nclaims = "nclaims", exposure = "exposure")
  expect_true("frequency" %in% names(x))
})

test_that("NA values in exposure handled gracefully", {
  mtpl_na <- MTPL
  mtpl_na$exposure[1:5] <- NA
  expect_silent(
    univariate_summary(mtpl_na, x = "zip", nclaims = "nclaims", exposure = "exposure")
  )
})

