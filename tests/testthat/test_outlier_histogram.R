context("outlier_histogram")

test_that("outlier_histogram returns a ggplot", {
  p <- outlier_histogram(MTPL2, "premium", bins = 20)

  expect_s3_class(p, "ggplot")
})

test_that("outlier_histogram supports left and right outlier bins", {
  p <- outlier_histogram(
    MTPL2,
    "premium",
    left = 30,
    right = 120,
    bins = 20
  )

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 4)
})

test_that("outlier_histogram supports density line", {
  p <- outlier_histogram(MTPL2, "premium", line = TRUE, bins = 20)

  expect_s3_class(p, "ggplot")
  layer_geoms <- vapply(p$layers, function(layer) class(layer$geom)[1],
                        character(1))
  expect_true("GeomLine" %in% layer_geoms)
})

test_that("outlier_histogram validates inputs", {
  expect_error(
    outlier_histogram(list(premium = 1:3), "premium"),
    "`data`"
  )
  expect_error(
    outlier_histogram(MTPL2, premium),
    "`x`"
  )
  expect_error(
    outlier_histogram(MTPL2, "missing"),
    "not found"
  )
  expect_error(
    outlier_histogram(transform(MTPL2, premium_chr = as.character(premium)), "premium_chr"),
    "must be numeric"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", bins = 0),
    "`bins`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", bins = 1.5),
    "`bins`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", line = NA),
    "`line`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", left = "30"),
    "`left`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", right = "120"),
    "`right`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", left = 120, right = 30),
    "`right`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", fill = 1),
    "`fill`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", color = 1),
    "`color`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", fill_outliers = 1),
    "`fill_outliers`"
  )
})

test_that("outlier_histogram rejects invalid numeric ranges", {
  expect_error(
    outlier_histogram(data.frame(x = rep(1, 10)), "x"),
    "distinct finite values"
  )
  expect_error(
    outlier_histogram(data.frame(x = c(NA_real_, NA_real_)), "x"),
    "finite value"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", left = min(MTPL2$premium, na.rm = TRUE)),
    "`left`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", right = max(MTPL2$premium, na.rm = TRUE)),
    "`right`"
  )
})

test_that("histbin remains available for NSE and character input", {
  expect_warning(
    p_nse <- histbin(MTPL2, premium, bins = 20),
    "deprecated"
  )
  p_chr <- suppressWarnings(
    histbin(MTPL2, "premium", bins = 20)
  )
  col <- "premium"
  p_var <- suppressWarnings(
    histbin(MTPL2, col, bins = 20)
  )

  expect_s3_class(p_nse, "ggplot")
  expect_s3_class(p_chr, "ggplot")
  expect_s3_class(p_var, "ggplot")
})
