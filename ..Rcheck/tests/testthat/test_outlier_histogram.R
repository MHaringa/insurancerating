context("outlier_histogram")

test_that("outlier_histogram returns a ggplot", {
  p <- outlier_histogram(MTPL2, "premium", bins = 20)

  expect_s3_class(p, "ggplot")
})

test_that("outlier_histogram supports lower and upper tail bins", {
  p <- outlier_histogram(
    MTPL2,
    "premium",
    lower = 30,
    upper = 120,
    bins = 20
  )

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 4)
})

test_that("outlier_histogram supports density line", {
  p <- outlier_histogram(MTPL2, "premium", density = TRUE, bins = 20)

  expect_s3_class(p, "ggplot")
  layer_geoms <- vapply(p$layers, function(layer) class(layer$geom)[1],
                        character(1))
  expect_true("GeomLine" %in% layer_geoms)
})

test_that("outlier_histogram uses package plot colors by default", {
  p <- outlier_histogram(
    MTPL2,
    "premium",
    lower = 30,
    upper = 120,
    density = TRUE,
    bins = 20
  )

  built <- ggplot2::ggplot_build(p)
  fills <- unlist(lapply(built$data, `[[`, "fill"), use.names = FALSE)
  colours <- unlist(lapply(built$data, `[[`, "colour"), use.names = FALSE)

  expect_true("#E6E6E6" %in% fills)
  expect_true("#F28E2B" %in% fills)
  expect_true("#2C7FB8" %in% colours)
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
    outlier_histogram(MTPL2, "premium", density = NA),
    "`density`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", lower = "30"),
    "`lower`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", upper = "120"),
    "`upper`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", lower = 120, upper = 30),
    "`upper`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", bar_fill = 1),
    "`bar_fill`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", bar_color = 1),
    "`bar_color`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", tail_fill = 1),
    "`tail_fill`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", tail_color = 1),
    "`tail_color`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", density_color = 1),
    "`density_color`"
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
    outlier_histogram(MTPL2, "premium", lower = min(MTPL2$premium, na.rm = TRUE)),
    "`lower`"
  )
  expect_error(
    outlier_histogram(MTPL2, "premium", upper = max(MTPL2$premium, na.rm = TRUE)),
    "`upper`"
  )
})

test_that("deprecated outlier_histogram arguments remain available", {
  expect_warning(
    p <- outlier_histogram(
      MTPL2,
      "premium",
      left = 30,
      right = 120,
      line = TRUE,
      fill = "#DDDDDD",
      color = "white",
      fill_outliers = "#F28E2B",
      bins = 20
    ),
    "deprecated"
  )

  expect_s3_class(p, "ggplot")
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
