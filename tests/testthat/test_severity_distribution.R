context("severity_distribution")

severity_test_data <- data.frame(
  segment = rep(LETTERS[1:5], times = c(30, 25, 12, 22, 35)),
  amount = c(
    seq(100, 3000, length.out = 30),
    seq(200, 8000, length.out = 25),
    seq(50, 500, length.out = 12),
    c(seq(150, 2500, length.out = 21), 50000),
    seq(80, 1200, length.out = 35)
  )
)

severity_plot <- function(..., show_labels = FALSE) {
  plot_severity_distribution(
    severity_test_data,
    claim_amount = "amount",
    risk_factor = "segment",
    show_labels = show_labels,
    ...
  )
}

test_that("plot_severity_distribution filters by min_claims and top_n", {
  x <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "none",
    mean = FALSE,
    median = FALSE
  )

  expect_s3_class(x, "ggplot")
  expect_equal(length(unique(attr(x, "severity_distribution_data")$.category)), 3)
  expect_false("C" %in% unique(attr(x, "severity_distribution_data")$.category))
})

test_that("plot_severity_distribution sorts by selected metric", {
  by_mean <- severity_plot(
    min_claims = 20,
    top_n = 2,
    sort = "mean",
    point_method = "none",
    mean = FALSE,
    median = FALSE
  )
  by_claims <- severity_plot(
    min_claims = 20,
    top_n = 2,
    sort = "n_claims",
    point_method = "none",
    mean = FALSE,
    median = FALSE
  )

  expect_equal(attr(by_mean, "severity_distribution_settings")$category_summary$.category[1], "B")
  expect_equal(attr(by_claims, "severity_distribution_settings")$category_summary$.category[1], "E")
})

test_that("plot_severity_distribution returns a ggplot object", {
  x <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "none",
    mean = FALSE,
    median = FALSE,
    boxplot = FALSE
  )

  expect_s3_class(x, "ggplot")
  expect_s3_class(x + ggplot2::labs(caption = "test"), "ggplot")
})

test_that("plot_severity_distribution supports log scales", {
  x <- severity_plot(
    min_claims = 20,
    top_n = 3,
    log_scale = TRUE,
    point_method = "none",
    mean = FALSE,
    median = FALSE
  )
  p <- x

  scale_classes <- vapply(p$scales$scales, function(s) class(s)[1],
                          character(1))
  expect_true(any(grepl("ScaleContinuousPosition", scale_classes)))
})

test_that("plot_severity_distribution supports an overall distribution", {
  x <- plot_severity_distribution(
    severity_test_data,
    claim_amount = "amount",
    risk_factor = NULL,
    all_claims_label = "All losses",
    min_claims = 20,
    point_method = "none",
    show_labels = FALSE
  )

  expect_equal(unique(attr(x, "severity_distribution_data")$.category), "All losses")
  expect_equal(attr(x, "severity_distribution_settings")$risk_factor, NULL)
  expect_equal(attr(x, "severity_distribution_settings")$all_claims_label, "All losses")
  expect_equal(nrow(attr(x, "severity_distribution_settings")$category_summary), 1)
})

test_that("plot_severity_distribution supports axis labels", {
  horizontal <- severity_plot(
    min_claims = 20,
    top_n = 3,
    x_label = "Claim size",
    y_label = "Risk factor",
    point_method = "none"
  )
  vertical <- severity_plot(
    min_claims = 20,
    top_n = 3,
    orientation = "vertical",
    x_label = "Risk factor",
    y_label = "Claim size",
    point_method = "none"
  )

  expect_equal(horizontal$labels$x, "Claim size")
  expect_equal(horizontal$labels$y, "Risk factor")
  expect_equal(vertical$labels$x, "Risk factor")
  expect_equal(vertical$labels$y, "Claim size")
})

test_that("plot_severity_distribution falls back when ggbeeswarm is unavailable", {
  skip_if(requireNamespace("ggbeeswarm", quietly = TRUE))

  expect_warning(
    x <- severity_plot(
      min_claims = 20,
      top_n = 3,
      point_method = "quasirandom"
    ),
    "Falling back to jittered points"
  )
  expect_s3_class(x, "ggplot")
})

test_that("plot_severity_distribution supports point_method none", {
  x <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "none",
    mean = FALSE,
    median = FALSE,
    boxplot = FALSE
  )
  geoms <- vapply(x$layers, function(layer) class(layer$geom)[1],
                  character(1))

  expect_false("GeomPoint" %in% geoms)
})

test_that("plot_severity_distribution uses no violin layer by default", {
  default_plot <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "none",
    mean = FALSE,
    median = FALSE,
    boxplot = FALSE
  )
  half_violin <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "none",
    mean = FALSE,
    median = FALSE,
    boxplot = FALSE,
    distribution = "half_violin"
  )
  violin <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "none",
    mean = FALSE,
    median = FALSE,
    boxplot = FALSE,
    distribution = "violin"
  )

  default_geoms <- vapply(default_plot$layers, function(layer) class(layer$geom)[1],
                          character(1))
  half_geoms <- vapply(half_violin$layers, function(layer) class(layer$geom)[1],
                       character(1))
  violin_geoms <- vapply(violin$layers, function(layer) class(layer$geom)[1],
                         character(1))

  expect_false("GeomPolygon" %in% default_geoms)
  expect_true("GeomPolygon" %in% half_geoms)
  expect_true("GeomPolygon" %in% violin_geoms)
})

test_that("plot_severity_distribution supports horizontal and vertical orientation", {
  horizontal <- severity_plot(
    min_claims = 20,
    top_n = 3,
    orientation = "horizontal",
    point_method = "jitter"
  )
  vertical <- severity_plot(
    min_claims = 20,
    top_n = 3,
    orientation = "vertical",
    point_method = "jitter"
  )

  expect_s3_class(horizontal, "ggplot")
  expect_s3_class(vertical, "ggplot")
})

test_that("plot_severity_distribution supports mean and median markers on and off", {
  with_mean <- severity_plot(
    min_claims = 20,
    top_n = 3,
    mean = TRUE,
    median = FALSE,
    point_method = "none"
  )
  with_median <- severity_plot(
    min_claims = 20,
    top_n = 3,
    mean = FALSE,
    median = TRUE,
    point_method = "none"
  )
  without_summary <- severity_plot(
    min_claims = 20,
    top_n = 3,
    mean = FALSE,
    median = FALSE,
    point_method = "none"
  )

  expect_gt(length(with_mean$layers), length(without_summary$layers))
  expect_gt(length(with_median$layers), length(without_summary$layers))
})

test_that("plot_severity_distribution supports subtle boxplot width", {
  x <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "jitter",
    boxplot = TRUE,
    boxplot_width = 0.03
  )
  geoms <- vapply(x$layers, function(layer) class(layer$geom)[1],
                  character(1))

  expect_true("GeomBoxplot" %in% geoms)
  expect_equal(attr(x, "severity_distribution_settings")$boxplot_width, 0.03)
})

test_that("plot_severity_distribution has no legend", {
  x <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "jitter",
    mean = TRUE,
    median = TRUE
  )

  expect_equal(x$theme$legend.position, "none")
})

test_that("plot_severity_distribution highlights threshold claims", {
  x <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "jitter",
    threshold = 10000
  )
  geoms <- vapply(x$layers, function(layer) class(layer$geom)[1],
                  character(1))

  expect_true(any(attr(x, "severity_distribution_data")$.above_threshold))
  expect_true("GeomVline" %in% geoms)
  expect_equal(attr(x, "severity_distribution_settings")$threshold, 10000)
})

test_that("plot_severity_distribution supports direct labels", {
  skip_if_not(requireNamespace("ggrepel", quietly = TRUE))

  x <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "none",
    mean_label = "Average",
    median_label = "Middle",
    threshold = 10000,
    threshold_label = "Large loss",
    show_labels = TRUE
  )
  geoms <- vapply(x$layers, function(layer) class(layer$geom)[1],
                  character(1))

  expect_true("GeomTextRepel" %in% geoms)
  expect_equal(attr(x, "severity_distribution_settings")$mean_label, "Average")
  expect_equal(attr(x, "severity_distribution_settings")$median_label, "Middle")
  expect_equal(attr(x, "severity_distribution_settings")$threshold_label, "Large loss")
})

test_that("plot_severity_distribution requires ggrepel for labels", {
  skip_if(requireNamespace("ggrepel", quietly = TRUE))

  expect_error(
    severity_plot(min_claims = 20, top_n = 3, show_labels = TRUE),
    "ggrepel"
  )
})

test_that("plot_severity_distribution removes missing and non-positive amounts", {
  bad_data <- rbind(
    severity_test_data,
    data.frame(segment = c("A", "A"), amount = c(NA_real_, 0))
  )

  expect_warning(
    x <- plot_severity_distribution(
      bad_data,
      claim_amount = "amount",
      risk_factor = "segment",
      min_claims = 20,
      top_n = 3,
      point_method = "none",
      show_labels = FALSE
    ),
    "Removed 2 claim observation"
  )
  expect_true(all(attr(x, "severity_distribution_data")$.claim_amount > 0))
})

test_that("plot_severity_distribution validates inputs", {
  expect_error(
    plot_severity_distribution(list(segment = "a", amount = 1), "amount", "segment"),
    "`data`"
  )
  expect_error(
    plot_severity_distribution(severity_test_data, missing, "segment"),
    "`claim_amount`"
  )
  expect_error(
    plot_severity_distribution(severity_test_data, "missing", "segment"),
    "Column"
  )
  expect_error(
    plot_severity_distribution(
      transform(severity_test_data, amount = as.character(amount)),
      "amount",
      "segment"
    ),
    "`claim_amount`"
  )
  expect_error(
    severity_plot(top_n = 0),
    "`top_n`"
  )
  expect_error(
    severity_plot(min_claims = 0),
    "`min_claims`"
  )
  expect_error(
    severity_plot(show_labels = NA),
    "`show_labels`"
  )
  expect_error(
    severity_plot(threshold = NA),
    "`threshold`"
  )
  expect_error(
    severity_plot(threshold = 0),
    "`threshold`"
  )
  expect_error(
    severity_plot(median = NA),
    "`median`"
  )
  expect_error(
    severity_plot(boxplot_width = 2),
    "`boxplot_width`"
  )
  expect_error(
    severity_plot(x_label = NA),
    "`x_label`"
  )
  expect_error(
    severity_plot(all_claims_label = NA),
    "`all_claims_label`"
  )
})
