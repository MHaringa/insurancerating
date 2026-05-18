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

  expect_s3_class(x, "severity_distribution_plot")
  expect_s3_class(x, "insurancerating")
  expect_equal(length(unique(x$data$.category)), 3)
  expect_false("C" %in% unique(x$data$.category))
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

  expect_equal(by_mean$settings$category_summary$.category[1], "B")
  expect_equal(by_claims$settings$category_summary$.category[1], "E")
})

test_that("plot_severity_distribution methods work", {
  x <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "none",
    mean = FALSE,
    median = FALSE,
    boxplot = FALSE
  )

  expect_s3_class(x$plot, "ggplot")
  expect_s3_class(summary(x), "summary.severity_distribution_plot")
  expect_output(print(summary(x)), "Categories shown")
  expect_silent(print(x))
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
  p <- x$plot

  scale_classes <- vapply(p$scales$scales, function(s) class(s)[1],
                          character(1))
  expect_true(any(grepl("ScaleContinuousPosition", scale_classes)))
})

test_that("plot_severity_distribution supports an overall distribution", {
  x <- plot_severity_distribution(
    severity_test_data,
    claim_amount = "amount",
    risk_factor = NULL,
    min_claims = 20,
    point_method = "none",
    show_labels = FALSE
  )

  expect_equal(unique(x$data$.category), "All claims")
  expect_equal(x$settings$risk_factor, NULL)
  expect_equal(nrow(x$settings$category_summary), 1)
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
  expect_s3_class(x$plot, "ggplot")
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
  geoms <- vapply(x$plot$layers, function(layer) class(layer$geom)[1],
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

  default_geoms <- vapply(default_plot$plot$layers, function(layer) class(layer$geom)[1],
                          character(1))
  half_geoms <- vapply(half_violin$plot$layers, function(layer) class(layer$geom)[1],
                       character(1))
  violin_geoms <- vapply(violin$plot$layers, function(layer) class(layer$geom)[1],
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

  expect_s3_class(horizontal$plot, "ggplot")
  expect_s3_class(vertical$plot, "ggplot")
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

  expect_gt(length(with_mean$plot$layers), length(without_summary$plot$layers))
  expect_gt(length(with_median$plot$layers), length(without_summary$plot$layers))
})

test_that("plot_severity_distribution supports subtle boxplot width", {
  x <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "jitter",
    boxplot = TRUE,
    boxplot_width = 0.03
  )
  geoms <- vapply(x$plot$layers, function(layer) class(layer$geom)[1],
                  character(1))

  expect_true("GeomBoxplot" %in% geoms)
  expect_equal(x$settings$boxplot_width, 0.03)
})

test_that("plot_severity_distribution has no legend", {
  x <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "jitter",
    mean = TRUE,
    median = TRUE
  )

  expect_equal(x$plot$theme$legend.position, "none")
})

test_that("plot_severity_distribution highlights threshold claims", {
  x <- severity_plot(
    min_claims = 20,
    top_n = 3,
    point_method = "jitter",
    threshold = 10000
  )
  geoms <- vapply(x$plot$layers, function(layer) class(layer$geom)[1],
                  character(1))

  expect_true(any(x$data$.above_threshold))
  expect_true("GeomVline" %in% geoms)
  expect_equal(x$settings$threshold, 10000)
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
  geoms <- vapply(x$plot$layers, function(layer) class(layer$geom)[1],
                  character(1))

  expect_true("GeomTextRepel" %in% geoms)
  expect_equal(x$settings$mean_label, "Average")
  expect_equal(x$settings$median_label, "Middle")
  expect_equal(x$settings$threshold_label, "Large loss")
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
  expect_true(all(x$data$.claim_amount > 0))
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
})
