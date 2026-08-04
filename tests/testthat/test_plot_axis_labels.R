test_that("discrete axis labels use one terminal period", {
  labels <- format_discrete_axis_labels(
    c("Bouwnijverheid", "Bouw"),
    label_width = 6
  )

  expect_identical(labels, c("Bouwn.", "Bouw"))
  expect_false(any(grepl("\\.\\.$", labels)))
})

test_that("axis label abbreviation can be disabled or overridden", {
  expect_identical(
    format_discrete_axis_labels(
      "Bouwnijverheid",
      abbreviate_labels = FALSE,
      label_width = 6
    ),
    "Bouwnijverheid"
  )

  expect_identical(
    format_discrete_axis_labels(
      c("Bouwnijverheid", "Onroerend goed"),
      label_width = 6,
      label_abbreviations = c(
        "Bouwnijverheid" = "Bouw",
        "Onroerend goed" = "Onr. goed"
      )
    ),
    c("Bouw", "Onr. goed")
  )
})

test_that("abbreviated labels remain distinguishable", {
  labels <- format_discrete_axis_labels(
    c("Construction industry", "Construction services"),
    label_width = 8
  )

  expect_length(unique(labels), 2)
  expect_true(all(nchar(labels) <= 8))
  expect_true(all(grepl("\\.$", labels)))
})

test_that("rating table autoplot abbreviates level labels", {
  portfolio <- data.frame(
    sector = factor(rep(c("Bouwnijverheid", "Kantoor"), each = 3)),
    claims = c(0, 1, 0, 1, 0, 1),
    exposure = rep(1, 6)
  )
  model <- glm(
    claims ~ sector + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  table <- rating_table(model, model_data = portfolio, exposure = "exposure")

  plot <- autoplot(
    table,
    risk_factors = "sector",
    label_width = 6,
    show_exposure_labels = FALSE
  )
  panel <- if (inherits(plot, "patchwork")) plot[[1]] else plot
  labels <- panel$scales$get_scales("x")$labels(
    c("Bouwnijverheid", "Kantoor")
  )

  expect_identical(labels, c("Bouwn.", "Kanto."))
})

test_that("factor analysis autoplot abbreviates level labels", {
  portfolio <- data.frame(
    sector = factor(rep(c("Bouwnijverheid", "Kantoor"), each = 3)),
    claims = c(0, 1, 0, 1, 0, 1),
    exposure = rep(1, 6)
  )
  analysis <- factor_analysis(
    portfolio,
    risk_factors = "sector",
    claim_count = "claims",
    exposure = "exposure"
  )

  plot <- autoplot(
    analysis,
    metrics = "frequency",
    label_width = 6,
    show_exposure = FALSE
  )
  panel <- if (inherits(plot, "patchwork")) plot[[1]] else plot
  labels <- panel$scales$get_scales("x")$labels(
    c("Bouwnijverheid", "Kantoor")
  )

  expect_identical(labels, c("Bouwn.", "Kanto."))
})

test_that("autoplot methods support explicit legend positions", {
  portfolio <- data.frame(
    sector = factor(rep(c("Industry", "Retail"), each = 3)),
    period = factor(rep(c("Current", "Previous", "Current"), 2)),
    claims = c(0, 1, 0, 1, 0, 1),
    exposure = rep(1, 6)
  )
  model <- glm(
    claims ~ sector + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  table <- rating_table(model, model_data = portfolio, exposure = "exposure")
  rating_plot <- autoplot(table, legend_position = "bottom")
  rating_panel <- if (inherits(rating_plot, "patchwork")) {
    rating_plot[[1]]
  } else {
    rating_plot
  }

  analysis <- factor_analysis(
    portfolio,
    risk_factors = "sector",
    claim_count = "claims",
    exposure = "exposure",
    group_by = "period"
  )
  analysis_plot <- autoplot(
    analysis,
    metrics = "frequency",
    legend_position = "top"
  )
  analysis_panel <- if (inherits(analysis_plot, "patchwork")) {
    analysis_plot[[1]]
  } else {
    analysis_plot
  }

  expect_identical(rating_panel$theme$legend.position, "bottom")
  expect_identical(analysis_panel$theme$legend.position, "top")
  expect_identical(
    rating_plot$patches$annotation$theme$legend.position,
    "bottom"
  )
  expect_identical(
    analysis_plot$patches$annotation$theme$legend.position,
    "top"
  )
  expect_error(
    autoplot(analysis, metrics = "frequency", legend_position = "inside"),
    "arg"
  )
})

test_that("autoplot.rating_table selects the legend automatically", {
  portfolio <- data.frame(
    sector = factor(rep(c("Industry", "Retail"), each = 3)),
    claims = c(0, 1, 0, 1, 0, 1),
    exposure = rep(1, 6)
  )
  model <- glm(
    claims ~ sector + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  model_alt <- glm(
    claims ~ sector + offset(log(exposure)),
    family = quasipoisson(),
    data = portfolio
  )

  single_plot <- autoplot(
    rating_table(model, model_data = portfolio, exposure = "exposure")
  )
  multiple_plot <- autoplot(
    rating_table(
      model,
      model_alt,
      model_data = portfolio,
      exposure = "exposure"
    )
  )
  explicit_plot <- autoplot(
    rating_table(model, model_data = portfolio, exposure = "exposure"),
    legend_position = "bottom"
  )

  expect_identical(
    single_plot$patches$annotation$theme$legend.position,
    "none"
  )
  expect_identical(
    multiple_plot$patches$annotation$theme$legend.position,
    "right"
  )
  expect_identical(
    explicit_plot$patches$annotation$theme$legend.position,
    "bottom"
  )
})
