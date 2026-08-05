shrinkage_portfolio <- function() {
  data.frame(
    claims = c(1, 2, 1, 3, 2, 4, 1, 5, 2, 3, 1, 4),
    exposure = c(1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1),
    sector = factor(rep(c("Industry", "Office", "Retail", "Transport"), 3))
  )
}

shrinkage_model <- function() {
  portfolio <- shrinkage_portfolio()
  glm(
    claims ~ sector + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
}

testthat::test_that("shrinkage uses the documented log-scale calculation", {
  portfolio <- shrinkage_portfolio()
  model <- shrinkage_model()
  specification <- prepare_refinement(model, portfolio) |>
    add_shrinkage(
      model_variable = "sector",
      credibility = 0.8,
      weights = "exposure"
    )

  step <- specification$steps[[1]]
  values <- step$values
  centre <- exp(stats::weighted.mean(
    log(values$original_relativity),
    values$weight
  ))
  unscaled <- exp(
    0.8 * log(values$original_relativity) + 0.2 * log(centre)
  )
  expected <- unscaled *
    (stats::weighted.mean(values$original_relativity, values$weight) /
       stats::weighted.mean(unscaled, values$weight))

  testthat::expect_equal(values$adjusted_relativity, expected)
  testthat::expect_equal(
    stats::weighted.mean(values$adjusted_relativity, values$weight),
    stats::weighted.mean(values$original_relativity, values$weight)
  )
  testthat::expect_lt(
    diff(range(log(values$adjusted_relativity))),
    diff(range(log(values$original_relativity)))
  )
})

testthat::test_that("NULL weights use the GLM offset or explicit model weights", {
  portfolio <- shrinkage_portfolio()
  frequency <- shrinkage_model()
  frequency_spec <- prepare_refinement(frequency, portfolio) |>
    add_shrinkage("sector")

  testthat::expect_identical(
    frequency_spec$steps[[1]]$weight_spec$column,
    "exposure"
  )
  testthat::expect_true(frequency_spec$steps[[1]]$weight_spec$inferred)

  severity_data <- transform(
    portfolio,
    amount = claims * c(100, 150, 200),
    claim_count = claims
  )
  severity <- glm(
    amount ~ sector,
    weights = claim_count,
    family = Gamma(link = "log"),
    data = severity_data
  )
  severity_spec <- prepare_refinement(severity, severity_data) |>
    add_shrinkage("sector")

  testthat::expect_identical(
    severity_spec$steps[[1]]$weight_spec$type,
    "model_weights"
  )
  testthat::expect_identical(
    severity_spec$steps[[1]]$weight_spec$label,
    "claim_count"
  )
})

testthat::test_that("equal weighting is explicit and preserves the level mean", {
  portfolio <- shrinkage_portfolio()
  model <- shrinkage_model()
  specification <- prepare_refinement(model, portfolio) |>
    add_shrinkage("sector", credibility = 0.5, weights = "equal")
  values <- specification$steps[[1]]$values

  testthat::expect_equal(values$weight, rep(1, nrow(values)))
  testthat::expect_equal(
    mean(values$adjusted_relativity),
    mean(values$original_relativity)
  )
})

testthat::test_that("credibility boundary values have clear interpretations", {
  portfolio <- shrinkage_portfolio()
  model <- shrinkage_model()
  unchanged <- prepare_refinement(model, portfolio) |>
    add_shrinkage("sector", credibility = 1, weights = "equal")
  common <- prepare_refinement(model, portfolio) |>
    add_shrinkage("sector", credibility = 0, weights = "equal")

  testthat::expect_equal(
    unchanged$steps[[1]]$values$adjusted_relativity,
    unchanged$steps[[1]]$values$original_relativity
  )
  testthat::expect_length(
    unique(round(common$steps[[1]]$values$adjusted_relativity, 12)),
    1L
  )
})

testthat::test_that("refit and rating_table use shrunk relativities", {
  portfolio <- shrinkage_portfolio()
  model <- shrinkage_model()
  specification <- prepare_refinement(model, portfolio) |>
    add_shrinkage("sector", credibility = 0.75)
  values <- specification$steps[[1]]$values
  fitted <- refit(specification)
  table <- rating_table(fitted, exposure = FALSE)
  estimate <- grep("^est_", names(table), value = TRUE)
  sector_rows <- table$risk_factor == "sector"

  testthat::expect_s3_class(fitted, "refitrestricted")
  testthat::expect_match(
    paste(deparse(stats::formula(fitted)), collapse = " "),
    "sector_shrunk"
  )
  testthat::expect_equal(
    table[[estimate]][sector_rows],
    values$adjusted_relativity[
      match(as.character(table$level[sector_rows]), values$level)
    ]
  )
})

testthat::test_that("shrinkage follows and precedes other refinement steps", {
  portfolio <- shrinkage_portfolio()
  model <- shrinkage_model()
  restrictions <- data.frame(
    sector = "Transport",
    sector_restricted = 2.5
  )

  after_restriction <- prepare_refinement(model, portfolio) |>
    add_restriction(restrictions) |>
    add_shrinkage("sector", credibility = 0.8)
  testthat::expect_equal(
    after_restriction$steps[[2]]$values$original_relativity[
      after_restriction$steps[[2]]$values$level == "Transport"
    ],
    2.5
  )

  after_shrinkage <- prepare_refinement(model, portfolio) |>
    add_shrinkage("sector", credibility = 0.8) |>
    add_restriction(restrictions)
  fitted <- refit(after_shrinkage)
  testthat::expect_s3_class(fitted, "refitrestricted")
  testthat::expect_match(
    paste(deparse(stats::formula(fitted)), collapse = " "),
    "sector_restricted"
  )
})

testthat::test_that("shrinkage can use a factor created by add_relativities", {
  portfolio <- data.frame(
    claims = c(1, 2, 2, 3, 3, 4, 4, 5),
    exposure = 1,
    broad = factor(rep(c("A", "B"), each = 4)),
    detail = factor(c("A1", "A2", "A1", "A2", "B1", "B2", "B1", "B2"))
  )
  model <- glm(
    claims ~ broad + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  split <- relativities(
    split_level("A", c("A1", "A2"), c(0.8, 1.2)),
    split_level("B", c("B1", "B2"), c(0.9, 1.1))
  )
  specification <- prepare_refinement(model, portfolio) |>
    add_relativities(
      model_variable = "broad",
      split_variable = "detail",
      output_variable = "broad_tariff",
      relativities = split,
      exposure = "exposure"
    ) |>
    add_shrinkage("broad_tariff", credibility = 0.8)
  fitted <- refit(specification)
  table <- rating_table(fitted, exposure = FALSE)

  testthat::expect_setequal(
    table$level[table$risk_factor == "broad_tariff"],
    c("A1", "A2", "B1", "B2")
  )
  testthat::expect_identical(
    specification$steps[[2]]$effective_model_term,
    "broad_rel"
  )
})

testthat::test_that("summary records the shrinkage assumptions", {
  portfolio <- shrinkage_portfolio()
  model <- shrinkage_model()
  specification <- prepare_refinement(model, portfolio) |>
    add_shrinkage("sector", credibility = 0.85)
  result <- summary(specification)

  testthat::expect_identical(result$steps$type, "shrinkage")
  testthat::expect_match(result$steps$description, "Shrinkage: sector")
  testthat::expect_match(result$steps$details, "credibility = 0.85")
  testthat::expect_match(result$steps$details, "weighted mean preserved")
})

testthat::test_that("autoplot previews current and shrunk relativities", {
  portfolio <- shrinkage_portfolio()
  model <- shrinkage_model()
  specification <- prepare_refinement(model, portfolio) |>
    add_shrinkage("sector", credibility = 0.85)
  plot <- autoplot(specification)

  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_setequal(
    unique(plot$data$type),
    c("Current relativity", "Shrunk relativity")
  )
})

testthat::test_that("shrinkage validates its inputs and weighting basis", {
  portfolio <- shrinkage_portfolio()
  model <- shrinkage_model()
  gaussian <- glm(claims ~ sector, family = gaussian(), data = portfolio)

  testthat::expect_error(
    prepare_refinement(model, portfolio) |>
      add_shrinkage("sector", credibility = 1.1),
    "between 0 and 1"
  )
  testthat::expect_error(
    prepare_refinement(model, portfolio) |>
      add_shrinkage("sector", weights = "missing_weight"),
    "not found"
  )
  testthat::expect_error(
    prepare_refinement(gaussian, portfolio) |>
      add_shrinkage("sector"),
    "No unambiguous shrinkage weight"
  )

  portfolio$invalid_weight <- portfolio$exposure
  portfolio$invalid_weight[1] <- -1
  invalid_model <- glm(
    claims ~ sector + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  testthat::expect_error(
    prepare_refinement(invalid_model, portfolio) |>
      add_shrinkage("sector", weights = "invalid_weight"),
    "non-negative"
  )
})

testthat::test_that("shrinkage cannot be added directly to fitted models", {
  model <- shrinkage_model()
  specification <- prepare_refinement(model) |>
    add_shrinkage("sector")
  fitted <- refit(specification)

  testthat::expect_error(
    add_shrinkage(model, "sector"),
    "cannot be added to or edited on a fitted GLM"
  )
  testthat::expect_error(
    add_shrinkage(fitted, "sector"),
    "cannot be added to or edited on a refitted GLM"
  )
})
