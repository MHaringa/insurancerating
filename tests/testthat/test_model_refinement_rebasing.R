rebasing_portfolio <- function() {
  data.frame(
    claims = c(1, 2, 1, 3, 2, 4, 1, 5, 2, 3, 1, 4),
    exposure = c(1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1),
    sector = factor(rep(c("Industry", "Office", "Retail", "Transport"), 3))
  )
}

rebasing_model <- function() {
  portfolio <- rebasing_portfolio()
  glm(
    claims ~ sector + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
}

testthat::test_that("explicit rebasing preserves all relative level ratios", {
  portfolio <- rebasing_portfolio()
  model <- rebasing_model()
  specification <- prepare_refinement(model, portfolio) |>
    add_rebasing("sector", reference_level = "Office")
  step <- specification$steps[[1]]
  values <- step$values
  reference <- values$original_relativity[values$level == "Office"]

  testthat::expect_identical(step$method, "explicit")
  testthat::expect_identical(step$reference_level, "Office")
  testthat::expect_equal(
    values$rebased_relativity,
    values$original_relativity / reference
  )
  testthat::expect_equal(
    values$rebased_relativity[values$level == "Office"],
    1
  )
  testthat::expect_equal(
    outer(values$rebased_relativity, values$rebased_relativity, "/"),
    outer(values$original_relativity, values$original_relativity, "/")
  )
})

testthat::test_that("automatic rebasing selects the largest weighted level", {
  portfolio <- rebasing_portfolio()
  model <- rebasing_model()
  specification <- prepare_refinement(model, portfolio) |>
    add_rebasing("sector", weights = "exposure")
  step <- specification$steps[[1]]
  expected <- names(which.max(tapply(
    portfolio$exposure,
    portfolio$sector,
    sum
  )))

  testthat::expect_identical(step$method, "largest_weight")
  testthat::expect_identical(step$reference_level, expected)
  testthat::expect_equal(
    step$values$rebased_relativity[
      step$values$level == step$reference_level
    ],
    1
  )
})

testthat::test_that("NULL weights use the same model basis as shrinkage", {
  portfolio <- rebasing_portfolio()
  model <- rebasing_model()
  specification <- prepare_refinement(model, portfolio) |>
    add_rebasing("sector")

  testthat::expect_identical(
    specification$steps[[1]]$weight_spec$column,
    "exposure"
  )
  testthat::expect_true(specification$steps[[1]]$weight_spec$inferred)
})

testthat::test_that("rebasing follows shrinkage and is used by refit", {
  portfolio <- rebasing_portfolio()
  model <- rebasing_model()
  specification <- prepare_refinement(model, portfolio) |>
    add_shrinkage("sector", credibility = 0.8, weights = "exposure") |>
    add_rebasing("sector", reference_level = "Office")
  shrinkage_values <- specification$steps[[1]]$values
  rebasing_values <- specification$steps[[2]]$values
  fitted <- refit(specification)
  table <- rating_table(fitted, exposure = FALSE)
  estimate <- grep("^est_", names(table), value = TRUE)
  rows <- table$risk_factor == "sector"

  testthat::expect_equal(
    rebasing_values$original_relativity,
    shrinkage_values$adjusted_relativity
  )
  testthat::expect_match(
    paste(deparse(stats::formula(fitted)), collapse = " "),
    "sector_rebased"
  )
  testthat::expect_equal(
    table[[estimate]][rows],
    rebasing_values$rebased_relativity[
      match(as.character(table$level[rows]), rebasing_values$level)
    ]
  )
})

testthat::test_that("rebasing can use a factor created by add_relativities", {
  portfolio <- data.frame(
    claims = c(1, 2, 2, 3, 3, 4, 4, 5),
    exposure = c(1, 2, 1, 2, 1, 3, 1, 3),
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
    add_rebasing("broad_tariff", reference_level = "A1")
  fitted <- refit(specification)
  table <- rating_table(fitted, exposure = FALSE)

  testthat::expect_identical(
    specification$steps[[2]]$effective_model_term,
    "broad_rel"
  )
  testthat::expect_equal(
    specification$steps[[2]]$values$rebased_relativity[
      specification$steps[[2]]$values$level == "A1"
    ],
    1
  )
  testthat::expect_setequal(
    table$level[table$risk_factor == "broad_tariff"],
    c("A1", "A2", "B1", "B2")
  )
})

testthat::test_that("summary and autoplot describe rebasing", {
  portfolio <- rebasing_portfolio()
  model <- rebasing_model()
  specification <- prepare_refinement(model, portfolio) |>
    add_rebasing("sector", reference_level = "Office")
  result <- summary(specification)
  plot <- autoplot(specification)

  testthat::expect_identical(result$steps$type, "rebasing")
  testthat::expect_match(result$steps$description, "Rebasing: sector")
  testthat::expect_match(result$steps$details, "reference = Office")
  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_setequal(
    unique(plot$data$type),
    c("Current relativity", "Rebased relativity")
  )
})

testthat::test_that("rebasing validates references, weights and fitted models", {
  portfolio <- rebasing_portfolio()
  model <- rebasing_model()
  specification <- prepare_refinement(model, portfolio)

  testthat::expect_error(
    add_rebasing(specification, "sector", reference_level = "Offic"),
    "Did you mean `Office`"
  )
  testthat::expect_error(
    add_rebasing(specification, "sector", weights = "missing_weight"),
    "not found"
  )
  testthat::expect_error(
    add_rebasing(specification, "sector", weights = "equal"),
    "cannot identify a largest reference level"
  )
  testthat::expect_error(
    add_rebasing(model, "sector"),
    "cannot be added to or edited on a fitted GLM"
  )
})
