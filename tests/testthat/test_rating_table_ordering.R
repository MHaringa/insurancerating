rating_ordering_data <- function() {
  data.frame(
    claims = c(1, 2, 4, 1, 3, 8, 2, 5, 10, 1, 4, 7),
    exposure = rep(1, 12),
    sector = factor(
      rep(c("Retail", "Industry", "Office"), each = 4),
      levels = c("Retail", "Industry", "Office")
    ),
    region = factor(
      rep(c("West", "East"), 6),
      levels = c("West", "East")
    )
  )
}


rating_ordering_model <- function() {
  portfolio <- rating_ordering_data()
  glm(
    claims ~ sector + region + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
}


numeric_level_ordering_data <- function() {
  data.frame(
    claims = c(1, 2, 3, 4, 2, 3, 4, 5),
    exposure = rep(1, 8),
    insured_amount_band = factor(
      rep(c("[0,100]", "(100,200]", "(200,1000]", "(1000,2000]"), 2),
      levels = c("[0,100]", "(1000,2000]", "(100,200]", "(200,1000]")
    )
  )
}


testthat::test_that("rating_table orders nominal factors by descending estimate", {
  model <- rating_ordering_model()
  table <- rating_table(model, exposure = FALSE)

  estimate <- grep("^est_", names(table), value = TRUE)
  for (risk_factor in c("sector", "region")) {
    values <- table[[estimate]][table$risk_factor == risk_factor]
    testthat::expect_true(all(diff(values) <= 0))
  }
  testthat::expect_identical(
    attr(table, "reference_levels"),
    c(sector = "Retail", region = "West")
  )
})


testthat::test_that("rating_table supports explicit level ordering", {
  model <- rating_ordering_model()

  alphabetical <- rating_table(
    model,
    exposure = FALSE,
    level_order = "alphabetical"
  )
  alphabetical_without_reference <- rating_table(
    model,
    exposure = FALSE,
    reference_first = FALSE,
    level_order = "alphabetical"
  )
  descending <- rating_table(
    model,
    exposure = FALSE,
    level_order = "estimate_descending"
  )

  testthat::expect_identical(
    alphabetical$level[alphabetical$risk_factor == "sector"],
    c("Retail", "Industry", "Office")
  )
  testthat::expect_identical(
    alphabetical_without_reference$level[
      alphabetical_without_reference$risk_factor == "sector"
    ],
    c("Industry", "Office", "Retail")
  )

  sector_rows <- descending$risk_factor == "sector"
  estimate <- grep("^est_", names(descending), value = TRUE)
  testthat::expect_true(all(diff(descending[[estimate]][sector_rows]) <= 0))
})


testthat::test_that("numeric intervals are ordered by their boundaries", {
  portfolio <- numeric_level_ordering_data()
  model <- glm(
    claims ~ insured_amount_band + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )

  table <- rating_table(
    model,
    exposure = FALSE,
    reference_first = FALSE,
    level_order = "alphabetical"
  )

  testthat::expect_identical(
    table$level[table$risk_factor == "insured_amount_band"],
    c("[0,100]", "(100,200]", "(200,1000]", "(1000,2000]")
  )
  testthat::expect_identical(attr(table, "numeric_level_order"), "ascending")
})


testthat::test_that("numeric character levels are ordered numerically", {
  portfolio <- data.frame(
    claims = c(1, 2, 3, 2, 3, 4),
    exposure = rep(1, 6),
    size_band = rep(c("1", "10", "2"), 2)
  )
  model <- glm(
    claims ~ size_band + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )

  table <- rating_table(
    model,
    exposure = FALSE,
    reference_first = FALSE,
    level_order = "alphabetical"
  )

  testthat::expect_identical(
    table$level[table$risk_factor == "size_band"],
    c("1", "2", "10")
  )
})


testthat::test_that("as_specified leaves numeric levels to level_order", {
  portfolio <- numeric_level_ordering_data()
  model <- glm(
    claims ~ insured_amount_band + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )

  table <- rating_table(
    model,
    exposure = FALSE,
    reference_first = FALSE,
    level_order = "alphabetical",
    numeric_level_order = "as_specified"
  )
  displayed <- table$level[table$risk_factor == "insured_amount_band"]

  testthat::expect_identical(displayed, sort(displayed))
})


testthat::test_that("numeric ordering takes precedence over reference_first", {
  portfolio <- numeric_level_ordering_data()
  portfolio$insured_amount_band <- stats::relevel(
    portfolio$insured_amount_band,
    ref = "(1000,2000]"
  )
  model <- glm(
    claims ~ insured_amount_band + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )

  table <- rating_table(model, exposure = FALSE)

  testthat::expect_identical(
    table$level[table$risk_factor == "insured_amount_band"],
    c("[0,100]", "(100,200]", "(200,1000]", "(1000,2000]")
  )
})


testthat::test_that("ordered factors retain their declared level sequence", {
  portfolio <- data.frame(
    claims = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6),
    exposure = rep(1, 10),
    urbanisation = ordered(
      rep(c("Low", "Moderate", "Average", "Strong", "Very strong"), 2),
      levels = c("Low", "Moderate", "Average", "Strong", "Very strong")
    )
  )
  model <- glm(
    claims ~ urbanisation + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )

  table <- rating_table(model, exposure = FALSE)

  testthat::expect_identical(
    table$level[table$risk_factor == "urbanisation"],
    c("Low", "Moderate", "Average", "Strong", "Very strong")
  )
})


testthat::test_that("level order can be overridden by risk factor", {
  model <- rating_ordering_model()
  table <- rating_table(
    model,
    exposure = FALSE,
    level_order_by_risk_factor = c(
      sector = "alphabetical",
      region = "model"
    )
  )

  testthat::expect_identical(
    table$level[table$risk_factor == "sector"],
    c("Industry", "Office", "Retail")
  )
  testthat::expect_identical(
    table$level[table$risk_factor == "region"],
    c("West", "East")
  )
  testthat::expect_identical(
    attr(table, "level_order_by_risk_factor"),
    c(sector = "alphabetical", region = "model")
  )

  testthat::expect_error(
    rating_table(
      model,
      exposure = FALSE,
      level_order_by_risk_factor = c(unknown = "model")
    ),
    "were not found"
  )
  testthat::expect_error(
    rating_table(
      model,
      exposure = FALSE,
      level_order_by_risk_factor = c(sector = "descending")
    ),
    "Unknown ordering"
  )
})


testthat::test_that("mixed categorical labels retain categorical ordering", {
  portfolio <- data.frame(
    claims = c(1, 2, 3, 2, 3, 4),
    exposure = rep(1, 6),
    industry = factor(
      rep(c("Industry 1", "Industry 10", "Industry 2"), 2),
      levels = c("Industry 1", "Industry 10", "Industry 2")
    )
  )
  model <- glm(
    claims ~ industry + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )

  table <- rating_table(
    model,
    exposure = FALSE,
    reference_first = FALSE,
    level_order = "alphabetical"
  )

  testthat::expect_identical(
    table$level[table$risk_factor == "industry"],
    c("Industry 1", "Industry 10", "Industry 2")
  )
})


testthat::test_that("rating_table supports risk-factor ordering", {
  model <- rating_ordering_model()
  model_order <- rating_table(model, exposure = FALSE)
  alphabetical <- rating_table(
    model,
    exposure = FALSE,
    risk_factor_order = "alphabetical"
  )

  testthat::expect_identical(
    unique(model_order$risk_factor),
    c("(Intercept)", "sector", "region")
  )
  testthat::expect_identical(
    unique(alphabetical$risk_factor),
    c("(Intercept)", "region", "sector")
  )
})


testthat::test_that("order_model selects the model used for estimate sorting", {
  portfolio <- rating_ordering_data()
  first <- glm(
    claims ~ sector + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  reversed_claims <- rev(portfolio$claims)
  second <- glm(
    reversed_claims ~ sector + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )

  first_order <- rating_table(
    first,
    second,
    exposure = FALSE,
    reference_first = FALSE,
    level_order = "estimate_descending",
    order_model = "first"
  )
  second_order <- rating_table(
    first,
    second,
    exposure = FALSE,
    reference_first = FALSE,
    level_order = "estimate_descending",
    order_model = "est_second"
  )

  testthat::expect_false(identical(
    first_order$level[first_order$risk_factor == "sector"],
    second_order$level[second_order$risk_factor == "sector"]
  ))
  testthat::expect_identical(attr(second_order, "order_model"), "second")
  testthat::expect_error(
    rating_table(first, second, exposure = FALSE, order_model = "unknown"),
    "Choose one of: first, second"
  )
})


testthat::test_that("ordering falls back for factors absent from order_model", {
  portfolio <- rating_ordering_data()
  sector_only <- glm(
    claims ~ sector + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  with_region <- glm(
    claims ~ sector + region + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  table <- rating_table(
    sector_only,
    with_region,
    exposure = FALSE,
    level_order = "alphabetical",
    order_model = "sector_only"
  )

  testthat::expect_identical(
    table$level[table$risk_factor == "region"],
    c("West", "East")
  )
})


testthat::test_that("rating_table uses a rebased reference level", {
  portfolio <- rating_ordering_data()
  model <- rating_ordering_model()
  fitted <- prepare_refinement(model, portfolio) |>
    add_rebasing("sector", reference_level = "Office") |>
    refit()
  table <- rating_table(
    fitted,
    exposure = FALSE,
    level_order = "alphabetical"
  )

  testthat::expect_identical(
    table$level[table$risk_factor == "sector"],
    c("Office", "Industry", "Retail")
  )
  testthat::expect_identical(
    unname(attr(table, "reference_levels")[["sector"]]),
    "Office"
  )
})


testthat::test_that("autoplot retains the rating-table level order", {
  model <- rating_ordering_model()
  table <- rating_table(
    model,
    exposure = FALSE,
    reference_first = FALSE,
    level_order = "alphabetical"
  )
  plot <- ggplot2::autoplot(table, risk_factors = "sector")
  panel <- plot[[1L]]

  testthat::expect_identical(
    levels(panel$data$level),
    c("Industry", "Office", "Retail")
  )
})


testthat::test_that("as_gt retains the rating-table row order", {
  testthat::skip_if_not_installed("gt")
  model <- rating_ordering_model()
  table <- rating_table(
    model,
    exposure = FALSE,
    reference_first = FALSE,
    level_order = "alphabetical",
    risk_factor_order = "alphabetical"
  )
  gt_table <- as_gt(table)

  testthat::expect_identical(
    as.character(gt_table[["_data"]]$level),
    as.character(table$level)
  )
  testthat::expect_identical(
    as.character(gt_table[["_data"]]$risk_factor),
    as.character(table$risk_factor)
  )
})
