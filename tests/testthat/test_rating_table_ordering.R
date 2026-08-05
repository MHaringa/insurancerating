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


testthat::test_that("rating_table places fitted reference levels first", {
  model <- rating_ordering_model()
  table <- rating_table(model, exposure = FALSE)

  testthat::expect_identical(
    table$level[table$risk_factor == "sector"],
    c("Retail", "Industry", "Office")
  )
  testthat::expect_identical(
    table$level[table$risk_factor == "region"],
    c("West", "East")
  )
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
  non_reference <- descending[[estimate]][sector_rows][-1L]
  testthat::expect_identical(
    descending$level[sector_rows][1L],
    "Retail"
  )
  testthat::expect_true(all(diff(non_reference) <= 0))
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
