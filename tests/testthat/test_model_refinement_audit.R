audit_portfolio <- function() {
  data.frame(
    claims = c(1, 2, 1, 3, 2, 4, 2, 5),
    exposure = c(1, 1, 0.5, 1, 1, 0.5, 1, 1),
    sector = factor(rep(c("Industry", "Retail"), each = 4)),
    size = factor(rep(c("Small", "Large"), 4))
  )
}

audit_models <- function() {
  portfolio <- audit_portfolio()
  model <- glm(
    claims ~ sector + size + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  specification <- prepare_refinement(model, portfolio) |>
    add_restriction(data.frame(
      sector = "Retail",
      sector_restricted = 1.20
    ))
  list(
    portfolio = portfolio,
    model = model,
    specification = specification,
    refined = refit(specification)
  )
}

testthat::test_that("summary describes a prepared refinement specification", {
  objects <- audit_models()
  result <- summary(objects$specification)

  testthat::expect_s3_class(result, "summary.rating_refinement")
  testthat::expect_identical(result$n_steps, 1L)
  testthat::expect_identical(result$steps$type, "restriction")
  testthat::expect_match(result$steps$details, "Industry =")
  testthat::expect_match(result$steps$details, "Retail = 1.2")
  testthat::expect_identical(result$package, "insurancerating")
  testthat::expect_output(print(result), "Refinement specification")
})

testthat::test_that("refit stores reproducible audit metadata", {
  objects <- audit_models()
  fitted <- objects$refined

  testthat::expect_s3_class(
    attr(fitted, "refinement_base_model", exact = TRUE),
    "glm"
  )
  testthat::expect_identical(
    attr(fitted, "refinement_package", exact = TRUE),
    "insurancerating"
  )
  testthat::expect_true(inherits(
    attr(fitted, "refinement_created_at", exact = TRUE),
    "POSIXct"
  ))
  testthat::expect_true(inherits(
    attr(fitted, "refinement_refitted_at", exact = TRUE),
    "POSIXct"
  ))
})

testthat::test_that("audit compares models on the same observed portfolio", {
  objects <- audit_models()
  result <- audit_refinement(objects$refined, metric = "frequency")

  testthat::expect_s3_class(result, "refinement_audit")
  testthat::expect_identical(result$scale, "per_exposure")
  testthat::expect_identical(result$exposure, "exposure")
  testthat::expect_identical(result$metric, "frequency")

  expected_before <- sum(stats::predict(
    objects$model,
    newdata = objects$portfolio,
    type = "response"
  )) / sum(objects$portfolio$exposure)
  expected_after <- sum(stats::predict(
    objects$refined,
    newdata = objects$refined$data,
    type = "response"
  )) / sum(objects$portfolio$exposure)

  testthat::expect_equal(result$portfolio$before, expected_before)
  testthat::expect_equal(result$portfolio$after, expected_after)
  testthat::expect_equal(
    result$portfolio$change,
    expected_after - expected_before
  )
  testthat::expect_equal(
    result$portfolio$change_ratio,
    (expected_after - expected_before) / expected_before
  )
})

testthat::test_that("audit reports final factor levels and multiple factors", {
  objects <- audit_models()
  result <- audit_refinement(objects$refined, exposure = "exposure")
  impact <- as.data.frame(result)

  testthat::expect_setequal(
    unique(impact$risk_factor),
    c("sector_restricted", "size")
  )
  testthat::expect_setequal(
    impact$level[impact$risk_factor == "sector_restricted"],
    c("Industry", "Retail")
  )
  testthat::expect_false(any(
    impact$level[impact$risk_factor == "sector_restricted"] %in%
      c("1", "1.2")
  ))
  testthat::expect_equal(
    sum(impact$exposure[impact$risk_factor == "sector_restricted"]),
    sum(objects$portfolio$exposure)
  )
  testthat::expect_equal(
    sum(impact$records[impact$risk_factor == "sector_restricted"]),
    nrow(objects$portfolio)
  )
})

testthat::test_that("summary selects the largest level changes", {
  objects <- audit_models()
  audit <- audit_refinement(objects$refined)
  result <- summary(audit, top_n = 2)

  testthat::expect_s3_class(result, "summary.refinement_audit")
  testthat::expect_identical(nrow(result$impact), 2L)
  testthat::expect_identical(result$total_levels, nrow(audit$impact))
  testthat::expect_output(print(result), "Refinement audit")
  testthat::expect_error(summary(audit, top_n = -1), "top_n")
})

testthat::test_that("response-scale audit works without exposure offset", {
  portfolio <- audit_portfolio()
  model <- glm(claims ~ sector, family = gaussian(), data = portfolio)
  specification <- prepare_refinement(model, portfolio) |>
    add_restriction(data.frame(
      sector = "Retail",
      sector_restricted = 1.10
    ))
  fitted <- refit(specification)
  result <- audit_refinement(fitted, scale = "auto")

  testthat::expect_identical(result$scale, "response")
  testthat::expect_null(result$exposure)
  testthat::expect_identical(result$metric, "fitted_response")
})

testthat::test_that("audit builds a common grid for smoothed models", {
  portfolio <- data.frame(
    claims = c(1, 2, 2, 3, 4, 5),
    exposure = 1,
    age = c(20, 30, 40, 50, 60, 70)
  )
  portfolio$age_band <- cut(portfolio$age, c(0, 35, 55, 100))
  model <- glm(
    claims ~ age_band + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  specification <- prepare_refinement(model, portfolio) |>
    add_smoothing(
      model_variable = "age_band",
      source_variable = "age",
      breaks = c(0, 35, 55, 100),
      weights = "exposure",
      smoothing = "poly",
      degree = 1
    )
  fitted <- refit(specification)
  result <- audit_refinement(fitted)

  expected_levels <- levels(cut(
    portfolio$age,
    c(0, 35, 55, 100),
    include.lowest = TRUE
  ))
  testthat::expect_setequal(result$impact$level, expected_levels)
  testthat::expect_equal(sum(result$impact$records), nrow(portfolio))
  testthat::expect_true(all(c("age_band", "age_band_smooth") %in%
    names(result$model_points)))
})

testthat::test_that("audit uses the final hybrid relativity factor", {
  portfolio <- data.frame(
    claims = c(1, 2, 3, 4, 2, 3, 4, 5),
    exposure = 1,
    sector = factor(rep(c("A", "B"), each = 4)),
    sector_detail = factor(c(
      "A1", "A2", "A1", "A2", "B1", "B2", "B1", "B2"
    ))
  )
  model <- glm(
    claims ~ sector + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  specification <- prepare_refinement(model, portfolio) |>
    add_relativities(
      model_variable = "sector",
      split_variable = "sector_detail",
      relativities = relativities(
        split_level("A", c("A1", "A2"), c(0.9, 1.1))
      ),
      exposure = "exposure",
      output_variable = "sector_tariff_segment"
    )
  fitted <- refit(specification)
  result <- audit_refinement(fitted)

  testthat::expect_identical(
    unique(result$impact$risk_factor),
    "sector_tariff_segment"
  )
  testthat::expect_setequal(result$impact$level, c("A1", "A2", "B"))
})

testthat::test_that("audit validates its model and requested columns", {
  objects <- audit_models()

  testthat::expect_error(
    audit_refinement(objects$model),
    "returned by `refit\\(\\)`"
  )
  testthat::expect_error(
    audit_refinement(objects$refined, exposure = "missing"),
    "Exposure column `missing`"
  )
  testthat::expect_error(
    audit_refinement(objects$refined, risk_factors = "missing"),
    "not available in the rating grid"
  )
})

testthat::test_that("as_gt presents refinement audit results", {
  testthat::skip_if_not_installed("gt")
  objects <- audit_models()
  result <- audit_refinement(objects$refined)

  table <- as_gt(result, locale = "en-US", title = NULL, subtitle = NULL)
  testthat::expect_s3_class(table, "gt_tbl")
})
