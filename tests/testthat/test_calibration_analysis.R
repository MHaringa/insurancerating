calibration_portfolio <- function() {
  data.frame(
    claims = c(2, 1, 8, 4, 6, 4),
    exposure = c(2, 1, 4, 2, 2, 1),
    x = c(0, 0, 1, 1, 2, 2),
    year = c(2024L, 2023L, 2024L, NA_integer_, 2023L, 2024L),
    region = factor(c("A", "B", "A", "B", NA, "A"), levels = c("B", "A", "C"))
  )
}

test_that("offset calibration totals are on the claim-count scale", {
  portfolio <- calibration_portfolio()
  formula_offset <- glm(claims ~ x + offset(log(exposure)), poisson(), portfolio)
  call_offset <- glm(claims ~ x, poisson(), portfolio, offset = log(exposure))

  evaluation <- portfolio
  evaluation$exposure <- rev(evaluation$exposure)
  beta <- unname(coef(formula_offset))
  expected <- exp(beta[1] + beta[2] * evaluation$x) * evaluation$exposure

  result <- calibration_analysis(formula_offset, evaluation)
  expect_named(result, c("Observed", "Expected", "OE", "N", "Exposure"))
  expect_s3_class(result, "calibration_result")
  expect_equal(data.frame(as.list(result)), data.frame(
    Observed = sum(evaluation$claims),
    Expected = sum(expected),
    OE = sum(evaluation$claims) / sum(expected),
    N = nrow(evaluation),
    Exposure = sum(evaluation$exposure)
  ))
  expect_equal(calibration_analysis(call_offset, evaluation), result)
})

test_that("calendar years and factor levels retain their ordering and missing group", {
  portfolio <- calibration_portfolio()
  model <- glm(claims ~ x + offset(log(exposure)), poisson(), portfolio)
  overall <- calibration_analysis(model, portfolio)
  year <- calibration_analysis(model, portfolio, by = "year")
  region <- calibration_analysis(model, portfolio, by = "region")
  expect_identical(year$year, c(2023L, 2024L, NA_integer_))
  expect_identical(as.character(region$region), c("B", "A", NA_character_))
  expect_identical(levels(region$region), levels(portfolio$region))
  expect_equal(year$Observed, c(7, 14, 4))
  expect_equal(year$Exposure, c(3, 7, 2))
  expect_equal(region$Observed, c(5, 14, 6))
  for (grouped in list(year, region)) {
    expect_equal(unname(colSums(grouped[c("Observed", "Expected", "N", "Exposure")])),
                 unname(unlist(overall[c("Observed", "Expected", "N", "Exposure")])))
    expect_equal(grouped$OE, grouped$Observed / grouped$Expected)
  }
})

test_that("calibration groups by observed combinations in the requested order", {
  portfolio <- calibration_portfolio()
  model <- glm(claims ~ x + offset(log(exposure)), poisson(), portfolio)
  result <- calibration_analysis(model, portfolio, by = c("year", "region"))

  expect_named(result, c("year", "region", "Observed", "Expected", "OE", "N", "Exposure"))
  expect_identical(result$year, c(2023L, 2023L, 2024L, NA_integer_))
  expect_identical(as.character(result$region), c("B", NA_character_, "A", "B"))
  expect_identical(levels(result$region), levels(portfolio$region))
  expect_equal(result$Observed, c(1, 6, 14, 4))
  expect_equal(result$N, c(1, 1, 3, 1))
  expect_equal(result$Exposure, c(1, 2, 7, 2))
  mu <- unname(fitted(model))
  expect_equal(result$Expected, c(mu[2], mu[5], sum(mu[c(1, 3, 6)]), mu[4]))
  expect_equal(result$OE, result$Observed / result$Expected)

  backwards <- calibration_analysis(model, portfolio, by = c("region", "year"))
  expect_named(backwards, c("region", "year", "Observed", "Expected", "OE", "N", "Exposure"))
  expect_equal(backwards$Observed, c(1, 4, 14, 6))
  expect_identical(attr(result, "calibration_by"), c("year", "region"))
})

test_that("calibration keeps separate grouping keys and preserves their types", {
  portfolio <- data.frame(
    claims = c(1, 2, 3, 4),
    region = c("A / B", "A", "A", "A / B"),
    segment = c("C", "B / C", "B / C", "C"),
    period = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01", "2024-02-01"))
  )
  model <- glm(claims ~ 1, poisson(), portfolio)
  result <- calibration_analysis(model, portfolio, by = c("region", "segment", "period"))
  expect_identical(result$period, portfolio$period[c(2, 1, 4)])
  expect_equal(result$Observed, c(5, 1, 4))
  expect_equal(result$Expected, c(5, 2.5, 2.5))
})

test_that("weighted claim-rate models recover counts and do not reuse an exposure offset", {
  portfolio <- calibration_portfolio()
  portfolio$rate <- portfolio$claims / portfolio$exposure
  weighted_rate <- glm(rate ~ x, poisson(), portfolio, weights = exposure)
  count <- glm(claims ~ x + offset(log(exposure)), poisson(), portfolio)
  both <- glm(claims ~ x + offset(log(exposure)), poisson(), portfolio, weights = exposure)

  expect_equal(calibration_analysis(weighted_rate, portfolio),
               calibration_analysis(count, portfolio))
  result <- calibration_analysis(both, portfolio)
  expect_equal(result$Observed, sum(portfolio$claims))
  expect_equal(result$Expected, sum(predict(both, portfolio, type = "response")))

  portfolio$duration <- portfolio$exposure
  inline_rate <- glm(I(claims / duration) ~ x, poisson(), portfolio, weights = duration)
  expect_equal(calibration_analysis(inline_rate, portfolio),
               calibration_analysis(weighted_rate, portfolio))

  portfolio$case_weight <- c(2, 1, 1, 3, 1, 2)
  case_weighted <- glm(claims ~ x + offset(log(exposure)), poisson(),
                       portfolio, weights = case_weight)
  result <- calibration_analysis(case_weighted, portfolio)
  expect_equal(result$Observed, sum(portfolio$claims * portfolio$case_weight))
  expect_equal(result$Expected,
               sum(predict(case_weighted, portfolio, type = "response") * portfolio$case_weight))
  expect_equal(result$Exposure, sum(portfolio$exposure))
})

test_that("risk deciles rank expected frequency rather than expected claim counts", {
  train <- data.frame(x = 0:2, claims = c(2, 4, 8), exposure = 1)
  model <- glm(claims ~ x + offset(log(exposure)), poisson(), train)
  evaluation <- data.frame(
    x = seq(0, 1, length.out = 20),
    claims = seq_len(20),
    exposure = rep(c(0.5, 1.5), 10)
  )
  result <- calibration_analysis(model, evaluation[20:1, ], groups = 10)
  expect_identical(result$Group, seq_len(10))
  expect_equal(result$N, rep(2, 10))
  expect_equal(result$Exposure, rep(2, 10))
  expect_equal(result$Observed, rowSums(matrix(evaluation$claims, 10, byrow = TRUE)))
  expected <- 2 * 2^evaluation$x * evaluation$exposure
  expect_equal(result$Expected, rowSums(matrix(expected, 10, byrow = TRUE)))
})

test_that("tied risk uses input order and exposure is balanced without splitting rows", {
  portfolio <- data.frame(claims = c(8, 2, 3, 1), exposure = c(3, 1, 1, 1))
  count <- glm(claims ~ 1 + offset(log(exposure)), poisson(), portfolio)
  groups <- calibration_analysis(count, portfolio, groups = 2)
  expect_identical(groups$N, c(1L, 3L))
  expect_equal(groups$Observed, c(8, 6))
  expect_equal(groups$Exposure, c(3, 3))
  expect_identical(groups, calibration_analysis(count, portfolio, groups = 2))

  no_offset <- glm(claims ~ 1, poisson(), portfolio)
  groups <- calibration_analysis(no_offset, portfolio, groups = 2)
  expect_identical(groups$N, c(2L, 2L))
  expect_equal(groups$Observed, c(10, 4))
  expect_false("Exposure" %in% names(groups))
})

test_that("risk grouping retains tariff components in a compound exposure offset", {
  portfolio <- data.frame(
    claims = c(2, 9, 7, 1),
    exposure = c(2, 1, 1, 2),
    relativity = c(1, 1.5, 1.5, 1)
  )
  model <- glm(claims ~ 1 + offset(log(exposure) + log(relativity)),
               poisson(), portfolio)
  result <- calibration_analysis(model, portfolio, groups = 2)

  # By count the higher risk rows would come first, since they have less exposure.
  expect_equal(result$Observed, c(2, 17))
  expect_equal(result$Exposure, c(2, 4))
  expect_equal(sum(result$Expected), sum(predict(model, portfolio, type = "response")))
})

test_that("weighted Gamma and Tweedie calibration uses prediction-row weights", {
  portfolio <- calibration_portfolio()
  portfolio$severity <- c(180, 140, 250, 200, 320, 350)
  portfolio$volume <- c(2, 1, 2, 4, 6, 1)
  evaluation <- portfolio
  evaluation$volume <- rev(portfolio$volume)
  for (family in list(Gamma(link = "log"), mgcv::Tweedie(p = 1.5))) {
    model <- glm(severity ~ x, family = family, data = portfolio, weights = volume)
    result <- calibration_analysis(model, evaluation)
    expected <- with(evaluation, exp(coef(model)[1] + coef(model)[2] * x))
    expect_equal(result$Observed, sum(evaluation$severity * evaluation$volume))
    expect_equal(result$Expected, sum(expected * evaluation$volume))
    expect_false("Exposure" %in% names(result))
  }
})

test_that("zero expected outcomes have undefined O/E", {
  portfolio <- calibration_portfolio()
  portfolio$benchmark <- portfolio$claims + 1
  model <- glm(claims ~ 0 + offset(benchmark), poisson(link = "identity"), portfolio)
  portfolio$benchmark <- 0
  result <- calibration_analysis(model, portfolio)
  expect_equal(result$Expected, 0)
  expect_identical(result$OE, NA_real_)
  expect_error(calibration_analysis(model, portfolio, groups = 2), "identifiable exposure")
})

test_that("invalid grouping, model families and missing model inputs are rejected", {
  portfolio <- calibration_portfolio()
  model <- glm(claims ~ x + offset(log(exposure)), poisson(), portfolio)
  for (groups in list(0, -1, 1.5, "2", NA, Inf, c(2, 3), nrow(portfolio) + 1)) {
    expect_error(calibration_analysis(model, portfolio, groups = groups), "positive whole number")
  }
  expect_error(calibration_analysis(model, portfolio, by = "year", groups = 2), "not both")
  expect_error(calibration_analysis(model, portfolio, by = c("year", "region"), groups = 2),
               "not both")
  expect_error(calibration_analysis(model, portfolio, by = "missing"), "was not found")
  expect_error(calibration_analysis(model, portfolio, by = "yaer"), "Did you mean `year`")
  expect_error(calibration_analysis(model, portfolio, by = c("year", "regio")),
               "Did you mean `region`")
  for (by in list(character(), c("year", "year"), c("year", NA), c("year", ""))) {
    expect_error(calibration_analysis(model, portfolio, by = by), "distinct column names")
  }
  absent <- portfolio[c("claims", "region")]
  expect_error(calibration_analysis(model, absent), "Missing model variables.*x.*exposure")
  expect_error(calibration_analysis(model, transform(portfolio, claims = NA_real_)),
               "Observed responses")
  expect_error(calibration_analysis(model, transform(portfolio, exposure = 0)), "must be positive")
  weighted <- do.call(glm, list(
    formula = claims ~ x, family = poisson(), data = portfolio, weights = portfolio$exposure
  ))
  expect_error(calibration_analysis(weighted, portfolio), "rather than a fixed vector")
  expect_error(calibration_analysis(lm(claims ~ x, portfolio), portfolio), "fitted GLM")
  gaussian <- glm(claims ~ x, data = portfolio)
  expect_error(calibration_analysis(gaussian, portfolio), "family is 'gaussian'")
})

test_that("autoplot chooses overall and equal-scale predicted-risk views", {
  portfolio <- calibration_portfolio()
  model <- glm(claims ~ x + offset(log(exposure)), poisson(), portfolio)
  overall <- calibration_analysis(model, portfolio)
  plot <- ggplot2::autoplot(overall)
  expect_s3_class(plot, "ggplot")
  expect_equal(ggplot2::ggplot_build(plot)$data[[1L]]$yintercept, 1)
  expect_equal(plot$data$OE, overall$OE)

  risk <- calibration_analysis(model, portfolio, groups = 3)
  original <- risk
  plot <- ggplot2::autoplot(risk)
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$data$Expected, risk$Expected / risk$Exposure)
  expect_equal(plot$data$Observed, risk$Observed / risk$Exposure)
  expect_equal(ggplot2::ggplot_build(plot)$data[[1L]]$slope, 1)
  expect_equal(ggplot2::ggplot_build(plot)$data[[1L]]$intercept, 0)
  expect_identical(plot$coordinates$limits$x, plot$coordinates$limits$y)
  expect_identical(risk, original)

  unweighted <- glm(claims ~ x, poisson(), portfolio)
  totals <- calibration_analysis(unweighted, portfolio, groups = 3)
  expect_identical(ggplot2::autoplot(totals)$data$Observed, totals$Observed)
})

test_that("autoplot preserves period and factor ordering and handles missing values", {
  portfolio <- calibration_portfolio()
  portfolio$period <- as.Date(paste0(portfolio$year, "-01-01"), format = "%Y-%m-%d")
  portfolio$calendar <- portfolio$year
  portfolio$benchmark <- portfolio$claims + 1
  model <- glm(claims ~ 0 + offset(benchmark), poisson(link = "identity"), portfolio)
  portfolio$benchmark[portfolio$region %in% "B"] <- 0

  for (column in c("period", "calendar")) {
    results <- calibration_analysis(model, portfolio, by = column)
    temporal <- ggplot2::autoplot(results)
    expect_s3_class(temporal, "ggplot")
    expect_identical(temporal$data$.group, results[[column]][!is.na(results[[column]])])
    expect_true(inherits(temporal$layers[[2L]]$geom, "GeomLine"))
    expect_match(temporal$labels$caption, "Missing periods")
  }

  factor_results <- calibration_analysis(model, portfolio, by = "region")
  level_plot <- ggplot2::autoplot(factor_results)
  expect_s3_class(level_plot, "ggplot")
  expect_identical(level_plot$scales$get_scales("x")$labels, c("B", "A", "(Missing)"))
  expect_equal(ggplot2::ggplot_build(level_plot)$data[[1L]]$yintercept, 1)
  expect_true(is.na(level_plot$data$OE[1L]))
  expect_silent(ggplot2::ggplot_gtable(ggplot2::ggplot_build(level_plot)))
  override <- ggplot2::autoplot(calibration_analysis(model, portfolio, by = "calendar"),
                               type = "factor")
  expect_identical(override$scales$get_scales("x")$labels, c("2023", "2024", "(Missing)"))
})

test_that("autoplot shows one point per calibration combination", {
  portfolio <- calibration_portfolio()
  model <- glm(claims ~ x + offset(log(exposure)), poisson(), portfolio)
  result <- calibration_analysis(model, portfolio, by = c("year", "region"))
  plot <- ggplot2::autoplot(result)

  expect_s3_class(plot, "ggplot")
  expect_identical(
    plot$scales$get_scales("x")$labels,
    c("2023 / B", "2023 / (Missing)", "2024 / A", "(Missing) / B")
  )
  expect_true(inherits(plot$layers[[2L]]$geom, "GeomPoint"))
  expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[2L]]), nrow(result))
  expect_error(ggplot2::autoplot(result, type = "temporal"), "not available")
})
.calibration_refinement_fixture <- function() {
  data <- data.frame(
    age = rep(21:60, each = 4),
    earned = rep(c(0.4, 0.7, 1, 1), 40)
  )
  data$age_band <- cut(data$age, c(20, 30, 40, 50, 60))
  data$claims <- rep(c(0, 0, 0, 1), 40) + as.integer(data$age > 40)
  data
}

test_that("calibration uses the new smoothing bands and actual exposure", {
  portfolio <- .calibration_refinement_fixture()
  base <- glm(claims ~ age_band + offset(log(earned)), poisson(), data = portfolio)
  model <- prepare_refinement(base, portfolio) |>
    add_smoothing("age_band", "age", breaks = c(20, 35, 50, 60),
                  smoothing = "poly", degree = 2) |>
    refit()

  expect_error(calibration_analysis(model, portfolio),
               "age_band_smooth.*extract_model_data")
  expect_error(calibration_analysis(model, portfolio, by = "age_smooth"),
               "age_smooth.*extract_model_data")
  expect_error(calibration_analysis(model, portfolio, by = "age"),
               "age_band_smooth.*Did you mean `age_smooth`.*extract_model_data")
  expect_error(calibration_analysis(model, portfolio, by = "age_smoth"),
               "Did you mean `age_smooth`.*extract_model_data")
  expect_error(calibration_analysis(model, portfolio, by = "typo"),
               "was not found")

  refined_data <- extract_model_data(model)
  expect_error(calibration_analysis(model, refined_data, by = "age_smoth"),
               "Did you mean `age_smooth`")
  expect_identical(calibration_analysis(model, refined_data, by = "age")$age, 21:60)
  bands <- calibration_analysis(model, refined_data, by = "age_smooth")
  expect_length(bands$age_smooth, 3L)
  expect_equal(sum(bands$Observed), sum(portfolio$claims))
  expect_equal(sum(bands$Expected), sum(fitted(model)))
  expect_equal(sum(bands$Exposure), sum(portfolio$earned))
  expected <- tapply(fitted(model), refined_data$age_smooth, sum)
  expect_equal(bands$Expected, as.numeric(expected[as.character(bands$age_smooth)]))

  groups <- calibration_analysis(model, refined_data, groups = 3)
  expect_equal(sum(groups$Exposure), sum(portfolio$earned))
  expect_equal(sum(groups$Expected), sum(fitted(model)))
  expect_true(all(diff(groups$Expected / groups$Exposure) >= 0))
})

test_that("a fixed restriction offset is not treated as exposure", {
  portfolio <- .calibration_refinement_fixture()
  for (formula in list(claims ~ age_band + offset(log(earned)), claims ~ age_band)) {
    base <- glm(formula, poisson(), data = portfolio)
    model <- prepare_refinement(base, portfolio) |>
      add_restriction(c("(50,60]" = 1.3), model_variable = "age_band") |>
      refit()
    expect_error(calibration_analysis(model, portfolio),
                 "age_band_restricted.*extract_model_data")
    data <- extract_model_data(model)
    before <- calibration_analysis(base, data, by = "age_band")
    after <- calibration_analysis(model, data, by = "age_band")
    expect_identical(before$Observed, after$Observed)
    expect_false(isTRUE(all.equal(before$Expected, after$Expected)))
    expect_equal(sum(after$Expected), sum(fitted(model)))
    groups <- calibration_analysis(model, data, groups = 2)
    expect_identical(after$Exposure, before$Exposure)
    expect_equal(sum(groups$Expected), sum(after$Expected))
    expect_identical("Exposure" %in% names(groups), "Exposure" %in% names(before))
  }
})
