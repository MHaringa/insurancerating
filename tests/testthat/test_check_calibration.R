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

  result <- check_calibration(formula_offset, evaluation)
  expect_named(result, c("Observed", "Expected", "OE", "N", "Exposure"))
  expect_equal(result, data.frame(
    Observed = sum(evaluation$claims),
    Expected = sum(expected),
    OE = sum(evaluation$claims) / sum(expected),
    N = nrow(evaluation),
    Exposure = sum(evaluation$exposure)
  ))
  expect_equal(check_calibration(call_offset, evaluation), result)
})

test_that("calendar years and factor levels retain their ordering and missing group", {
  portfolio <- calibration_portfolio()
  model <- glm(claims ~ x + offset(log(exposure)), poisson(), portfolio)
  overall <- check_calibration(model, portfolio)
  year <- check_calibration(model, portfolio, by = "year")
  region <- check_calibration(model, portfolio, by = "region")
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

test_that("weighted claim-rate models recover counts and do not reuse an exposure offset", {
  portfolio <- calibration_portfolio()
  portfolio$rate <- portfolio$claims / portfolio$exposure
  weighted_rate <- glm(rate ~ x, poisson(), portfolio, weights = exposure)
  count <- glm(claims ~ x + offset(log(exposure)), poisson(), portfolio)
  both <- glm(claims ~ x + offset(log(exposure)), poisson(), portfolio, weights = exposure)

  expect_equal(check_calibration(weighted_rate, portfolio),
               check_calibration(count, portfolio))
  result <- check_calibration(both, portfolio)
  expect_equal(result$Observed, sum(portfolio$claims))
  expect_equal(result$Expected, sum(predict(both, portfolio, type = "response")))

  portfolio$duration <- portfolio$exposure
  inline_rate <- glm(I(claims / duration) ~ x, poisson(), portfolio, weights = duration)
  expect_equal(check_calibration(inline_rate, portfolio),
               check_calibration(weighted_rate, portfolio))

  portfolio$case_weight <- c(2, 1, 1, 3, 1, 2)
  case_weighted <- glm(claims ~ x + offset(log(exposure)), poisson(),
                       portfolio, weights = case_weight)
  result <- check_calibration(case_weighted, portfolio)
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
  result <- check_calibration(model, evaluation[20:1, ], groups = 10)
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
  groups <- check_calibration(count, portfolio, groups = 2)
  expect_identical(groups$N, c(1L, 3L))
  expect_equal(groups$Observed, c(8, 6))
  expect_equal(groups$Exposure, c(3, 3))
  expect_identical(groups, check_calibration(count, portfolio, groups = 2))

  no_offset <- glm(claims ~ 1, poisson(), portfolio)
  groups <- check_calibration(no_offset, portfolio, groups = 2)
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
  result <- check_calibration(model, portfolio, groups = 2)

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
    result <- check_calibration(model, evaluation)
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
  result <- check_calibration(model, portfolio)
  expect_equal(result$Expected, 0)
  expect_identical(result$OE, NA_real_)
  expect_error(check_calibration(model, portfolio, groups = 2), "identifiable exposure")
})

test_that("invalid grouping, model families and missing model inputs are rejected", {
  portfolio <- calibration_portfolio()
  model <- glm(claims ~ x + offset(log(exposure)), poisson(), portfolio)
  for (groups in list(0, -1, 1.5, "2", NA, Inf, c(2, 3), nrow(portfolio) + 1)) {
    expect_error(check_calibration(model, portfolio, groups = groups), "positive whole number")
  }
  expect_error(check_calibration(model, portfolio, by = "year", groups = 2), "not both")
  expect_error(check_calibration(model, portfolio, by = "missing"), "one column")
  absent <- portfolio[c("claims", "region")]
  expect_error(check_calibration(model, absent), "Missing model variables.*x.*exposure")
  expect_error(check_calibration(model, transform(portfolio, claims = NA_real_)),
               "Observed responses")
  expect_error(check_calibration(model, transform(portfolio, exposure = 0)), "must be positive")
  weighted <- do.call(glm, list(
    formula = claims ~ x, family = poisson(), data = portfolio, weights = portfolio$exposure
  ))
  expect_error(check_calibration(weighted, portfolio), "rather than a fixed vector")
  expect_error(check_calibration(lm(claims ~ x, portfolio), portfolio), "fitted GLM")
  gaussian <- glm(claims ~ x, data = portfolio)
  expect_error(check_calibration(gaussian, portfolio), "family is 'gaussian'")
})
