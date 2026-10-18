multiplier_fixture <- function(b_relativity = 1.8) {
  exposure <- rep(10, 6)
  claims_a <- c(8, 10, 12)
  claims_b <- round(claims_a * b_relativity)
  data <- data.frame(
    claims = c(claims_a, claims_b),
    exposure = exposure,
    risk_class = factor(rep(c("A", "B"), each = 3))
  )
  model <- glm(
    claims ~ risk_class + offset(log(exposure)),
    family = poisson(),
    data = data
  )
  list(model = model, data = data)
}

tariff_relativity <- function(model, risk_factor, level) {
  table <- rating_table(model, exposure = FALSE)$df
  estimate <- names(table)[3]
  table[[estimate]][
    table$risk_factor == risk_factor & table$level == level
  ][1]
}

testthat::test_that("fixed is the default restriction type", {
  fixture <- multiplier_fixture()
  restrictions <- data.frame(risk_class = "B", fixed = 0.95)

  implicit <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(restrictions)
  explicit <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(restrictions, restriction_type = "fixed")

  testthat::expect_identical(implicit$steps, explicit$steps)
  testthat::expect_identical(implicit$steps[[1]]$restriction_type, "fixed")
})

testthat::test_that("named vectors are the concise restriction input", {
  fixture <- multiplier_fixture()

  fixed <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(
      restrictions = c(A = 1, B = 0.95),
      model_variable = "risk_class"
    )
  multiplier <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(
      restrictions = c(A = 1, B = 1.1),
      model_variable = "risk_class",
      restriction_type = "multiplier"
    )

  testthat::expect_identical(
    names(fixed$steps[[1]]$restrictions),
    c("risk_class", "risk_class_restricted")
  )
  testthat::expect_identical(
    names(multiplier$steps[[1]]$restrictions),
    c("risk_class", "risk_class_multiplier")
  )
  testthat::expect_equal(
    tariff_relativity(refit(fixed), "risk_class_restricted", "B"),
    0.95
  )
  testthat::expect_equal(
    tariff_relativity(refit(multiplier), "risk_class", "B"),
    tariff_relativity(fixture$model, "risk_class", "B") * 1.1
  )
})

testthat::test_that("named-vector and data-frame routes are equivalent", {
  fixture <- multiplier_fixture()

  vector_form <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(
      restrictions = c(A = 1, B = 0.95),
      model_variable = "risk_class",
      output_variable = "tariff_relativity"
    )
  data_frame_form <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(data.frame(
      risk_class = c("A", "B"),
      tariff_relativity = c(1, 0.95)
    ))

  testthat::expect_identical(vector_form$steps, data_frame_form$steps)
})

testthat::test_that("named restriction levels may be non-syntactic", {
  data <- data.frame(
    claims = c(1, 2, 3, 4),
    exposure = rep(1, 4),
    activity = factor(c(
      "retail shop", "office / services",
      "retail shop", "office / services"
    ))
  )
  model <- glm(
    claims ~ activity + offset(log(exposure)),
    family = poisson(),
    data = data
  )
  refinement <- prepare_refinement(model, data) |>
    add_restriction(
      restrictions = c(
        "retail shop" = 1.1,
        "office / services" = 0.9
      ),
      model_variable = "activity"
    )

  testthat::expect_setequal(
    refinement$steps[[1]]$restrictions$activity,
    c("retail shop", "office / services")
  )
})

testthat::test_that("named-vector restriction input is validated", {
  fixture <- multiplier_fixture()
  refinement <- prepare_refinement(fixture$model, fixture$data)

  testthat::expect_error(
    add_restriction(refinement, c(A = 1, B = 0.95)),
    "model_variable"
  )
  testthat::expect_error(
    add_restriction(
      refinement,
      c(1, 0.95),
      model_variable = "risk_class"
    ),
    "non-empty level name"
  )
  testthat::expect_error(
    add_restriction(
      refinement,
      stats::setNames(c(1, 0.95), c("A", "")),
      model_variable = "risk_class"
    ),
    "non-empty level name"
  )
  testthat::expect_error(
    add_restriction(
      refinement,
      stats::setNames(c(1, 0.95), c("A", "A")),
      model_variable = "risk_class"
    ),
    "duplicate level"
  )
  testthat::expect_error(
    add_restriction(
      refinement,
      c(A = "1", B = "0.95"),
      model_variable = "risk_class"
    ),
    "named numeric vector"
  )
  testthat::expect_error(
    add_restriction(
      refinement,
      data.frame(risk_class = "B", fixed = 0.95),
      model_variable = "risk_class"
    ),
    "Do not also supply"
  )
  testthat::expect_error(
    add_restriction(
      refinement,
      c(B = 0.95),
      model_variable = "risk_class",
      output_variable = "risk_class"
    ),
    "must differ"
  )
})

testthat::test_that("multipliers scale current model relativities", {
  fixture <- multiplier_fixture()
  original <- tariff_relativity(fixture$model, "risk_class", "B")

  adjusted <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(
      data.frame(risk_class = c("A", "B"), multiplier = c(1, 1.1)),
      restriction_type = "multiplier"
    ) |>
    refit()

  testthat::expect_equal(
    tariff_relativity(adjusted, "risk_class", "B"),
    original * 1.1
  )
  testthat::expect_equal(
    tariff_relativity(adjusted, "risk_class", "A"),
    1
  )
})

testthat::test_that("multipliers below one decrease current relativities", {
  fixture <- multiplier_fixture()
  original <- tariff_relativity(fixture$model, "risk_class", "B")
  adjusted <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(
      data.frame(risk_class = "B", multiplier = 0.9),
      restriction_type = "multiplier"
    ) |>
    refit()

  testthat::expect_equal(
    tariff_relativity(adjusted, "risk_class", "B"),
    original * 0.9
  )
})

testthat::test_that("fixed and multiplier restrictions follow step order", {
  fixture <- multiplier_fixture()

  fixed_then_multiplier <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(data.frame(risk_class = "B", fixed = 0.95)) |>
    add_restriction(
      data.frame(risk_class = "B", multiplier = 1.1),
      restriction_type = "multiplier"
    ) |>
    refit()
  multiplier_then_fixed <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(
      data.frame(risk_class = "B", multiplier = 1.1),
      restriction_type = "multiplier"
    ) |>
    add_restriction(data.frame(risk_class = "B", fixed = 0.95)) |>
    refit()

  testthat::expect_equal(
    tariff_relativity(fixed_then_multiplier, "fixed", "B"),
    0.95 * 1.1
  )
  testthat::expect_equal(
    tariff_relativity(multiplier_then_fixed, "fixed", "B"),
    0.95
  )
})

testthat::test_that("successive multiplier restrictions compound", {
  fixture <- multiplier_fixture()
  original <- tariff_relativity(fixture$model, "risk_class", "B")
  adjusted <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(
      data.frame(risk_class = "B", multiplier_one = 1.1),
      restriction_type = "multiplier"
    ) |>
    add_restriction(
      data.frame(risk_class = "B", multiplier_two = 0.9),
      restriction_type = "multiplier"
    ) |>
    refit()

  testthat::expect_equal(
    tariff_relativity(adjusted, "risk_class", "B"),
    original * 1.1 * 0.9
  )
})

testthat::test_that("multipliers are evaluated against each base model", {
  low <- multiplier_fixture(1.4)
  high <- multiplier_fixture(2)
  apply_multiplier <- function(fixture) {
    prepare_refinement(fixture$model, fixture$data) |>
      add_restriction(
        data.frame(risk_class = "B", multiplier = 1.1),
        restriction_type = "multiplier"
      ) |>
      refit()
  }

  low_original <- tariff_relativity(low$model, "risk_class", "B")
  high_original <- tariff_relativity(high$model, "risk_class", "B")
  testthat::expect_equal(
    tariff_relativity(apply_multiplier(low), "risk_class", "B"),
    low_original * 1.1
  )
  testthat::expect_equal(
    tariff_relativity(apply_multiplier(high), "risk_class", "B"),
    high_original * 1.1
  )
})

testthat::test_that("multipliers work after add_relativities", {
  portfolio <- data.frame(
    claims = c(1, 2, 3, 4, 2, 3, 4, 5),
    exposure = rep(1, 8),
    industry_group = factor(rep(c("A", "B"), each = 4)),
    industry_detail = factor(c(
      "A1", "A2", "A1", "A2", "B1", "B2", "B1", "B2"
    ))
  )
  model <- glm(
    claims ~ industry_group + offset(log(exposure)),
    family = poisson(),
    data = portfolio
  )
  split <- relativities(split_level("A", c(A1 = 0.9, A2 = 1.1)))

  adjusted <- prepare_refinement(model, portfolio) |>
    add_relativities(
      "industry_group", "industry_detail", split, "exposure",
      normalize = FALSE
    ) |>
    add_restriction(
      data.frame(industry_group_refined = "A1", multiplier = 1.1),
      restriction_type = "multiplier"
    ) |>
    refit()

  testthat::expect_equal(
    tariff_relativity(adjusted, "industry_group_refined", "A1"),
    0.9 * 1.1
  )
})

testthat::test_that("splits replace preceding multiplier offsets exactly once", {
  fixture <- multiplier_fixture()
  fixture$data$detail <- c("A1", "A2", "A2", "B1", "B1", "B2")
  split <- relativities(split_level("B", c(B1 = 0.9, B2 = 1.1)))
  original <- tariff_relativity(fixture$model, "risk_class", "B")

  for (vector_input in c(TRUE, FALSE)) {
    for (normalize in c(TRUE, FALSE)) {
      refinement <- prepare_refinement(fixture$model, fixture$data)
      if (vector_input) {
        refinement <- add_restriction(
          refinement, c(B = 1.15), model_variable = "risk_class",
          restriction_type = "multiplier"
        )
      } else {
        refinement <- add_restriction(
          refinement, data.frame(risk_class = "B", multiplier = 1.15),
          restriction_type = "multiplier"
        )
      }
      multiplier_column <- refinement$steps[[1]]$execution_column
      refinement <- add_relativities(
        refinement, "risk_class", "detail", split, "exposure",
        normalize = normalize
      )

      testthat::expect_warning(preview <- preview_refinement(refinement, 2), NA)
      testthat::expect_warning(fitted <- refit(refinement), NA)
      testthat::expect_false(
        multiplier_column %in% all.vars(stats::formula(fitted))
      )
      testthat::expect_setequal(
        all.vars(stats::formula(fitted)),
        c("claims", "risk_class_rel", "exposure")
      )

      split_values <- c(B1 = 0.9, B2 = 1.1)
      if (normalize) {
        split_values <- split_values / stats::weighted.mean(
          split_values, c(20, 10)
        )
      }
      expected <- c(A = 1, original * 1.15 * split_values)
      actual <- rating_table(fitted, exposure = FALSE)$df
      actual <- actual[actual$risk_factor != "(Intercept)", ]
      testthat::expect_identical(nrow(actual), 3L)
      testthat::expect_setequal(actual$risk_factor, "risk_class_refined")
      testthat::expect_equal(
        actual[[3]], unname(expected[actual$level])
      )

      row_levels <- ifelse(fixture$data$risk_class == "A", "A",
                           fixture$data$detail)
      expected_effect <- unname(expected[row_levels])
      testthat::expect_equal(preview$state$data$risk_class_rel, expected_effect)
      expected_count <- fixture$data$exposure * expected_effect
      expected_count <- expected_count *
        sum(fixture$data$claims) / sum(expected_count)
      testthat::expect_equal(unname(stats::fitted(fitted)), expected_count)
      testthat::expect_warning(
        ggplot2::ggplot_build(ggplot2::autoplot(refinement, step = 2)), NA
      )
    }
  }
})

testthat::test_that("splits use current coefficients after fixed and multiplier steps", {
  fixture <- multiplier_fixture()
  fixture$data$detail <- c("A1", "A2", "A2", "B1", "B1", "B2")
  split <- relativities(split_level("B", c(B1 = 0.9, B2 = 1.1)))

  for (variable in c("risk_class", "risk_class_restricted")) {
    refinement <- prepare_refinement(fixture$model, fixture$data) |>
      add_restriction(c(A = 1.2, B = 0.8), model_variable = "risk_class") |>
      add_restriction(
        c(B = 1.1), model_variable = "risk_class",
        restriction_type = "multiplier"
      ) |>
      add_relativities(
        variable, "detail", split, "exposure", normalize = FALSE,
        output_variable = "tariff_segment"
      )

    testthat::expect_warning(fitted <- refit(refinement), NA)
    testthat::expect_setequal(
      all.vars(stats::formula(fitted)),
      c("claims", "risk_class_rel", "exposure")
    )
    actual <- rating_table(fitted, exposure = FALSE)$df
    actual <- actual[actual$risk_factor != "(Intercept)", ]
    expected <- c(A = 1.2, B1 = 0.8 * 1.1 * 0.9, B2 = 0.8 * 1.1 * 1.1)
    testthat::expect_identical(nrow(actual), 3L)
    testthat::expect_setequal(actual$risk_factor, "tariff_segment")
    testthat::expect_equal(actual[[3]], unname(expected[actual$level]))
  }
})

testthat::test_that("splits consume cumulative multipliers and preserve other offsets", {
  fixture <- multiplier_fixture()
  fixture$data$detail <- c("A1", "A2", "A2", "B1", "B1", "B2")
  fixture$data$period <- rep(c("short", "long", "short"), 2)

  refinement <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(
      c(short = 0.9, long = 1.1), model_variable = "period",
      allow_new_risk_factors = TRUE
    ) |>
    add_restriction(
      c(B = 1.15), model_variable = "risk_class",
      restriction_type = "multiplier"
    ) |>
    add_restriction(
      c(B = 0.95), model_variable = "risk_class",
      restriction_type = "multiplier"
    ) |>
    add_relativities(
      "risk_class", "detail",
      relativities(split_level("B", c(B1 = 0.9, B2 = 1.1))),
      "exposure", normalize = FALSE
    )

  # Adding another multiplier executes the preceding steps for validation.
  testthat::expect_warning(
    refinement <- add_restriction(
      refinement, c(B1 = 1.02), model_variable = "risk_class_refined",
      restriction_type = "multiplier"
    ), NA
  )
  testthat::expect_warning(fitted <- refit(refinement), NA)
  original <- tariff_relativity(fixture$model, "risk_class", "B")
  expected <- c(A = 1, B1 = original * 1.15 * 0.95 * 0.9 * 1.02,
                B2 = original * 1.15 * 0.95 * 1.1)
  actual <- rating_table(fitted, exposure = FALSE)$df
  testthat::expect_setequal(
    setdiff(actual$risk_factor, "(Intercept)"),
    c("period_restricted", "risk_class_refined")
  )
  sector <- actual[actual$risk_factor == "risk_class_refined", ]
  testthat::expect_identical(nrow(sector), 3L)
  testthat::expect_equal(sector[[3]], unname(expected[sector$level]))
  period <- c(short = 0.9, long = 1.1)[fixture$data$period]
  row_levels <- ifelse(fixture$data$risk_class == "A", "A", fixture$data$detail)
  expected_count <- unname(expected[row_levels] * period * fixture$data$exposure)
  expected_count <- expected_count * sum(fixture$data$claims) / sum(expected_count)
  testthat::expect_equal(unname(stats::fitted(fitted)), expected_count)
})

testthat::test_that("multiplier validation is explicit", {
  fixture <- multiplier_fixture()
  refinement <- prepare_refinement(fixture$model, fixture$data)

  testthat::expect_error(
    add_restriction(
      refinement,
      data.frame(risk_class = "B", multiplier = 1.1),
      restriction_type = "absolute"
    ),
    "one of"
  )
  for (value in c(NA_real_, NaN, Inf, -1, 0)) {
    testthat::expect_error(
      add_restriction(
        refinement,
        data.frame(risk_class = "B", multiplier = value),
        restriction_type = "multiplier"
      ),
      "finite|greater than zero"
    )
  }
  testthat::expect_error(
    add_restriction(
      refinement,
      data.frame(unknown = "x", multiplier = 1.1),
      restriction_type = "multiplier"
    ),
    "requires an existing relativity"
  )
  testthat::expect_error(
    add_restriction(
      refinement,
      data.frame(risk_class = "new", multiplier = 1.1),
      restriction_type = "multiplier"
    ),
    "cannot create new level"
  )
  testthat::expect_error(
    add_restriction(
      refinement,
      data.frame(risk_class = "B", multiplier = 1.1),
      restriction_type = "multiplier",
      allow_new_risk_factors = TRUE
    ),
    NA
  )
  testthat::expect_error(
    add_restriction(
      refinement,
      data.frame(risk_class = "B", multiplier = 1.1),
      restriction_type = "multiplier",
      replaces = "risk_class"
    ),
    "only be used"
  )
})

testthat::test_that("multiplier metadata remains explicit", {
  fixture <- multiplier_fixture()
  refinement <- prepare_refinement(fixture$model, fixture$data) |>
    add_restriction(
      data.frame(risk_class = "B", multiplier = 1.1),
      restriction_type = "multiplier"
    )

  testthat::expect_identical(
    refinement$steps[[1]]$restriction_type,
    "multiplier"
  )
  testthat::expect_equal(
    refinement$steps[[1]]$restrictions$multiplier,
    c(1, 1.1)
  )
  testthat::expect_true(any(grepl(
    "Multiplier",
    capture.output(print(refinement))
  )))
  testthat::expect_match(
    summary(refinement)$steps$details,
    "type = multiplier"
  )
})
