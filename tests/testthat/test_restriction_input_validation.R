restriction_input_fixture <- function() {
  data <- data.frame(
    claims = c(1, 2, 1, 2, 3, 4, 3, 4),
    exposure = rep(1, 8),
    zip = factor(rep(c("1", "2"), each = 4)),
    detail = rep(c("a", "b"), 4),
    period = rep(c("short", "long"), 4)
  )
  model <- glm(claims ~ zip + offset(log(exposure)),
               family = poisson(), data = data)
  prepare_refinement(model, data)
}

testthat::test_that("restriction levels must be character or factor", {
  refinement <- restriction_input_fixture()
  for (type in c("fixed", "multiplier")) {
    for (levels in list(c(1, 2), 1:2, c(TRUE, FALSE),
                        as.Date(c("2026-01-01", "2026-01-02")))) {
      testthat::expect_error(
        add_restriction(
          refinement, data.frame(zip = levels, value = c(0.9, 1.1)),
          restriction_type = type
        ),
        "level column `zip`.*character or factor.*as.character"
      )
    }
    for (levels in list(c("1", "2"), factor(c("1", "2")),
                        ordered(c("1", "2")))) {
      testthat::expect_silent(actual <- add_restriction(
        refinement, data.frame(zip = levels, value = c(0.9, 1.1)),
        restriction_type = type
      ))
      expected <- add_restriction(
        refinement, c("1" = 0.9, "2" = 1.1), model_variable = "zip",
        output_variable = "value", restriction_type = type
      )
      testthat::expect_identical(actual$steps, expected$steps)
      testthat::expect_identical(
        predict(refit(actual), type = "response"),
        predict(refit(expected), type = "response")
      )
    }
  }
})

testthat::test_that("allowing new factors is harmless for existing factors", {
  refinement <- restriction_input_fixture()
  split <- refinement |>
    add_relativities(
      model_variable = "zip", split_variable = "detail",
      output_variable = "tariff_segment",
      relativities = relativities(split_level("2", c(a = 0.9, b = 1.1))),
      exposure = "exposure"
    )
  external <- add_restriction(
    refinement, c(short = 0.9, long = 1.1), model_variable = "period",
    allow_new_risk_factors = TRUE
  )
  for (type in c("fixed", "multiplier")) {
    for (case in list(
      list(refinement = refinement, variable = "zip", level = "2"),
      list(refinement = split, variable = "tariff_segment", level = "a"),
      list(refinement = external, variable = "period", level = "short")
    )) {
      for (vector_input in c(TRUE, FALSE)) {
        args <- list(model = case$refinement, restriction_type = type)
        if (vector_input) {
          args$restrictions <- stats::setNames(0.9, case$level)
          args$model_variable <- case$variable
        } else {
          args$restrictions <- stats::setNames(
            data.frame(case$level, 0.9),
            c(case$variable, paste0(case$variable, "_restricted"))
          )
        }
        testthat::expect_silent(actual <- do.call(
          add_restriction, c(args, list(allow_new_risk_factors = TRUE))
        ))
        expected <- do.call(add_restriction, args)
        testthat::expect_identical(
          predict(refit(actual), type = "response"),
          predict(refit(expected), type = "response")
        )
        testthat::expect_equal(length(actual$steps), length(expected$steps))
      }
    }
  }
})

testthat::test_that("permission to add factors does not relax other validation", {
  refinement <- restriction_input_fixture()
  testthat::expect_error(
    add_restriction(refinement, c(short = 0.9, long = 1.1),
                    model_variable = "period"),
    "allow_new_risk_factors = TRUE"
  )
  for (permission in c(FALSE, TRUE)) {
    testthat::expect_error(
      add_restriction(refinement, c(short = 0.9, long = 1.1),
                      model_variable = "period", restriction_type = "multiplier",
                      allow_new_risk_factors = permission),
      "requires an existing relativity"
    )
  }
  for (type in c("fixed", "multiplier")) {
    testthat::expect_error(
      add_restriction(refinement, c("2" = 0.9), model_variable = "zip",
                      restriction_type = type, allow_new_risk_factors = NA),
      "allow_new_risk_factors"
    )
  }
  testthat::expect_error(
    add_restriction(refinement, c(short = 0.9), model_variable = "period",
                    allow_new_risk_factors = TRUE),
    "Every observed level"
  )
})
