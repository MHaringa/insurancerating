library(insurancerating)
context("Model refinement: smoothing and restrictions coefficients")

mod1 <- glm(cyl ~ mpg + disp + offset(log(gear)),
            family = "poisson",
            data = mtcars)
mod2 <- glm(cyl ~ mpg + disp, offset = log(gear),
            family = "poisson",
            data = mtcars)
mod3 <- glm(cyl ~ mpg + disp,
            family = "poisson",
            data = mtcars)
mod4 <- glm(cyl ~ mpg + disp + offset(log(gear)) + offset(log(disp)),
            family = "poisson",
            data = mtcars)
mod5 <- glm(cyl ~ mpg + disp + offset(log(gear)),
            offset = log(disp),
            family = "poisson", data = mtcars)

testthat::test_that(
  "Correct offset-term is returned", {
    testthat::expect_equal(get_offset(mod1), "log(gear)")
    testthat::expect_equal(get_offset(mod2), "log(gear)")
    testthat::expect_equal(get_offset(mod3), NULL)
  }
)

testthat::test_that(
  "Error is returned for multiple offset-terms", {
    testthat::expect_error(get_offset(mod4))
    testthat::expect_error(get_offset(mod5))
  }
)

testthat::test_that(
  "Offset-term is removed from formula", {
    fm <- formula(cyl ~ mpg + disp)
    testthat::expect_equal(remove_offset_formula(formula(mod1)), fm)
    testthat::expect_equal(remove_offset_formula(formula(mod2)), fm)
    testthat::expect_equal(remove_offset_formula(formula(mod3)), fm)
    testthat::expect_equal(remove_offset_formula(formula(mod4)), fm)
    testthat::expect_equal(remove_offset_formula(formula(mod5)), fm)
  }
)

testthat::test_that(
  "prepare_refinement validates supplied model data", {
    df <- data.frame(
      y = c(1, 2, 1, 3),
      exposure = rep(1, 4),
      zip = factor(c("a", "b", "a", "b"))
    )
    model <- glm(y ~ zip + offset(log(exposure)), family = poisson(), data = df)

    testthat::expect_s3_class(prepare_refinement(model, data = df),
                              "rating_refinement")
    testthat::expect_error(
      prepare_refinement(model, data = df[1:3, ]),
      "same number of rows"
    )
    testthat::expect_error(
      prepare_refinement(model, data = df[, c("y", "zip")]),
      "missing model column"
    )
    testthat::expect_error(
      prepare_refinement(model, data = list()),
      "data.frame"
    )
  }
)

testthat::test_that(
  "refinement workflow supports restriction, refit, summary and plotting", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      zip = factor(c("a", "b", "c", "a", "b", "c")),
      zip_split = factor(c("a1", "b1", "c1", "a2", "b2", "c2")),
      age = c(20, 30, 40, 50, 60, 70)
    )
    model <- glm(y ~ zip + offset(log(exposure)),
                 family = poisson(),
                 data = df)
    restrictions <- data.frame(
      zip = c("a", "b", "c"),
      zip_rst = c(0.9, 1, 1.1)
    )

    ref <- prepare_refinement(model, data = df) |>
      add_restriction(restrictions)

    testthat::expect_s3_class(ref, "rating_refinement")
    testthat::expect_equal(summary(ref)$n_steps, 1)
    testthat::expect_named(preview_refinement(ref), c("state", "step"))

    refined <- refit(ref)
    testthat::expect_s3_class(refined, "glm")
    testthat::expect_s3_class(refined, "refitrestricted")
    testthat::expect_equal(as.character(refined$call$data), "refined_data")

    testthat::expect_s3_class(ggplot2::autoplot(ref), "ggplot")
  }
)

testthat::test_that(
  "add_smoothing validates public arguments before fitting", {
    df <- data.frame(
      y = c(1, 2, 1, 3),
      exposure = rep(1, 4),
      zip = factor(c("a", "b", "a", "b")),
      age = c(20, 30, 40, 50)
    )
    model <- glm(y ~ zip + offset(log(exposure)), family = poisson(), data = df)
    ref <- prepare_refinement(model, data = df)

    testthat::expect_error(
      add_smoothing(ref, model_variable = "zip", source_variable = "age",
                    breaks = c(1, 2),
                    smoothing = "bad"),
      "smoothing"
    )
    testthat::expect_error(
      add_smoothing(ref, model_variable = "zip", source_variable = "age",
                    breaks = c(2, 1)),
      "strictly increasing"
    )
    testthat::expect_error(
      add_smoothing(ref, model_variable = "missing", source_variable = "age",
                    breaks = c(1, 2)),
      "model_variable"
    )
    testthat::expect_error(
      add_smoothing(ref, model_variable = "zip", source_variable = "age",
                    breaks = c(1, 2),
                    degree = "2"),
      "degree"
    )
    testthat::expect_error(
      add_smoothing(ref, model_variable = "zip", source_variable = "age",
                    breaks = c(1, 2),
                    k = 1.5),
      "k"
    )
    testthat::expect_s3_class(
      add_smoothing(ref, model_variable = "zip", source_variable = "age",
                    breaks = c(1, 2)),
      "rating_refinement"
    )
    testthat::expect_warning(
      add_smoothing(ref, tariff_class = "zip", rating_variable = "age",
                    breaks = c(1, 2)),
      "deprecated"
    )
    testthat::expect_warning(
      add_smoothing(ref, x_cut = "zip", x_org = "age", breaks = c(1, 2)),
      "deprecated"
    )
    testthat::expect_error(
      add_smoothing(ref, model_variable = "zip", x_cut = "zip",
                    source_variable = "age", breaks = c(1, 2)),
      "Use only one"
    )
  }
)

testthat::test_that(
  "add_relativities validates inputs before storing a step", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      zip = factor(c("a", "b", "c", "a", "b", "c")),
      zip_split = factor(c("a1", "b1", "c1", "a2", "b2", "c2"))
    )
    model <- glm(y ~ zip + offset(log(exposure)),
                 family = poisson(),
                 data = df)
    ref <- prepare_refinement(model, data = df)
    rel <- relativities_list(
      split_level("a", c("a1", "a2"), c(1, 1.2))
    )

    testthat::expect_error(
      add_relativities(ref, "zip", "zip_split", rel, "exposure",
                       normalize = NA),
      "normalize"
    )
    testthat::expect_error(
      add_relativities(ref, model_variable = "zip", split_variable = "missing",
                       relativities = rel, exposure = "exposure"),
      "split_variable"
    )
    testthat::expect_s3_class(
      add_relativities(ref, model_variable = "zip", split_variable = "zip_split",
                       relativities = rel, exposure = "exposure"),
      "rating_refinement"
    )
  }
)
