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
      "does not appear to be the same data"
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
  "refinement steps require a retained rating_refinement specification", {
    portfolio <- data.frame(
      claims = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      risk_group = factor(c("A", "B", "C", "A", "B", "C")),
      risk_detail = factor(c("A1", "B1", "C1", "A2", "B2", "C2"))
    )
    model <- glm(
      claims ~ risk_group + offset(log(exposure)),
      family = poisson(),
      data = portfolio
    )
    restrictions <- data.frame(
      risk_group = c("A", "B", "C"),
      risk_group_restricted = c(0.9, 1.0, 1.1)
    )
    rel <- relativities(
      split_level("A", c("A1", "A2"), c(0.9, 1.1))
    )

    refinement <- prepare_refinement(model, data = portfolio) |>
      add_restriction(restrictions)
    refitted_model <- refit(refinement)

    ordinary_glm_error <- "cannot be added to or edited on a fitted GLM"
    refitted_glm_error <- paste0(
      "cannot be added to or edited on a refitted GLM returned by ",
      "`refit\\(\\)`"
    )

    testthat::expect_error(
      add_smoothing(model),
      ordinary_glm_error
    )
    testthat::expect_error(
      add_restriction(model, restrictions),
      ordinary_glm_error
    )
    testthat::expect_error(
      add_relativities(
        model,
        model_variable = "risk_group",
        split_variable = "risk_detail",
        relativities = rel,
        exposure = "exposure"
      ),
      ordinary_glm_error
    )
    testthat::expect_error(
      edit_smoothing(model, from = 0, to = 1),
      ordinary_glm_error
    )

    testthat::expect_error(
      add_smoothing(refitted_model),
      refitted_glm_error
    )
    testthat::expect_error(
      add_restriction(refitted_model, restrictions),
      refitted_glm_error
    )
    testthat::expect_error(
      add_relativities(
        refitted_model,
        model_variable = "risk_group",
        split_variable = "risk_detail",
        relativities = rel,
        exposure = "exposure"
      ),
      refitted_glm_error
    )
    testthat::expect_error(
      edit_smoothing(refitted_model, from = 0, to = 1),
      refitted_glm_error
    )

    new_round <- prepare_refinement(refitted_model)
    testthat::expect_s3_class(new_round, "rating_refinement")
    testthat::expect_length(new_round$steps, 0)
  }
)

testthat::test_that(
  "prepare_refinement explains rows omitted by missing model values", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      construction_year = c(1990, NA, 2000, 2005, 2010, 2015)
    )
    model <- glm(
      y ~ construction_year + offset(log(exposure)),
      family = poisson(),
      data = df,
      na.action = na.omit
    )

    error <- tryCatch(
      prepare_refinement(model, data = df),
      error = identity
    )
    message <- conditionMessage(error)

    testthat::expect_s3_class(error, "error")
    testthat::expect_match(message, "fitted on 5 observations")
    testthat::expect_match(message, "`data` contains 6 rows", fixed = TRUE)
    testthat::expect_match(message, "1 observation appears to have been omitted")
    testthat::expect_match(message, "- construction_year: 1", fixed = TRUE)
    testthat::expect_match(message, "model frame no longer contains")
  }
)

testthat::test_that(
  "prepare_refinement reports multiple numeric and factor predictors", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      insured_amount = c(100, NA, 300, 400, 500, 600),
      sector = factor(c("Industry", "Retail", "Industry", "Retail", NA,
                        "Industry"))
    )
    model <- glm(
      y ~ insured_amount + sector + offset(log(exposure)),
      family = poisson(),
      data = df,
      na.action = na.omit
    )

    error <- tryCatch(
      prepare_refinement(model, data = df),
      error = identity
    )
    message <- conditionMessage(error)

    testthat::expect_match(message, "fitted on 4 observations")
    testthat::expect_match(message, "2 observations appear to have been omitted")
    testthat::expect_match(message, "- insured_amount: 1", fixed = TRUE)
    testthat::expect_match(message, "- sector: 1", fixed = TRUE)
  }
)

testthat::test_that(
  "prepare_refinement reports source variables in transformed terms", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2),
      exposure = rep(1, 5),
      insured_amount = c(100, 200, NA, 400, 500)
    )
    model <- glm(
      y ~ log(insured_amount) + offset(log(exposure)),
      family = poisson(),
      data = df,
      na.action = na.omit
    )

    error <- tryCatch(
      prepare_refinement(model, data = df),
      error = identity
    )
    message <- conditionMessage(error)

    testthat::expect_match(message, "- insured_amount: 1", fixed = TRUE)
    testthat::expect_false(grepl("- log\\(insured_amount\\):", message))
  }
)

testthat::test_that(
  "prepare_refinement keeps filtering mismatches general", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      sector = factor(c("A", "B", "A", "B", "A", "B"))
    )
    model <- glm(
      y ~ sector + offset(log(exposure)),
      family = poisson(),
      data = df,
      subset = seq_len(nrow(df)) != 2
    )

    error <- tryCatch(
      prepare_refinement(model, data = df),
      error = identity
    )
    message <- conditionMessage(error)

    testthat::expect_match(message, "model frame contains 5 rows")
    testthat::expect_match(message, "subsetting, filtering, or row removal")
    testthat::expect_false(grepl("appear to have been omitted.*missing", message))
  }
)

testthat::test_that(
  "prepare_refinement reports non-finite numeric model inputs separately", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2),
      exposure = rep(1, 5),
      insured_amount = c(100, 200, Inf, 400, 500)
    )
    model <- glm(
      y ~ insured_amount + offset(log(exposure)),
      family = poisson(),
      data = df,
      subset = is.finite(insured_amount)
    )

    error <- tryCatch(
      prepare_refinement(model, data = df),
      error = identity
    )
    message <- conditionMessage(error)

    testthat::expect_match(message, "non-finite values")
    testthat::expect_match(message, "- insured_amount: 1", fixed = TRUE)
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
    testthat::expect_identical(class(refined)[1], "refitrestricted")
    testthat::expect_identical(as.character(refined$call[[1]]), "glm")
    testthat::expect_equal(as.character(refined$call$data), "refined_data")

    printed <- paste(capture.output(print(refined)), collapse = "\n")
    testthat::expect_match(printed, "Refined generalized linear model")
    testthat::expect_match(printed, "Original formula:")
    testthat::expect_match(printed, "Refitted formula:")
    testthat::expect_match(printed, "Restriction: zip -> zip_rst \\(3 levels\\)")
    testthat::expect_match(printed, "Intercept-only refit: no")
    testthat::expect_match(printed, "Call:")
    testthat::expect_match(printed, "Coefficients:")
    testthat::expect_false(any(grepl(
      "function \\(formula, family",
      printed
    )))

    testthat::expect_s3_class(ggplot2::autoplot(ref), "ggplot")
  }
)

testthat::test_that(
  "refit validates intercept_only and supports object argument", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      zip = factor(c("a", "b", "c", "a", "b", "c"))
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

    testthat::expect_error(
      refit(object = ref, intercept_only = NA),
      "intercept_only"
    )

    refined <- refit(object = ref, intercept_only = TRUE)
    testthat::expect_s3_class(refined, "glm")
    testthat::expect_true(isTRUE(attr(refined, "intercept_only")))
  }
)

testthat::test_that(
  "add_restriction completes partial restrictions with fitted relativities", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      zip = factor(c("a", "b", "c", "a", "b", "c"))
    )
    model <- glm(y ~ zip + offset(log(exposure)),
                 family = poisson(),
                 data = df)

    restrictions <- data.frame(
      zip = "c",
      zip_rst = 1.4
    )

    ref <- prepare_refinement(model, data = df) |>
      add_restriction(restrictions)

    completed <- ref$steps[[1]]$restrictions

    testthat::expect_equal(as.character(completed$zip), c("a", "b", "c"))
    testthat::expect_equal(completed$zip_rst[completed$zip == "c"], 1.4)
    testthat::expect_equal(completed$zip_rst[completed$zip == "a"], 1)
    testthat::expect_equal(completed$zip_rst[completed$zip == "b"], 1)
    testthat::expect_s3_class(refit(ref), "refitrestricted")
  }
)

testthat::test_that(
  "add_restriction supports new tariff levels", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      postal_area = factor(c("A", "B", "C", "A", "B", "C"))
    )
    model <- glm(y ~ postal_area + offset(log(exposure)),
                 family = poisson(),
                 data = df)
    ref <- prepare_refinement(model, data = df)
    restrictions <- data.frame(
      postal_area = c("C", "D"),
      relativity = c(1.1, 1.2)
    )

    refined <- add_restriction(ref, restrictions)
    completed <- refined$steps[[1]]$restrictions

    testthat::expect_equal(
      as.character(completed$postal_area),
      c("A", "B", "C", "D")
    )
    testthat::expect_equal(
      completed$relativity[completed$postal_area == "D"],
      1.2
    )
    testthat::expect_identical(refined$steps[[1]]$new_levels, "D")
    testthat::expect_true(refined$steps[[1]]$allow_new_levels)

    fitted <- refit(refined)
    tariff <- rating_table(fitted, exposure = FALSE)$df

    testthat::expect_s3_class(fitted, "refitrestricted")
    testthat::expect_equal(
      tariff$est_fitted[
        tariff$risk_factor == "relativity" & tariff$level == "D"
      ],
      1.2
    )

    legacy <- suppressWarnings(
      restrict_coef(
        model,
        restrictions
      )
    )
    testthat::expect_s3_class(legacy, "rating_refinement")
    testthat::expect_identical(legacy$steps[[1]]$new_levels, "D")
    testthat::expect_s3_class(refit(legacy), "refitrestricted")
  }
)

testthat::test_that(
  "add_restriction validates restriction inputs and strict level matching", {
    df <- data.frame(
      y = c(1, 2, 1, 3),
      exposure = rep(1, 4),
      zip = factor(c("a", "b", "a", "b"))
    )
    model <- glm(y ~ zip + offset(log(exposure)),
                 family = poisson(),
                 data = df)
    ref <- prepare_refinement(model, data = df)

    testthat::expect_error(
      add_restriction(
        ref,
        data.frame(zip = "missing", zip_rst = 1.2),
        allow_new_levels = FALSE
      ),
      "not found"
    )
    testthat::expect_error(
      add_restriction(
        ref,
        data.frame(zip = "missing", zip_rst = 1.2),
        allow_new_levels = NA
      ),
      "allow_new_levels"
    )
    testthat::expect_error(
      add_restriction(ref, data.frame(zip = c("a", "a"), zip_rst = c(1, 1.1))),
      "unique"
    )
    testthat::expect_error(
      add_restriction(ref, data.frame(zip = "a", zip_rst = NA_real_)),
      "finite numeric"
    )
  }
)

testthat::test_that(
  "add_restriction can add an expert-specified risk factor", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      postal_area = factor(c("A", "B", "C", "A", "B", "C")),
      hail_zone = factor(c("low", "high", "low", "high", "low", "high"))
    )
    model <- glm(
      y ~ postal_area + offset(log(exposure)),
      family = poisson(),
      data = df
    )
    ref <- prepare_refinement(model, data = df)
    restrictions <- data.frame(
      hail_zone = c("low", "high"),
      hail_relativity = c(1, 1.2)
    )

    testthat::expect_error(
      add_restriction(ref, restrictions),
      "allow_new_risk_factors = TRUE",
      fixed = TRUE
    )

    refined <- add_restriction(
      ref,
      restrictions,
      allow_new_risk_factors = TRUE
    )

    testthat::expect_true(refined$steps[[1]]$new_risk_factor)
    testthat::expect_true(refined$steps[[1]]$allow_new_risk_factors)
    testthat::expect_equal(
      refined$steps[[1]]$restrictions,
      restrictions
    )

    fitted <- refit(refined)
    tariff <- rating_table(fitted, exposure = FALSE)$df

    testthat::expect_s3_class(fitted, "refitrestricted")
    testthat::expect_match(
      paste(deparse(stats::formula(fitted)), collapse = ""),
      "offset\\(log\\(hail_relativity\\) \\+ log\\(exposure\\)\\)"
    )
    testthat::expect_equal(
      tariff$est_fitted[
        tariff$risk_factor == "hail_relativity" & tariff$level == "low"
      ],
      1
    )
    testthat::expect_equal(
      tariff$est_fitted[
        tariff$risk_factor == "hail_relativity" & tariff$level == "high"
      ],
      1.2
    )
    testthat::expect_equal(
      attr(fitted, "restriction_map"),
      data.frame(
        source_var = "hail_zone",
        risk_factor = "hail_relativity",
        stringsAsFactors = FALSE
      )
    )
    testthat::expect_s3_class(ggplot2::autoplot(refined), "ggplot")

    legacy <- suppressWarnings(
      restrict_coef(
        model,
        restrictions
      )
    )
    testthat::expect_true(legacy$steps[[1]]$new_risk_factor)
    testthat::expect_s3_class(refit(legacy), "refitrestricted")
  }
)

testthat::test_that(
  "new restriction risk factors require data and a complete level mapping", {
    df <- data.frame(
      y = c(1, 2, 1, 3),
      exposure = rep(1, 4),
      postal_area = factor(c("A", "B", "A", "B")),
      hail_zone = factor(c("low", "high", "low", "high"))
    )
    model <- glm(
      y ~ postal_area + offset(log(exposure)),
      family = poisson(),
      data = df
    )
    ref <- prepare_refinement(model, data = df)

    testthat::expect_error(
      add_restriction(
        ref,
        data.frame(unknown_zone = "low", relativity = 1),
        allow_new_risk_factors = TRUE
      ),
      "Add a column assigning each observation to a level"
    )
    testthat::expect_error(
      add_restriction(
        ref,
        data.frame(hail_zone = "low", relativity = 1),
        allow_new_risk_factors = TRUE
      ),
      "Missing level\\(s\\): high"
    )
    testthat::expect_error(
      add_restriction(
        ref,
        data.frame(
          hail_zone = c("low", "high", "future"),
          relativity = c(1, 1.2, 1.3)
        ),
        allow_new_levels = FALSE,
        allow_new_risk_factors = TRUE
      ),
      "allow_new_levels = TRUE",
      fixed = TRUE
    )
    missing_factor <- ref
    missing_factor$base$data$hail_zone[1] <- NA
    testthat::expect_error(
      add_restriction(
        missing_factor,
        data.frame(
          hail_zone = c("low", "high"),
          relativity = c(1, 1.2)
        ),
        allow_new_risk_factors = TRUE
      ),
      "contains 1 missing value"
    )
    testthat::expect_error(
      add_restriction(
        ref,
        data.frame(
          hail_zone = c("low", "high"),
          relativity = c(1, 0)
        ),
        allow_new_risk_factors = TRUE
      ),
      "greater than zero"
    )
    testthat::expect_error(
      add_restriction(
        ref,
        data.frame(
          hail_zone = c("low", "high"),
          relativity = c(1, 1.2)
        ),
        allow_new_risk_factors = NA
      ),
      "allow_new_risk_factors"
    )
  }
)

testthat::test_that(
  "add_smoothing validates public arguments before fitting", {
    df <- data.frame(
      y = c(1, 2, 1, 3),
      exposure = rep(1, 4),
      age = c(20, 30, 40, 50)
    )
    df$age_band <- cut(df$age, breaks = c(20, 35, 50),
                       include.lowest = TRUE)
    model <- glm(y ~ age_band + offset(log(exposure)),
                 family = poisson(),
                 data = df)
    ref <- prepare_refinement(model, data = df)

    testthat::expect_error(
      add_smoothing(ref, model_variable = "age_band", source_variable = "age",
                    breaks = c(20, 35, 50),
                    smoothing = "bad"),
      "smoothing"
    )
    testthat::expect_error(
      add_smoothing(ref, model_variable = "age_band", source_variable = "age",
                    breaks = c(35, 20)),
      "strictly increasing"
    )
    testthat::expect_error(
      add_smoothing(ref, model_variable = "missing", source_variable = "age",
                    breaks = c(20, 35, 50)),
      "model_variable"
    )
    testthat::expect_error(
      add_smoothing(ref, model_variable = "age_band", source_variable = "age",
                    breaks = c(20, 35, 50),
                    degree = "2"),
      "degree"
    )
    testthat::expect_error(
      add_smoothing(ref, model_variable = "age_band", source_variable = "age",
                    breaks = c(20, 35, 50),
                    k = 1.5),
      "k"
    )
    testthat::expect_s3_class(
      add_smoothing(ref, model_variable = "age_band", source_variable = "age",
                    breaks = c(20, 35, 50)),
      "rating_refinement"
    )
    ref_with_missing <- ref
    ref_with_missing$base$data$age_band[2] <- NA
    testthat::expect_error(
      add_smoothing(
        ref_with_missing,
        model_variable = "age_band",
        source_variable = "age",
        breaks = c(20, 35, 50)
      ),
      "`model_variable` column `age_band` contains 1 missing value.*remove or impute"
    )
    testthat::expect_warning(
      add_smoothing(ref, tariff_class = "age_band", rating_variable = "age",
                    breaks = c(20, 35, 50)),
      "deprecated"
    )
    testthat::expect_warning(
      add_smoothing(ref, x_cut = "age_band", x_org = "age",
                    breaks = c(20, 35, 50)),
      "deprecated"
    )
    testthat::expect_error(
      add_smoothing(ref, model_variable = "age_band", x_cut = "age_band",
                    source_variable = "age", breaks = c(20, 35, 50)),
      "Use only one"
    )
  }
)

testthat::test_that(
  "add_smoothing requires interval-style model variable levels", {
    df <- data.frame(
      y = c(1, 2, 1, 3),
      exposure = rep(1, 4),
      age = c(20, 30, 40, 50),
      age_band = factor(c("young", "young", "old", "old"))
    )
    model <- glm(y ~ age_band + offset(log(exposure)),
                 family = poisson(),
                 data = df)
    ref <- prepare_refinement(model, data = df)

    testthat::expect_error(
      add_smoothing(ref, model_variable = "age_band", source_variable = "age",
                    breaks = c(20, 35, 50)),
      "interval-style"
    )
  }
)

testthat::test_that(
  "add_smoothing explains infeasible GAM basis dimensions", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4, 2, 3),
      exposure = rep(1, 8),
      insured_amount = c(50, 75, 125, 175, 225, 275, 325, 375)
    )
    df$insured_amount_band <- cut(
      df$insured_amount,
      breaks = c(0, 100, 200, 300, 400),
      include.lowest = TRUE
    )
    model <- glm(
      y ~ insured_amount_band + offset(log(exposure)),
      family = poisson(),
      data = df
    )
    ref <- prepare_refinement(model, data = df)

    error <- tryCatch(
      add_smoothing(
        ref,
        model_variable = "insured_amount_band",
        source_variable = "insured_amount",
        breaks = c(0, 100, 200, 300, 400),
        smoothing = "gam",
        k = 5
      ),
      error = identity
    )
    message <- conditionMessage(error)

    testthat::expect_match(
      message,
      "Cannot fit GAM smoothing for source variable `insured_amount`",
      fixed = TRUE
    )
    testthat::expect_match(message, "only 4 unique values")
    testthat::expect_match(message, "requires 5 degrees of freedom")
    testthat::expect_false(grepl(
      "fewer unique covariate combinations",
      message,
      fixed = TRUE
    ))

    testthat::expect_error(
      add_smoothing(
        ref,
        model_variable = "insured_amount_band",
        source_variable = "insured_amount",
        breaks = c(0, 100, 200, 300, 400),
        smoothing = "gam"
      ),
      "requires 10 degrees of freedom"
    )

    valid_refinement <- add_smoothing(
      ref,
      model_variable = "insured_amount_band",
      source_variable = "insured_amount",
      breaks = c(0, 100, 200, 300, 400),
      smoothing = "gam",
      k = 4
    )
    testthat::expect_s3_class(valid_refinement, "rating_refinement")
    smoothed_model <- refit(valid_refinement)
    testthat::expect_s3_class(smoothed_model, "glm")
    testthat::expect_identical(class(smoothed_model)[1], "refitsmooth")

    smoothing_output <- paste(
      capture.output(print(smoothed_model)),
      collapse = "\n"
    )
    testthat::expect_match(
      smoothing_output,
      "Smoothing: insured_amount_band from insured_amount"
    )
    testthat::expect_match(smoothing_output, "method: gam, k: 4")
    testthat::expect_match(smoothing_output, "Call:")

    testthat::expect_error(
      add_smoothing(
        ref,
        model_variable = "insured_amount_band",
        source_variable = "insured_amount",
        breaks = c(0, 100, 200, 300, 400),
        smoothing = "mpi",
        k = 5
      ),
      "Cannot fit shape-constrained `mpi` smoothing.*only 4 unique values"
    )

    testthat::expect_error(
      add_smoothing(
        ref,
        model_variable = "insured_amount_band",
        source_variable = "insured_amount",
        breaks = c(0, 100, 200, 300, 400),
        smoothing = "spline",
        degree = 4
      ),
      "polynomial degree 4 requires at least 5 unique values"
    )
  }
)

testthat::test_that(
  "autoplot can limit the visible smoothing range with x_max and y_max", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4, 2, 3),
      exposure = rep(1, 8),
      insured_amount = c(
        500000, 1000000, 2500000, 5000000,
        7500000, 10000000, 50000000, 100000000
      )
    )
    df$insured_amount_band <- cut(
      df$insured_amount,
      breaks = c(0, 2500000, 10000000, 50000000, 100000000),
      include.lowest = TRUE
    )
    model <- glm(
      y ~ insured_amount_band + offset(log(exposure)),
      family = poisson(),
      data = df
    )
    ref <- prepare_refinement(model, data = df) |>
      add_smoothing(
        model_variable = "insured_amount_band",
        source_variable = "insured_amount",
        breaks = c(0, 2500000, 5000000, 10000000, 50000000, 100000000),
        smoothing = "gam",
        k = 4
      )

    full_plot <- ggplot2::autoplot(ref)
    limited_plot <- ggplot2::autoplot(
      ref,
      x_max = 10000000,
      y_max = 1.5
    )

    testthat::expect_s3_class(limited_plot, "ggplot")
    testthat::expect_null(full_plot$coordinates$limits$x)
    testthat::expect_equal(
      limited_plot$coordinates$limits$x,
      c(NA_real_, 10000000)
    )
    testthat::expect_equal(
      limited_plot$coordinates$limits$y,
      c(NA_real_, 1.5)
    )
    testthat::expect_error(
      ggplot2::autoplot(ref, x_max = Inf),
      "x_max"
    )
    testthat::expect_error(
      ggplot2::autoplot(ref, y_max = NA_real_),
      "y_max"
    )

    restriction_ref <- prepare_refinement(model, data = df) |>
      add_restriction(data.frame(
        insured_amount_band = levels(df$insured_amount_band)[1],
        relativity = 1
      ))
    testthat::expect_error(
      ggplot2::autoplot(
        restriction_ref,
        x_max = 10000000,
        y_max = 1.5
      ),
      "only available when plotting a smoothing step"
    )
  }
)

testthat::test_that(
  "edit_smoothing stores edits with public argument names", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      age = c(20, 25, 35, 40, 50, 55)
    )
    df$age_band <- cut(df$age, breaks = c(18, 30, 45, 60),
                       include.lowest = TRUE)
    model <- glm(y ~ age_band + offset(log(exposure)),
                 family = poisson(),
                 data = df)

    ref <- prepare_refinement(model, data = df) |>
      add_smoothing(model_variable = "age_band",
                    source_variable = "age",
                    breaks = c(18, 30, 45, 60))

    edited <- edit_smoothing(
      ref,
      model_variable = "age_band",
      from = 30,
      to = 45,
      from_value = 1,
      to_value = 1.1,
      control_positions = c(37.5),
      control_values = c(1.05),
      extrapolation_step = 5
    )

    edit <- edited$steps[[1]]$edit
    testthat::expect_equal(edit$from, 30)
    testthat::expect_equal(edit$to, 45)
    testthat::expect_equal(edit$from_value, 1)
    testthat::expect_equal(edit$to_value, 1.1)
    testthat::expect_equal(edit$control_positions, c(37.5))
    testthat::expect_equal(edit$control_values, c(1.05))
    testthat::expect_s3_class(refit(edited), "glm")
  }
)

testthat::test_that(
  "edit_smoothing validates control point inputs", {
    df <- data.frame(
      y = c(1, 2, 1, 3),
      exposure = rep(1, 4),
      age = c(20, 30, 40, 50)
    )
    df$age_band <- cut(df$age, breaks = c(18, 30, 45, 60),
                       include.lowest = TRUE)
    model <- glm(y ~ age_band + offset(log(exposure)),
                 family = poisson(),
                 data = df)
    ref <- prepare_refinement(model, data = df) |>
      add_smoothing(model_variable = "age_band",
                    source_variable = "age",
                    breaks = c(18, 30, 45, 60))

    testthat::expect_error(
      edit_smoothing(ref, model_variable = "age_band", from = 45, to = 30),
      "'from' must be smaller"
    )
    testthat::expect_error(
      edit_smoothing(ref, model_variable = "age_band", from = 30, to = 45,
                     control_positions = c(35, 40),
                     control_values = c(1.1)),
      "same length"
    )
    testthat::expect_error(
      edit_smoothing(ref, model_variable = "age_band", from = 30, to = 45,
                     control_positions = c(50),
                     control_values = c(1.1)),
      "between 'from' and 'to'"
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
    rel <- relativities(
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

testthat::test_that(
  "add_relativities resolves earlier restrictions as its coefficient basis", {
    portfolio <- data.frame(
      claims = c(1, 2, 3, 4, 2, 3, 4, 5),
      exposure = rep(1, 8),
      industry_group = factor(rep(c("A", "B"), each = 4)),
      industry_detail = factor(c(
        "A1", "A2", "A1", "A2",
        "B1", "B2", "B1", "B2"
      ))
    )
    model <- glm(
      claims ~ industry_group + offset(log(exposure)),
      family = poisson(),
      data = portfolio
    )
    restrictions <- data.frame(
      industry_group = c("A", "B"),
      industry_group_restricted = c(0.8, 1.4)
    )
    rel <- relativities(
      split_level("A", c("A1", "A2"), c(0.9, 1.1))
    )

    unrestricted <- prepare_refinement(model, data = portfolio) |>
      add_relativities(
        model_variable = "industry_group",
        split_variable = "industry_detail",
        relativities = rel,
        exposure = "exposure",
        normalize = FALSE
      )
    unrestricted_preview <- preview_refinement(unrestricted, 1)

    restricted <- prepare_refinement(model, data = portfolio) |>
      add_restriction(restrictions) |>
      add_relativities(
        model_variable = "industry_group",
        split_variable = "industry_detail",
        relativities = rel,
        exposure = "exposure",
        normalize = FALSE
      )
    restricted_step <- restricted$steps[[2]]
    restricted_preview <- preview_refinement(restricted, 2)

    testthat::expect_identical(
      restricted_step$model_variable,
      "industry_group"
    )
    testthat::expect_identical(
      restricted_step$source_model_variable,
      "industry_group"
    )
    testthat::expect_identical(
      restricted_step$effective_model_variable,
      "industry_group_restricted"
    )
    testthat::expect_equal(
      unrestricted_preview$state$relativities_df$estimate,
      c(0.9, 1.1)
    )
    testthat::expect_equal(
      restricted_preview$state$relativities_df$estimate,
      c(0.72, 0.88)
    )
    testthat::expect_false(
      isTRUE(all.equal(
        unrestricted_preview$state$relativities_df$estimate,
        restricted_preview$state$relativities_df$estimate
      ))
    )
    testthat::expect_match(
      paste(deparse(restricted_preview$state$formula), collapse = " "),
      "log\\(industry_group_rel\\)"
    )
    testthat::expect_false(grepl(
      "log\\(industry_group_restricted\\)",
      paste(deparse(restricted_preview$state$formula), collapse = " ")
    ))

    fitted <- refit(restricted)
    tariff <- rating_table(fitted, exposure = FALSE)
    testthat::expect_setequal(
      setdiff(unique(tariff$df$risk_factor), "(Intercept)"),
      "industry_detail"
    )
    testthat::expect_false(
      "industry_group_restricted" %in% tariff$df$risk_factor
    )
    testthat::expect_equal(
      tariff$df$est_fitted[
        tariff$df$risk_factor == "industry_detail" &
          tariff$df$level == "A1"
      ],
      0.72
    )
    testthat::expect_equal(
      tariff$df$est_fitted[
        tariff$df$risk_factor == "industry_detail" &
          tariff$df$level == "A2"
      ],
      0.88
    )
  }
)

testthat::test_that(
  "add_relativities accepts an explicitly restricted variable once", {
    portfolio <- data.frame(
      claims = c(1, 2, 3, 4, 2, 3, 4, 5),
      exposure = rep(1, 8),
      portfolio_band = factor(rep(c("A", "B"), each = 4)),
      portfolio_detail = factor(c(
        "A1", "A2", "A1", "A2",
        "B1", "B2", "B1", "B2"
      ))
    )
    model <- glm(
      claims ~ portfolio_band + offset(log(exposure)),
      family = poisson(),
      data = portfolio
    )
    restrictions <- data.frame(
      portfolio_band = c("A", "B"),
      fixed_band_effect = c(0.8, 1.4)
    )
    rel <- relativities(
      split_level("A", c("A1", "A2"), c(0.9, 1.1))
    )

    refinement <- prepare_refinement(model, data = portfolio) |>
      add_restriction(restrictions) |>
      add_relativities(
        model_variable = "fixed_band_effect",
        split_variable = "portfolio_detail",
        relativities = rel,
        exposure = "exposure",
        normalize = FALSE
      )

    step <- refinement$steps[[2]]
    preview <- preview_refinement(refinement, 2)

    testthat::expect_identical(step$model_variable, "fixed_band_effect")
    testthat::expect_identical(
      step$source_model_variable,
      "portfolio_band"
    )
    testthat::expect_identical(
      step$effective_model_variable,
      "fixed_band_effect"
    )
    testthat::expect_equal(
      preview$state$relativities_df$estimate,
      c(0.72, 0.88)
    )
    testthat::expect_equal(
      lengths(regmatches(
        preview$state$offset,
        gregexpr("fixed_band_effect", preview$state$offset, fixed = TRUE)
      )),
      0
    )
  }
)

testthat::test_that(
  "only restrictions added before add_relativities affect its source", {
    portfolio <- data.frame(
      claims = c(1, 2, 3, 4, 2, 3, 4, 5),
      exposure = rep(1, 8),
      territory = factor(rep(c("A", "B"), each = 4)),
      territory_detail = factor(c(
        "A1", "A2", "A1", "A2",
        "B1", "B2", "B1", "B2"
      ))
    )
    model <- glm(
      claims ~ territory + offset(log(exposure)),
      family = poisson(),
      data = portfolio
    )
    restrictions <- data.frame(
      territory = c("A", "B"),
      territory_restricted = c(0.8, 1.4)
    )
    rel <- relativities(
      split_level("A", c("A1", "A2"), c(0.9, 1.1))
    )

    refinement <- prepare_refinement(model, data = portfolio) |>
      add_relativities(
        model_variable = "territory",
        split_variable = "territory_detail",
        relativities = rel,
        exposure = "exposure",
        normalize = FALSE
      ) |>
      add_restriction(restrictions)

    testthat::expect_identical(
      refinement$steps[[1]]$effective_model_variable,
      "territory"
    )
    testthat::expect_equal(
      preview_refinement(refinement, 1)$state$relativities_df$estimate,
      c(0.9, 1.1)
    )
  }
)
