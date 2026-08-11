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
  "relativity helpers define named parent-level splits", {
    residential <- split_level(
      "residential",
      new_levels = c("flat", "house"),
      relativities = c(0.95, 1.05)
    )
    commercial <- split_level(
      "commercial",
      new_levels = c("shop", "office"),
      relativities = c(1.10, 0.90)
    )

    specification <- relativities(residential, commercial)

    testthat::expect_named(specification, c("residential", "commercial"))
    testthat::expect_named(
      specification$residential,
      c("new_level", "relativity")
    )
    testthat::expect_warning(
      legacy <- split_relativities(
        new_levels = c("flat", "house"),
        relativities = c(0.95, 1.05)
      ),
      "deprecated"
    )
    testthat::expect_named(legacy, c("new_level", "relativity"))
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
    testthat::expect_error(
      prepare_refinement(mtcars),
      paste0(
        "`model` must be a fitted `glm` object, not a data frame. ",
        "Fit the model first"
      ),
      fixed = TRUE
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
  "intercept-only refit ignores exposure metadata on remaining factors", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4, 2, 3),
      exposure = rep(1, 8),
      group = factor(rep(c("low", "high"), each = 4)),
      zip = factor(rep(c("a", "b"), 4))
    )
    model <- glm(
      y ~ group + zip + offset(log(exposure)),
      family = poisson(),
      data = df
    )

    refinement <- prepare_refinement(model, data = df) |>
      add_restriction(data.frame(zip = "b", zip_rst = 1.1))

    refined <- refit(refinement, intercept_only = TRUE)

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
  "add_restriction updates stored levels and retains other restrictions", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      zip = factor(c("a", "b", "c", "a", "b", "c"))
    )
    model <- glm(
      y ~ zip + offset(log(exposure)),
      family = poisson(),
      data = df
    )

    refinement <- prepare_refinement(model, data = df) |>
      add_restriction(data.frame(
        zip = c("a", "c"),
        zip_restricted = c(0.9, 1.2)
      ))
    original_id <- refinement$steps[[1]]$id

    testthat::expect_message(
      refinement <- add_restriction(
        refinement,
        data.frame(zip = "a", zip_restricted = 1.1)
      ),
      "Updated existing restriction.*zip.*a"
    )

    stored <- refinement$steps[[1]]$restrictions
    testthat::expect_length(refinement$steps, 1)
    testthat::expect_identical(refinement$steps[[1]]$id, original_id)
    testthat::expect_equal(
      stored$zip_restricted[stored$zip == "a"],
      1.1
    )
    testthat::expect_equal(
      stored$zip_restricted[stored$zip == "c"],
      1.2
    )
    testthat::expect_setequal(
      refinement$steps[[1]]$supplied_levels,
      c("a", "c")
    )

    testthat::expect_error(
      add_restriction(
        refinement,
        data.frame(zip = "a", another_restriction = 1.05)
      ),
      "already has a restriction stored"
    )
  }
)

testthat::test_that(
  "restriction updates retain settings for a new expert risk factor", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      zip = factor(c("a", "b", "c", "a", "b", "c")),
      hail_zone = factor(c("low", "high", "low", "high", "low", "high"))
    )
    model <- glm(
      y ~ zip + offset(log(exposure)),
      family = poisson(),
      data = df
    )

    refinement <- prepare_refinement(model, data = df) |>
      add_restriction(
        data.frame(
          hail_zone = c("low", "high"),
          hail_relativity = c(1, 1.2)
        ),
        allow_new_risk_factors = TRUE
      )

    testthat::expect_message(
      refinement <- add_restriction(
        refinement,
        data.frame(hail_zone = "low", hail_relativity = 1.1)
      ),
      "Updated existing restriction.*hail_zone.*low"
    )

    stored <- refinement$steps[[1]]$restrictions
    testthat::expect_true(
      refinement$steps[[1]]$allow_new_risk_factors
    )
    testthat::expect_equal(
      stored$hail_relativity[stored$hail_zone == "low"],
      1.1
    )
    testthat::expect_equal(
      stored$hail_relativity[stored$hail_zone == "high"],
      1.2
    )
    testthat::expect_s3_class(refit(refinement), "refitrestricted")
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

    testthat::expect_message(
      refined <- add_restriction(ref, restrictions),
      paste0(
        "Added new level `D` to risk factor `postal_area` with relativity ",
        "1.2. This level was not observed in the model data."
      ),
      fixed = TRUE
    )
    completed <- refined$steps[[1]]$restrictions

    printed <- testthat::capture_output(print(refined))
    testthat::expect_match(printed, "Base model: Poisson GLM \\(log link\\)")
    testthat::expect_match(
      printed,
      "Restriction: postal_area -> relativity \\(4 levels\\).*new level: D"
    )

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
    testthat::expect_error(
      add_restriction(ref, data.frame(zip = "a", zip_rst = "1")),
      paste0(
        "The relativity column `zip_rst` must be numeric, but it is ",
        "character. Supply relativities as numeric values, for example `1` ",
        "instead of `\"1\"`."
      ),
      fixed = TRUE
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
  "a new restricted risk factor can replace an existing model term", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4, 2, 3),
      exposure = rep(1, 8),
      postal_area = factor(rep(c("A", "B"), 4)),
      vehicle_group = factor(rep(c("small", "large"), each = 4)),
      hail_zone = factor(rep(c("low", "high"), each = 2, times = 2))
    )
    model <- glm(
      y ~ postal_area + vehicle_group + offset(log(exposure)),
      family = poisson(),
      data = df
    )
    restrictions <- data.frame(
      hail_zone = c("low", "high"),
      hail_relativity = c(0.9, 1.2)
    )

    refinement <- prepare_refinement(model, data = df) |>
      add_restriction(restrictions, replaces = "postal_area")

    testthat::expect_identical(refinement$steps[[1]]$replaces, "postal_area")
    testthat::expect_true(refinement$steps[[1]]$new_risk_factor)
    testthat::expect_true(refinement$steps[[1]]$allow_new_risk_factors)
    testthat::expect_match(
      refinement |> summary() |> capture.output() |> paste(collapse = "\n"),
      "replaces postal_area"
    )

    fitted <- refit(refinement)
    fitted_terms <- attr(stats::terms(fitted), "term.labels")
    testthat::expect_false("postal_area" %in% fitted_terms)
    testthat::expect_true("vehicle_group" %in% fitted_terms)
    testthat::expect_match(
      paste(deparse(stats::formula(fitted)), collapse = ""),
      "offset\\(log\\(hail_relativity\\) \\+ log\\(exposure\\)\\)"
    )
    testthat::expect_false(any(grepl("postal_area", names(stats::coef(fitted)))))

    tariff <- as.data.frame(rating_table(fitted, exposure = FALSE))
    testthat::expect_true("hail_relativity" %in% tariff$risk_factor)
    testthat::expect_false("postal_area" %in% tariff$risk_factor)

    refinement <- add_restriction(
      refinement,
      data.frame(hail_zone = "low", hail_relativity = 1)
    )
    testthat::expect_identical(refinement$steps[[1]]$replaces, "postal_area")
    testthat::expect_s3_class(refit(refinement), "refitrestricted")
  }
)

testthat::test_that(
  "replacement restrictions validate the model term explicitly", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4, 2, 3),
      exposure = rep(1, 8),
      postal_area = factor(rep(c("A", "B"), 4)),
      vehicle_group = factor(rep(c("small", "large"), each = 4)),
      hail_zone = factor(rep(c("low", "high"), each = 2, times = 2))
    )
    restrictions <- data.frame(
      hail_zone = c("low", "high"),
      hail_relativity = c(0.9, 1.2)
    )
    model <- glm(
      y ~ postal_area + vehicle_group + offset(log(exposure)),
      family = poisson(),
      data = df
    )
    refinement <- prepare_refinement(model, data = df)

    testthat::expect_error(
      add_restriction(refinement, restrictions, replaces = "postal_are"),
      "Did you mean `postal_area`?",
      fixed = TRUE
    )
    testthat::expect_error(
      add_restriction(
        refinement,
        restrictions,
        allow_new_risk_factors = FALSE,
        replaces = "postal_area"
      ),
      "conflicts with `allow_new_risk_factors = FALSE`",
      fixed = TRUE
    )
    testthat::expect_error(
      add_restriction(
        refinement,
        data.frame(postal_area = c("A", "B"), postal_fixed = c(1, 1.1)),
        replaces = "vehicle_group"
      ),
      "can only be used when `postal_area` is a new fixed risk factor",
      fixed = TRUE
    )

    interaction_model <- glm(
      y ~ postal_area * vehicle_group + offset(log(exposure)),
      family = poisson(),
      data = df
    )
    testthat::expect_error(
      prepare_refinement(interaction_model, data = df) |>
        add_restriction(restrictions, replaces = "postal_area"),
      "also occurs in interaction term"
    )
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
    testthat::expect_identical(
      names(formals(add_smoothing))[1:8],
      c(
        "model", "model_variable", "source_variable", "breaks", "smoothing",
        "k", "degree", "weights"
      )
    )

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
      add_smoothing(
        ref,
        model_variable = "age_band",
        source_variable = "age"
      ),
      "`breaks` is required",
      fixed = TRUE
    )

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
      add_smoothing(ref, model_variable = "age_bnd", source_variable = "age",
                    breaks = c(20, 35, 50)),
      paste0(
        "Variable `age_bnd`, supplied through `model_variable`, is not a ",
        "model term in the GLM used by `prepare_refinement()`. Did you mean ",
        "`age_band`?"
      ),
      fixed = TRUE
    )
    testthat::expect_error(
      add_smoothing(ref, model_variable = "age", source_variable = "age",
                    breaks = c(20, 35, 50)),
      paste0(
        "Variable `age`, supplied through `model_variable`, is not a model ",
        "term in the GLM used by `prepare_refinement()`."
      ),
      fixed = TRUE
    )
    testthat::expect_error(
      add_smoothing(ref, model_variable = "age_band", source_variable = "agee",
                    breaks = c(20, 35, 50)),
      paste0(
        "Column `agee`, supplied through `source_variable`, was not found ",
        "in the refinement data. Did you mean `age`?"
      ),
      fixed = TRUE
    )
    testthat::expect_error(
      add_smoothing(
        ref,
        model_variable = "age_band",
        source_variable = "age",
        breaks = c(20, 35, 50),
        weights = "exposure1"
      ),
      paste0(
        "Column `exposure1`, supplied through `weights`, was not found in ",
        "the refinement data. Did you mean `exposure`?"
      ),
      fixed = TRUE
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
                    breaks = c(20, 35, 50),
                    smoothing = "poly", degree = 1),
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
                    breaks = c(20, 35, 50),
                    smoothing = "poly", degree = 1),
      "deprecated"
    )
    testthat::expect_warning(
      add_smoothing(ref, x_cut = "age_band", x_org = "age",
                    breaks = c(20, 35, 50),
                    smoothing = "poly", degree = 1),
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
  "add_smoothing validates source values and break coverage", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      age = c(20, 25, 30, 35, 40, 50)
    )
    df$age_band <- cut(
      df$age,
      breaks = c(20, 30, 40, 50),
      include.lowest = TRUE
    )
    model <- glm(
      y ~ age_band + offset(log(exposure)),
      family = poisson(),
      data = df
    )
    ref <- prepare_refinement(model, data = df)

    missing_source <- ref
    missing_source$base$data$age[2] <- NA_real_
    testthat::expect_error(
      add_smoothing(
        missing_source,
        model_variable = "age_band",
        source_variable = "age",
        breaks = c(20, 30, 40, 50),
        smoothing = "poly",
        degree = 1
      ),
      "`source_variable` column `age` contains 1 missing value.*finite numeric values"
    )

    infinite_source <- ref
    infinite_source$base$data$age[2] <- Inf
    testthat::expect_error(
      add_smoothing(
        infinite_source,
        model_variable = "age_band",
        source_variable = "age",
        breaks = c(20, 30, 40, 50),
        smoothing = "poly",
        degree = 1
      ),
      "`source_variable` column `age` contains 1 non-finite value.*`Inf` or `-Inf`"
    )

    testthat::expect_error(
      add_smoothing(
        ref,
        model_variable = "age_band",
        source_variable = "age",
        breaks = c(25, 30, 40, 45),
        smoothing = "poly",
        degree = 1
      ),
      paste0(
        "`breaks` do not cover all values in the `source_variable` column ",
        "`age`.*1 below the first break and 1 above the last break"
      )
    )

    testthat::expect_warning(
      add_smoothing(
        ref,
        model_variable = "age_band",
        source_variable = "age",
        breaks = c(15, 30, 40, 55),
        smoothing = "poly",
        degree = 1
      ),
      paste0(
        "The supplied `breaks` extend beyond the fitted GLM range ",
        "\\(20\u201350\\).*based on extrapolation rather than observed ",
        "model estimates.*Use `edit_smoothing\\(\\)`"
      )
    )

    testthat::expect_warning(
      add_smoothing(
        ref,
        model_variable = "age_band",
        source_variable = "age",
        breaks = c(20, 30, 40, 50),
        smoothing = "poly",
        degree = 1
      ),
      NA
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

    default_refinement <- add_smoothing(
      ref,
      model_variable = "insured_amount_band",
      source_variable = "insured_amount",
      breaks = c(0, 100, 200, 300, 400)
    )
    testthat::expect_identical(
      default_refinement$steps[[1]]$smoothing,
      "spline"
    )
    testthat::expect_identical(default_refinement$steps[[1]]$k, 4L)

    polynomial_refinement <- add_smoothing(
      ref,
      model_variable = "insured_amount_band",
      source_variable = "insured_amount",
      breaks = c(0, 100, 200, 300, 400),
      smoothing = "poly",
      degree = 2
    )
    testthat::expect_identical(
      polynomial_refinement$steps[[1]]$smoothing,
      "poly"
    )

    testthat::expect_error(
      add_smoothing(
        ref,
        model_variable = "insured_amount_band",
        source_variable = "insured_amount",
        breaks = c(0, 100, 200, 300, 400),
        smoothing = "spline",
        degree = 2
      ),
      "degree.*only used.*poly"
    )
    testthat::expect_error(
      add_smoothing(
        ref,
        model_variable = "insured_amount_band",
        source_variable = "insured_amount",
        breaks = c(0, 100, 200, 300, 400),
        smoothing = "poly",
        k = 4
      ),
      "k.*only used"
    )
    testthat::expect_error(
      add_smoothing(
        ref,
        model_variable = "insured_amount_band",
        source_variable = "insured_amount",
        breaks = c(0, 100, 200, 300, 400),
        smoothing = "spline",
        k = 2
      ),
      "k.*at least 3"
    )

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

    automatic_k <- add_smoothing(
      ref,
      model_variable = "insured_amount_band",
      source_variable = "insured_amount",
      breaks = c(0, 100, 200, 300, 400),
      smoothing = "gam"
    )
    testthat::expect_identical(automatic_k$steps[[1]]$k, 4L)

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
        smoothing = "increasing",
        k = 5
      ),
      "Cannot fit shape-constrained `increasing` smoothing.*only 4 unique values"
    )

    readable_method <- add_smoothing(
      ref,
      model_variable = "insured_amount_band",
      source_variable = "insured_amount",
      breaks = c(0, 100, 200, 300, 400),
      smoothing = "increasing",
      k = 4
    )
    legacy_alias <- add_smoothing(
      ref,
      model_variable = "insured_amount_band",
      source_variable = "insured_amount",
      breaks = c(0, 100, 200, 300, 400),
      smoothing = "mpi",
      k = 4
    )

    testthat::expect_identical(
      readable_method$steps[[1]]$smoothing,
      "increasing"
    )
    testthat::expect_identical(
      legacy_alias$steps[[1]]$smoothing,
      "increasing"
    )
    testthat::expect_identical(
      readable_method$steps[[1]]$smoothing_code,
      "mpi"
    )
    testthat::expect_identical(
      legacy_alias$steps[[1]]$smoothing_code,
      "mpi"
    )
    testthat::expect_match(
      paste(capture.output(print(legacy_alias)), collapse = "\n"),
      "method: increasing, k: 4"
    )

    saved_legacy_object <- legacy_alias
    saved_legacy_object$steps[[1]]$smoothing <- "mpi"
    saved_legacy_object$steps[[1]]$smoothing_code <- NULL
    readable_fit <- refit(readable_method)
    saved_legacy_fit <- refit(saved_legacy_object)
    testthat::expect_equal(
      stats::fitted(readable_fit),
      stats::fitted(saved_legacy_fit)
    )

    testthat::expect_error(
      add_smoothing(
        ref,
        model_variable = "insured_amount_band",
        source_variable = "insured_amount",
        breaks = c(0, 100, 200, 300, 400),
        smoothing = "poly",
        degree = 4
      ),
      "polynomial degree 4 requires at least 5 unique values"
    )
  }
)

testthat::test_that("readable smoothing methods retain all legacy aliases", {
  aliases <- c(
    mpi = "increasing",
    mpd = "decreasing",
    cx = "convex",
    cv = "concave",
    micx = "increasing_convex",
    micv = "increasing_concave",
    mdcx = "decreasing_convex",
    mdcv = "decreasing_concave"
  )

  resolved <- vapply(
    names(aliases),
    function(method) .resolve_smoothing_method(method)$method,
    character(1)
  )
  codes <- vapply(
    unname(aliases),
    function(method) .resolve_smoothing_method(method)$code,
    character(1)
  )

  testthat::expect_identical(unname(resolved), unname(aliases))
  testthat::expect_identical(unname(codes), names(aliases))
})

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

    testthat::expect_length(edited$steps, 2L)
    testthat::expect_identical(edited$steps[[1]]$type, "smoothing")
    testthat::expect_identical(edited$steps[[2]]$type, "smoothing_edit")
    testthat::expect_identical(
      edited$steps[[2]]$smoothing_step_id,
      edited$steps[[1]]$id
    )
    edit <- edited$steps[[2]]$edit
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

testthat::test_that("the former global strength argument is absent", {
  old_argument <- paste0("effect", "_strength")
  testthat::expect_false(old_argument %in% names(formals(add_smoothing)))
  testthat::expect_false(old_argument %in% names(formals(edit_smoothing)))
})

testthat::test_that(
  "relative smoothing adjustments are stored and inherit the method", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = c(1, 2, 1, 3, 1, 4),
      age = c(20, 25, 35, 40, 50, 55)
    )
    df$age_band <- cut(
      df$age,
      breaks = c(18, 30, 45, 60),
      include.lowest = TRUE
    )
    model <- glm(
      y ~ age_band + offset(log(exposure)),
      family = poisson(),
      data = df
    )
    base <- prepare_refinement(model, data = df) |>
      add_smoothing(
        model_variable = "age_band",
        source_variable = "age",
        breaks = c(18, 30, 45, 60),
        smoothing = "poly",
        degree = 1,
        weights = "exposure"
      )

    edited <- edit_smoothing(
      base,
      model_variable = "age_band",
      from = 18,
      to = 60,
      adjustment = 1.05
    )
    base_state <- preview_refinement(base, 1)$state
    state <- preview_refinement(edited, 2)$state

    testthat::expect_equal(edited$steps[[2]]$edit$adjustment, 1.05)
    testthat::expect_null(edited$steps[[2]]$edit$transition)
    testthat::expect_identical(state$transition, "poly")
    testthat::expect_true(any(state$new$yhat > base_state$new$yhat))
    boundary_rows <- c(1L, nrow(state$new_line))
    testthat::expect_equal(
      state$new_line$yhat[boundary_rows],
      base_state$new_line$yhat[boundary_rows]
    )

    upper_tail <- edit_smoothing(
      base,
      model_variable = "age_band",
      from = 30,
      adjustment = 1.05,
      transition = "linear"
    )
    upper_state <- preview_refinement(upper_tail, 2)$state
    testthat::expect_null(upper_tail$steps[[2]]$edit$to)
    testthat::expect_equal(
      upper_state$new_line$yhat[upper_state$new_line$age == 30],
      base_state$new_line$yhat[base_state$new_line$age == 30]
    )
    testthat::expect_true(
      tail(upper_state$new_line$yhat, 1) > tail(base_state$new_line$yhat, 1)
    )
  }
)

testthat::test_that(
  "smoothing edits are separate cumulative steps with an initial overlay", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = c(1, 2, 1, 3, 1, 4),
      age = c(20, 25, 35, 40, 50, 55)
    )
    df$age_band <- cut(
      df$age,
      breaks = c(18, 30, 45, 60),
      include.lowest = TRUE
    )
    model <- glm(
      y ~ age_band + offset(log(exposure)),
      family = poisson(),
      data = df
    )
    refinement <- prepare_refinement(model, data = df) |>
      add_smoothing(
        model_variable = "age_band",
        source_variable = "age",
        breaks = c(18, 30, 45, 60),
        smoothing = "poly",
        degree = 1,
        weights = "exposure"
      ) |>
      edit_smoothing(
        model_variable = "age_band",
        from = 18,
        to = 60,
        adjustment = 1.05,
        transition = "linear"
      ) |>
      edit_smoothing(
        model_variable = "age_band",
        from = 18,
        to = 60,
        adjustment = 1.03,
        transition = "linear"
      )

    testthat::expect_identical(
      vapply(refinement$steps, `[[`, character(1), "type"),
      c("smoothing", "smoothing_edit", "smoothing_edit")
    )

    initial <- preview_refinement(refinement, 1)$state$new_line$yhat
    first_edit <- preview_refinement(refinement, 2)$state$new_line$yhat
    second_edit <- preview_refinement(refinement, 3)$state$new_line$yhat
    testthat::expect_true(any(first_edit > initial))
    testthat::expect_true(any(second_edit > first_edit))
    testthat::expect_equal(second_edit[c(1L, length(second_edit))],
                           initial[c(1L, length(initial))])

    plot <- ggplot2::autoplot(
      refinement,
      step = 3,
      show_initial_smoothing = TRUE
    )
    overlay_layers <- which(vapply(
      plot$layers,
      function(layer) "curve" %in% names(layer$data),
      logical(1)
    ))
    testthat::expect_length(overlay_layers, 1L)
    testthat::expect_setequal(
      unique(plot$layers[[overlay_layers]]$data$curve),
      c("Initial smoothing", "Current smoothing")
    )
    testthat::expect_equal(
      plot$layers[[overlay_layers]]$data$yhat[
        plot$layers[[overlay_layers]]$data$curve == "Current smoothing"
      ],
      second_edit
    )

    first_plot <- ggplot2::autoplot(
      refinement,
      step = 2,
      show_initial_smoothing = TRUE
    )
    first_overlay <- which(vapply(
      first_plot$layers,
      function(layer) "curve" %in% names(layer$data),
      logical(1)
    ))
    testthat::expect_equal(
      first_plot$layers[[first_overlay]]$data$yhat[
        first_plot$layers[[first_overlay]]$data$curve == "Current smoothing"
      ],
      first_edit
    )

    plain_plot <- ggplot2::autoplot(refinement, step = 3)
    testthat::expect_false(any(vapply(
      plain_plot$layers,
      function(layer) "curve" %in% names(layer$data),
      logical(1)
    )))

    latest_plot <- ggplot2::autoplot(
      refinement,
      variable = "age_band",
      show_initial_smoothing = TRUE
    )
    testthat::expect_s3_class(latest_plot, "ggplot")
    testthat::expect_s3_class(refit(refinement), "glm")
  }
)

testthat::test_that(
  "linear relative adjustments are continuous and local", {
    smooth <- data.frame(
      age = c(25, 30, 35, 40, 45, 50, 55),
      yhat = rep(1, 7)
    )
    line <- smooth
    new_rf <- data.frame(yhat = smooth$yhat)

    increased <- .apply_smoothing_adjustment(
      smooth = smooth,
      line = line,
      new_rf = new_rf,
      source_variable = "age",
      from = 30,
      to = 50,
      adjustment = 1.05,
      transition = "linear",
      original_smoothing = "spline"
    )
    testthat::expect_equal(
      increased$smooth$yhat,
      c(1, 1, 1.025, 1.05, 1.025, 1, 1)
    )
    testthat::expect_equal(increased$line$yhat[c(2, 6)], c(1, 1))

    decreased <- .apply_smoothing_adjustment(
      smooth = smooth,
      line = line,
      new_rf = new_rf,
      source_variable = "age",
      from = 30,
      to = 50,
      adjustment = 0.95,
      transition = "linear",
      original_smoothing = "spline"
    )
    testthat::expect_equal(
      decreased$smooth$yhat,
      c(1, 1, 0.975, 0.95, 0.975, 1, 1)
    )
  }
)

testthat::test_that(
  "one-sided relative adjustments use the available smoothing range", {
    smooth <- data.frame(
      age = c(20, 30, 40, 50, 60),
      yhat = rep(1, 5)
    )
    new_rf <- data.frame(yhat = smooth$yhat)

    upper_tail <- .apply_smoothing_adjustment(
      smooth = smooth,
      line = smooth,
      new_rf = new_rf,
      source_variable = "age",
      from = 30,
      to = NULL,
      adjustment = 1.05,
      transition = "linear",
      original_smoothing = "spline"
    )
    testthat::expect_equal(
      upper_tail$smooth$yhat,
      c(1, 1, 1 + 0.05 / 3, 1 + 0.10 / 3, 1.05)
    )
    testthat::expect_equal(upper_tail$smooth$yhat[2], smooth$yhat[2])

    lower_tail <- .apply_smoothing_adjustment(
      smooth = smooth,
      line = smooth,
      new_rf = new_rf,
      source_variable = "age",
      from = NULL,
      to = 50,
      adjustment = 0.95,
      transition = "linear",
      original_smoothing = "spline"
    )
    testthat::expect_equal(
      lower_tail$smooth$yhat,
      c(0.95, 1 - 0.10 / 3, 1 - 0.05 / 3, 1, 1)
    )
    testthat::expect_equal(lower_tail$smooth$yhat[4], smooth$yhat[4])
  }
)

testthat::test_that("step adjustments explicitly allow boundary jumps", {
  smooth <- data.frame(age = c(25, 30, 35, 40, 45, 50, 55), yhat = 1)
  changed <- .apply_smoothing_adjustment(
    smooth = smooth,
    line = smooth,
    new_rf = data.frame(yhat = smooth$yhat),
    source_variable = "age",
    from = 30,
    to = 50,
    adjustment = 1.05,
    transition = "step",
    original_smoothing = "spline"
  )

  testthat::expect_equal(
    changed$smooth$yhat,
    c(1, 1.05, 1.05, 1.05, 1.05, 1.05, 1)
  )
})

testthat::test_that(
  "inherited constrained transitions preserve feasible structure", {
    smooth <- data.frame(
      age = c(30, 35, 40, 45, 50),
      yhat = c(0.8, 1.0, 1.15, 1.25, 1.3)
    )
    changed <- .apply_smoothing_adjustment(
      smooth = smooth,
      line = smooth,
      new_rf = data.frame(yhat = smooth$yhat),
      source_variable = "age",
      from = 30,
      to = 50,
      adjustment = 1.05,
      transition = NULL,
      original_smoothing = "increasing_concave"
    )

    slopes <- diff(changed$line$yhat) / diff(changed$line$age)
    testthat::expect_true(all(slopes >= 0))
    testthat::expect_true(all(diff(slopes) <= 0))
    testthat::expect_identical(changed$transition, "increasing_concave")
    profile <- changed$smooth$yhat / smooth$yhat
    testthat::expect_true(all(diff(profile[1:3]) >= 0))
    testthat::expect_true(all(diff(profile[3:5]) <= 0))
  }
)

testthat::test_that(
  "infeasible inherited shape adjustments return an actionable error", {
    smooth <- data.frame(
      age = c(30, 35, 40, 45, 50),
      yhat = c(1, 1.01, 1.02, 1.03, 1.04)
    )
    testthat::expect_error(
      .apply_smoothing_adjustment(
        smooth = smooth,
        line = smooth,
        new_rf = data.frame(yhat = smooth$yhat),
        source_variable = "age",
        from = 30,
        to = 50,
        adjustment = 1.20,
        transition = NULL,
        original_smoothing = "increasing"
      ),
      "violate the inherited `increasing` smoothing structure"
    )
  }
)

testthat::test_that(
  "relative adjustments reject ambiguous edit combinations", {
    df <- data.frame(
      y = c(1, 2, 1, 3, 2, 4),
      exposure = rep(1, 6),
      age = c(20, 25, 35, 40, 50, 55)
    )
    df$age_band <- cut(df$age, c(18, 30, 45, 60), include.lowest = TRUE)
    model <- glm(
      y ~ age_band + offset(log(exposure)),
      family = poisson(),
      data = df
    )
    ref <- prepare_refinement(model, data = df) |>
      add_smoothing(
        model_variable = "age_band",
        source_variable = "age",
        breaks = c(18, 30, 45, 60),
        smoothing = "poly",
        degree = 1
      )

    testthat::expect_error(
      edit_smoothing(
        ref, model_variable = "age_band", from = 18, to = 60,
        adjustment = 1.05, from_value = 1
      ),
      "cannot be combined"
    )
    testthat::expect_error(
      edit_smoothing(
        ref, model_variable = "age_band", from = 30, from_value = 1.05
      ),
      "Explicit target-value edits require both `from` and `to`"
    )
    testthat::expect_error(
      edit_smoothing(
        ref, model_variable = "age_band", from = 18, to = 60,
        transition = "linear", from_value = 1
      ),
      "only used with `adjustment`"
    )
    testthat::expect_error(
      edit_smoothing(
        ref, model_variable = "age_band", from = 18, to = 60,
        adjustment = 1.05, allow_extrapolation = TRUE
      ),
      "only available for explicit"
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
      add_relativities(
        ref,
        model_variable = "zip",
        split_variable = "zip_split",
        relativities = rel,
        exposure = "exposure",
        output_variable = "zip_split"
      ),
      "already exists in the refinement data"
    )
    testthat::expect_error(
      add_relativities(
        ref,
        model_variable = "zip",
        split_variable = "zip_split",
        relativities = rel,
        exposure = "exposure",
        output_variable = ""
      ),
      "output_variable"
    )
    testthat::expect_error(
      add_relativities(ref, model_variable = "zip", split_variable = "missing",
                       relativities = rel, exposure = "exposure"),
      "split_variable"
    )
    testthat::expect_error(
      add_relativities(
        ref,
        model_variable = "zipp",
        split_variable = "zip_split",
        relativities = rel,
        exposure = "exposure"
      ),
      paste0(
        "Column `zipp`, supplied through `model_variable`, was not found in ",
        "the refinement data and does not identify a restricted variable ",
        "created by an earlier `add_restriction()` step. Did you mean `zip`?"
      ),
      fixed = TRUE
    )
    testthat::expect_error(
      add_relativities(
        ref,
        model_variable = "zip",
        split_variable = "zip_splitt",
        relativities = rel,
        exposure = "exposure"
      ),
      paste0(
        "Column `zip_splitt`, supplied through `split_variable`, was not ",
        "found in the refinement data. Did you mean `zip_split`?"
      ),
      fixed = TRUE
    )
    testthat::expect_error(
      add_relativities(
        ref,
        model_variable = "zip",
        split_variable = "zip_split",
        relativities = rel,
        exposure = "exposure2"
      ),
      paste0(
        "Column `exposure2`, supplied through `exposure`, was not found in ",
        "the refinement data. Did you mean `exposure`?"
      ),
      fixed = TRUE
    )
    misspelled <- relativities(
      split_level("a", c("a1", "a 2"), c(1, 1.2))
    )
    testthat::expect_error(
      add_relativities(
        ref,
        model_variable = "zip",
        split_variable = "zip_split",
        relativities = misspelled,
        exposure = "exposure"
      ),
      "`a 2`.*Did you mean `a2`",
      perl = TRUE
    )
    misspelled_parent <- relativities(
      split_level("aa", c("a1", "a2"), c(1, 1.2))
    )
    testthat::expect_error(
      add_relativities(
        ref,
        model_variable = "zip",
        split_variable = "zip_split",
        relativities = misspelled_parent,
        exposure = "exposure"
      ),
      "(?s)category.*`aa`.*Did you mean `a`",
      perl = TRUE
    )
    wrong_parent <- relativities(
      split_level("a", c("a1", "b1"), c(1, 1.2))
    )
    testthat::expect_error(
      add_relativities(
        ref,
        model_variable = "zip",
        split_variable = "zip_split",
        relativities = wrong_parent,
        exposure = "exposure"
      ),
      "do not occur within their specified `model_variable` levels"
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
    testthat::expect_identical(
      restricted_step$output_variable,
      "industry_group_refined"
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
      "industry_group_refined"
    )
    testthat::expect_false(
      "industry_group_restricted" %in% tariff$df$risk_factor
    )
    testthat::expect_equal(
      tariff$df$est_fitted[
        tariff$df$risk_factor == "industry_group_refined" &
          tariff$df$level == "A1"
      ],
      0.72
    )
    testthat::expect_equal(
      tariff$df$est_fitted[
        tariff$df$risk_factor == "industry_group_refined" &
          tariff$df$level == "A2"
      ],
      0.88
    )
  }
)

testthat::test_that(
  "restriction updates retain step order for later relativities", {
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
    rel <- relativities(
      split_level("A", c("A1", "A2"), c(0.9, 1.1))
    )

    refinement <- prepare_refinement(model, data = portfolio) |>
      add_restriction(data.frame(
        industry_group = c("A", "B"),
        industry_group_restricted = c(0.8, 1.4)
      )) |>
      add_relativities(
        model_variable = "industry_group",
        split_variable = "industry_detail",
        relativities = rel,
        exposure = "exposure",
        normalize = FALSE
      )

    testthat::expect_message(
      refinement <- add_restriction(
        refinement,
        data.frame(
          industry_group = "A",
          industry_group_restricted = 0.9
        )
      ),
      "Updated existing restriction"
    )

    preview <- preview_refinement(refinement, 2)
    testthat::expect_identical(
      vapply(refinement$steps, `[[`, character(1), "type"),
      c("restriction", "relativities")
    )
    testthat::expect_equal(
      preview$state$relativities_df$estimate,
      c(0.81, 0.99)
    )

    fitted <- refit(refinement)
    tariff <- rating_table(fitted, exposure = FALSE)$df
    testthat::expect_equal(
      tariff$est_fitted[
        tariff$risk_factor == "industry_group_refined" &
          tariff$level == "A1"
      ],
      0.81
    )
    testthat::expect_equal(
      tariff$est_fitted[
        tariff$risk_factor == "industry_group_refined" &
          tariff$level == "A2"
      ],
      0.99
    )
  }
)

testthat::test_that(
  "add_restriction recognises a factor derived by add_relativities", {
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
    rel <- relativities(
      split_level("A", c("A1", "A2"), c(0.9, 1.1))
    )

    refinement <- prepare_refinement(model, data = portfolio) |>
      add_relativities(
        model_variable = "industry_group",
        split_variable = "industry_detail",
        relativities = rel,
        exposure = "exposure",
        normalize = FALSE
      ) |>
      add_restriction(data.frame(
        industry_group_refined = "A1",
        industry_group_refined_restricted = 0.75
      ))

    restriction_step <- refinement$steps[[2]]
    stored <- restriction_step$restrictions

    testthat::expect_false(restriction_step$new_risk_factor)
    testthat::expect_false(restriction_step$allow_new_risk_factors)
    testthat::expect_true(restriction_step$replace_refinement_offset)
    testthat::expect_identical(
      restriction_step$model_term,
      "industry_group_rel"
    )
    testthat::expect_equal(
      stored$industry_group_refined_restricted[
        stored$industry_group_refined == "A1"
      ],
      0.75
    )
    testthat::expect_equal(
      stored$industry_group_refined_restricted[
        stored$industry_group_refined == "A2"
      ],
      1.1
    )
    testthat::expect_equal(
      stored$industry_group_refined_restricted[
        stored$industry_group_refined == "B"
      ],
      1.4
    )

    fitted <- refit(refinement)
    formula_text <- paste(deparse(stats::formula(fitted)), collapse = " ")
    tariff <- rating_table(fitted, exposure = FALSE)$df

    testthat::expect_match(
      formula_text,
      "log\\(industry_group_refined_restricted\\)"
    )
    testthat::expect_false(grepl(
      "log\\(industry_group_rel\\)",
      formula_text
    ))
    testthat::expect_equal(
      tariff$est_fitted[
        tariff$risk_factor == "industry_group_refined_restricted" &
          tariff$level == "A1"
      ],
      0.75
    )
    testthat::expect_equal(
      tariff$est_fitted[
        tariff$risk_factor == "industry_group_refined_restricted" &
          tariff$level == "A2"
      ],
      1.1
    )
    testthat::expect_equal(
      tariff$est_fitted[
        tariff$risk_factor == "industry_group_refined_restricted" &
          tariff$level == "B"
      ],
      1.4
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
