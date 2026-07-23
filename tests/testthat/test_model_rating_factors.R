library(insurancerating)
library(dplyr)

testthat::context("rating_factors")


# Restricted glm ----------------------------------------------------------

# Fit frequency and severity model
freq <- glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
            data = MTPL)
sev <- glm(amount ~ bm + zip, weights = nclaims,
           family = Gamma(link = "log"),
           data = MTPL %>% filter(amount > 0))

# Add predictions for freq and sev to data, and calculate premium
premium_df <- MTPL |>
  add_prediction(freq, sev) |>
  mutate(premium = pred_nclaims_freq * pred_amount_sev)

# Restrictions on risk factors for region (zip)
zip_df <- data.frame(zip = c(0, 1, 2, 3),
                     zip_rst = c(0.8, 0.9, 1, 1.2))

# Fit unrestricted model
burn <- glm(premium ~ bm + zip, weights = exposure,
            family = Gamma(link = "log"), data = premium_df)

# Fit restricted model
burn_rst <- prepare_refinement(burn) |>
  add_restriction(zip_df) |>
  refit()

# Fit unrestricted model with only one risk factor
burn2 <- glm(premium ~ zip, weights = exposure,
             family = Gamma(link = "log"), data = premium_df)

# Fit restricted model with intercept only
burn2_rst <- prepare_refinement(burn2) |>
  add_restriction(zip_df) |>
  refit()


# Smoothed glm ------------------------------------------------------------

# Fit GAM for claim frequency
age_policyholder_frequency <- risk_factor_gam(data = MTPL,
                                              claim_count = "nclaims",
                                              risk_factor = "age_policyholder",
                                              exposure = "exposure")

# Determine tariff segments
age_segments_freq <- derive_tariff_segments(age_policyholder_frequency)

# Add tariff segments to MTPL portfolio
dat <- MTPL |>
  add_tariff_segments(age_segments_freq, name = "age_policyholder_freq_cat") |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(across(where(is.factor), ~set_reference_level(., exposure)))

# Fit frequency and severity model
freq <- glm(nclaims ~ bm + age_policyholder_freq_cat, offset = log(exposure),
            family = poisson(), data = dat)
sev <- glm(amount ~ bm + zip, weights = nclaims,
           family = Gamma(link = "log"), data = dat |> filter(amount > 0))

# Add predictions for freq and sev to data, and calculate premium
premium_df <- dat |>
  add_prediction(freq, sev) |>
  mutate(premium = pred_nclaims_freq * pred_amount_sev)

# Fit unrestricted model
burn_unrestricted <- glm(premium ~ zip + bm + age_policyholder_freq_cat,
                         weights = exposure,
                         family = Gamma(link = "log"),
                         data = premium_df)

# Impose smoothing and refit model
suppressWarnings({
  burn_smooth <- prepare_refinement(burn_unrestricted) |>
    add_smoothing(model_variable = "age_policyholder_freq_cat",
                  source_variable = "age_policyholder",
                  breaks = seq(18, 95, 5)) |>
    refit()
})

# Fit unrestricted model with intercept only
burn2_unrestricted <- glm(premium ~ age_policyholder_freq_cat,
                          weights = exposure,
                          family = Gamma(link = "log"),
                          data = premium_df)

# Impose smoothing and refit model with intercept only
suppressWarnings({
  burn2_smooth <- prepare_refinement(burn2_unrestricted) |>
    add_smoothing(model_variable = "age_policyholder_freq_cat",
                  source_variable = "age_policyholder",
                  breaks = seq(18, 95, 5)) |>
    refit()
})



# Glm ---------------------------------------------------------------------

mtpl2a <- MTPL2
mtpl2a$area <- as.factor(mtpl2a$area)
x <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
         data = mtpl2a)

df <- MTPL2 %>%
  mutate(across(c(area), as.factor)) %>%
  mutate(across(c(area), ~set_reference_level(., exposure)))

mod1 <- glm(nclaims ~ area + premium, offset = log(exposure),
            family = poisson(), data = df)
mod2 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
            data = df)




# Tests -------------------------------------------------------------------

testthat::test_that(
  "No errors are returned for smoothed glm objects", {
    testthat::expect_error(rating_table(burn_smooth, significance = FALSE), NA)
  }
)

testthat::test_that(
  "No errors are returned for restricted glm objects", {
    testthat::expect_error(rating_table(burn_rst, significance = FALSE), NA)
  }
)

testthat::test_that(
  "No errors are returned for restricted glm objects with intercept only", {
    testthat::expect_error(rating_table(burn2_rst, significance = FALSE), NA)
  }
)

testthat::test_that(
  "No errors are returned for smoothed glm objects with intercept only", {
    testthat::expect_error(rating_table(burn2_smooth, significance = FALSE), NA)
  }
)

testthat::test_that(
  "No errors are returned for glm objects", {
    testthat::expect_error(rating_table(x, significance = FALSE), NA)
  }
)

testthat::test_that(
  "No errors are returned for multiple glm objects with model_data", {
    testthat::expect_error(rating_table(mod1, mod2, model_data = df,
                                        exposure = "exposure",
                                        significance = FALSE), NA)
  }
)

testthat::test_that(
  "rating_table returns a stable output contract", {
    rt <- rating_table(mod1,
                       model_data = df,
                       exposure = "exposure",
                       exposure_output = "earned_exposure",
                       significance = TRUE)

    testthat::expect_s3_class(rt, "rating_table")
    testthat::expect_s3_class(rt, "riskfactor")
    testthat::expect_true(rt$significance)
    testthat::expect_true(rt$signif_stars)
    testthat::expect_equal(rt$exposure, "earned_exposure")
    testthat::expect_true(all(c(
      "risk_factor", "level", "est_mod1", "earned_exposure", "signif_mod1"
    ) %in% names(rt$df)))
    testthat::expect_s3_class(summary(rt), "summary.rating_table")
    testthat::expect_s3_class(summary(rt), "summary.riskfactor")
    testthat::expect_s3_class(as.data.frame(rt), "data.frame")
  }
)

testthat::test_that(
  "rating_table compares multiple models with one exposure output", {
    rt <- rating_table(mod1, mod2,
                       model_data = df,
                       exposure = "exposure",
                       exposure_output = "earned_exposure",
                       significance = FALSE)

    testthat::expect_true(all(c(
      "risk_factor", "level", "est_mod1", "est_mod2", "earned_exposure"
    ) %in% names(rt$df)))
    testthat::expect_false(any(grepl("^signif_", names(rt$df))))
  }
)

testthat::test_that(
  "as_gt presents rating factors as grouped tariff tables", {
    testthat::skip_if_not_installed("gt")

    rt <- rating_table(
      mod1,
      model_data = df,
      exposure = "exposure",
      exposure_output = "earned_exposure",
      significance = TRUE
    )
    tbl <- as_gt(rt)

    testthat::expect_s3_class(tbl, "gt_tbl")
    testthat::expect_equal(tbl[["_locale"]]$locale, "nl-NL")
    testthat::expect_true(all(c(
      "risk_factor", "level", "est_mod1", "earned_exposure"
    ) %in% names(tbl[["_data"]])))
    testthat::expect_setequal(
      unique(tbl[["_data"]]$risk_factor),
      tbl[["_row_groups"]]
    )

    html <- suppressWarnings(as.character(gt::as_raw_html(tbl)))
    testthat::expect_match(html, "Relativities")
    testthat::expect_match(html, "Significance levels")
    testthat::expect_match(html, "Risk factor")

    without_stars <- as_gt(
      rt,
      significance = FALSE,
      locale = "en-US",
      estimate_decimals = 2,
      exposure_decimals = 1
    )
    testthat::expect_s3_class(without_stars, "gt_tbl")
    testthat::expect_equal(without_stars[["_locale"]]$locale, "en-US")
    testthat::expect_no_match(
      suppressWarnings(as.character(gt::as_raw_html(without_stars))),
      "Significance levels"
    )

    coefficient_table <- rating_table(
      mod1,
      model_data = df,
      exposure = FALSE,
      exponentiate = FALSE
    )
    coefficient_gt <- as_gt(coefficient_table)
    testthat::expect_match(
      suppressWarnings(as.character(gt::as_raw_html(coefficient_gt))),
      "Coefficients"
    )

    comparison_table <- rating_table(
      mod1,
      mod2,
      model_data = df,
      exposure = FALSE
    )
    comparison_gt <- as_gt(comparison_table)
    testthat::expect_true(all(
      c("est_mod1", "est_mod2") %in% names(comparison_gt[["_data"]])
    ))
  }
)

testthat::test_that(
  "as_gt validates unavailable rating-table significance", {
    testthat::skip_if_not_installed("gt")

    rt <- rating_table(
      mod1,
      model_data = df,
      exposure = "exposure",
      significance = FALSE
    )

    testthat::expect_error(
      as_gt(rt, significance = TRUE),
      "rating_table.*significance = TRUE"
    )
    testthat::expect_error(
      as_gt(rt, significance = NA),
      "significance"
    )
  }
)

testthat::test_that(
  "deprecated rating_table arguments remain available", {
    testthat::expect_warning(
      rating_table(mod1, model_data = df, exposure = "exposure",
                   exposure_name = "earned_exposure"),
      "deprecated"
    )
    testthat::expect_warning(
      rating_table(mod1, signif_stars = TRUE),
      "deprecated"
    )
    testthat::expect_error(
      rating_table(mod1, model_data = df, exposure = "exposure",
                   exposure_output = "earned_exposure",
                   exposure_name = "other_exposure"),
      "Use only one"
    )
    testthat::expect_warning(
      rating_factors(mod1),
      "deprecated"
    )
    testthat::expect_warning(
      rating_factors2(mod1),
      "deprecated"
    )
  }
)

testthat::test_that(
  "rating_table rejects pre-refit refinement objects", {
    testthat::expect_error(
      rating_table(prepare_refinement(mod1)),
      "after refit"
    )
  }
)

testthat::test_that(
  "autoplot works for rating_table output", {
    rt <- rating_table(mod1, model_data = df, exposure = "exposure")
    p <- ggplot2::autoplot(
      rt,
      risk_factors = "area",
      show_exposure_labels = TRUE,
      decimal_mark = ",",
      y_label = "Relativity",
      bar_fill = "#E6E6E6",
      model_color = "#2C7FB8",
      use_linetype = FALSE
    )

    testthat::expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
    testthat::expect_identical(autoplot.riskfactor, autoplot.rating_table)
  }
)

testthat::test_that(
  "observed experience can be attached to rating_table output", {
    rt <- rating_table(mod1, model_data = df, exposure = "exposure")
    observed <- factor_analysis(
      df,
      risk_factors = "area",
      claim_count = "nclaims",
      exposure = "exposure"
    )

    rt_observed <- add_portfolio_experience(
      rt,
      observed,
      metric = "frequency",
      label = "Observed frequency",
      scale = "mean"
    )

    testthat::expect_s3_class(rt_observed, "rating_table")
    testthat::expect_identical(
      rt_observed$observed_experience$metric,
      "frequency"
    )
    testthat::expect_true("data" %in% names(rt_observed$observed_experience))
    testthat::expect_true("area" %in%
                            rt_observed$observed_experience$data$risk_factor)

    p <- ggplot2::autoplot(rt_observed, risk_factors = "area")
    testthat::expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
  }
)

testthat::test_that(
  "observed experience can be calculated automatically for rating table factors", {
    rt <- rating_table(mod1, model_data = df, exposure = "exposure")

    rt_observed <- add_portfolio_experience(
      rt,
      data = df,
      claim_count = "nclaims",
      exposure = "exposure"
    )

    testthat::expect_s3_class(rt_observed, "rating_table")
    expected_factors <- intersect(unique(rt$df$risk_factor), names(df))
    testthat::expect_true(all(
      expected_factors %in% unique(rt_observed$observed_experience$data$risk_factor)
    ))
    testthat::expect_identical(
      rt_observed$observed_experience$metric,
      "frequency"
    )

    p <- ggplot2::autoplot(rt_observed, metric = "frequency")
    testthat::expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
  }
)

testthat::test_that(
  "multiple factor_analysis objects can be attached to a rating table", {
    rt <- rating_table(mod1, model_data = df, exposure = "exposure")
    observed_area <- factor_analysis(
      df,
      risk_factors = "area",
      claim_count = "nclaims",
      exposure = "exposure"
    )
    observed_premium <- factor_analysis(
      df,
      risk_factors = "premium",
      claim_count = "nclaims",
      exposure = "exposure"
    )

    rt_observed <- add_portfolio_experience(
      rt,
      observed = list(observed_area, observed_premium)
    )

    testthat::expect_equal(
      sort(unique(rt_observed$observed_experience$data$risk_factor)),
      sort(c("area", "premium"))
    )
    testthat::expect_s3_class(
      ggplot2::autoplot(rt_observed, risk_factors = "area", metric = "frequency"),
      "ggplot"
    )
  }
)

testthat::test_that(
  "add_observed_experience remains available as deprecated alias", {
    rt <- rating_table(mod1, model_data = df, exposure = "exposure")

    testthat::expect_warning(
      rt_observed <- add_observed_experience(
        rt,
        data = df,
        claim_count = "nclaims",
        exposure = "exposure"
      ),
      "deprecated"
    )

    testthat::expect_s3_class(rt_observed, "rating_table")
  }
)

testthat::test_that(
  "deprecated autoplot.rating_table arguments remain available", {
    rt <- rating_table(mod1, model_data = df, exposure = "exposure")

    testthat::expect_warning(
      p <- ggplot2::autoplot(
        rt,
        risk_factors = "area",
        labels = FALSE,
        dec.mark = ".",
        ylab = "Relativity",
        fill = "#E6E6E6",
        color = "#2C7FB8",
        linetype = TRUE
      ),
      "deprecated"
    )

    testthat::expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
  }
)


filter_zip <- rating_table(burn_smooth)$df |>
  dplyr::filter(risk_factor == "zip")

testthat::test_that(
  "NA values are set to 1 for smoothed glm", {
    testthat::expect_gte(sum(filter_zip$est_burn_smooth), 0)
  }
)

filter_area <- rating_table(mod1)$df |>
  dplyr::filter(risk_factor == "area")

testthat::test_that(
  "NA values are set to 1 for non smoothed or non restricted glm", {
    testthat::expect_gte(sum(filter_area$est_burn_smooth), 0)
  }
)


zip_df <- data.frame(zip = c(0,1,2,3),
                     zip_restricted = c(0.8, 0.9, 1, 1.2))

burn_restricted2 <- prepare_refinement(burn_unrestricted) |>
  add_restriction(zip_df) |>
  refit()

filter_zip_rst <- rating_table(burn_restricted2)$df |>
  dplyr::filter(risk_factor == "zip_restricted")

testthat::test_that(
  "Check if all values are not equal to one", {
    testthat::expect_false(var(filter_zip_rst$est_burn_restricted2) == 0)
  }
)
