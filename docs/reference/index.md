# Package index

## Portfolio analysis

Analyse portfolio behaviour and inspect risk factors before model
estimation.

- [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  [`univariate_summary()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  [`univariate()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  : Factor analysis for discrete risk factors
- [`autoplot(`*`<univariate>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.univariate.md)
  : Automatically create a ggplot for objects obtained from factor
  analysis
- [`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  [`histbin()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  : Histogram with outlier bins
- [`biggest_reference()`](https://mharinga.github.io/insurancerating/reference/biggest_reference.md)
  : Set reference group to the group with largest exposure

## Tariff classes

Analyse continuous risk factors and convert them to tariff classes.

- [`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
  : Generalized Additive Model for Insurance Risk Factors

- [`summary(`*`<fitgam>`*`)`](https://mharinga.github.io/insurancerating/reference/summary.fitgam.md)
  : Summary method for fitgam objects

- [`print(`*`<fitgam>`*`)`](https://mharinga.github.io/insurancerating/reference/print.fitgam.md)
  : Print method for fitgam objects

- [`autoplot(`*`<fitgam>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.fitgam.md)
  :

  Autoplot for GAM Objects from
  [`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)

- [`as.data.frame(`*`<fitgam>`*`)`](https://mharinga.github.io/insurancerating/reference/as.data.frame.fitgam.md)
  : Coerce fitgam objects to a data frame

- [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md)
  : Construct insurance tariff classes

- [`print(`*`<constructtariffclasses>`*`)`](https://mharinga.github.io/insurancerating/reference/print.constructtariffclasses.md)
  : Print method for constructtariffclasses objects

- [`as.vector(`*`<constructtariffclasses>`*`)`](https://mharinga.github.io/insurancerating/reference/as.vector.constructtariffclasses.md)
  : Coerce constructtariffclasses to a vector

- [`autoplot(`*`<constructtariffclasses>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.constructtariffclasses.md)
  : Autoplot for tariff class objects

## Modelling and interpretation

Estimate pricing models and interpret fitted coefficients in tariff
terms.

- [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  : Include reference group in regression output

- [`autoplot(`*`<riskfactor>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.riskfactor.md)
  :

  Plot risk factor effects from
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  results

- [`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md)
  : Add Model Predictions to a Data Frame

## Refinement workflow

Apply structured tariff adjustments such as smoothing, restrictions, and
relativities.

- [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md)
  : Prepare a model refinement workflow
- [`autoplot(`*`<rating_refinement>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_refinement.md)
  **\[experimental\]** : Automatically create a ggplot for objects
  obtained from refinement
- [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  : Add smoothing to a refinement workflow
- [`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
  [`update_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
  : Edit an existing smoothing step in a refinement workflow
- [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  [`restrict_coef()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  : Add coefficient restrictions to a refinement workflow
- [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
  : Add expert-based relativities to a refinement workflow
- [`relativities_list()`](https://mharinga.github.io/insurancerating/reference/relativities_list.md)
  : Combine multiple level splits into a relativities list
- [`split_level()`](https://mharinga.github.io/insurancerating/reference/split_level.md)
  : Define a level split with relativities
- [`split_relativities()`](https://mharinga.github.io/insurancerating/reference/split_relativities.md)
  : Construct a relativities mapping for level splitting
- [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
  : Refit a prepared refinement workflow
- [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
  [`update_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
  : Refit a GLM model or refinement workflow

## Validation

Assess model fit, predictive stability, dispersion, and residual
behaviour.

- [`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md)
  : Performance of fitted GLMs
- [`print(`*`<model_performance>`*`)`](https://mharinga.github.io/insurancerating/reference/print.model_performance.md)
  : Print method for model_performance objects
- [`rmse()`](https://mharinga.github.io/insurancerating/reference/rmse.md)
  : Root Mean Squared Error (RMSE)
- [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md)
  [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md)
  : Bootstrapped model performance
- [`as.vector(`*`<bootstrap_performance>`*`)`](https://mharinga.github.io/insurancerating/reference/as.vector.bootstrap_performance.md)
  : Coerce bootstrap_performance objects to a vector
- [`autoplot(`*`<bootstrap_performance>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.bootstrap_performance.md)
  : Autoplot for bootstrap_performance objects
- [`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md)
  : Check overdispersion of a Poisson GLM
- [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md)
  : Check model residuals
- [`autoplot(`*`<check_residuals>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.check_residuals.md)
  : Autoplot for check_residuals objects

## Model structure

Extract model data and analyse observed rating-grid combinations.

- [`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  **\[experimental\]** : Extract model data
- [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  [`construct_model_points()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  : Construct observed rating-grid points from model data or a data
  frame

## Distribution helpers

Helper functions for truncated and actuarial distribution workflows.

- [`fit_truncated_dist()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_dist.md)
  **\[experimental\]** : Fit a distribution to truncated severity (loss)
  data
- [`autoplot(`*`<truncated_dist>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.truncated_dist.md)
  : Automatically create a ggplot for objects obtained from
  fit_truncated_dist()
- [`rlnormt()`](https://mharinga.github.io/insurancerating/reference/rlnormt.md)
  : Generate random samples from a truncated lognormal distribution
- [`rgammat()`](https://mharinga.github.io/insurancerating/reference/rgammat.md)
  : Generate random samples from a truncated gamma distribution

## Utilities

Supporting functions used across pricing workflows.

- [`fisher_classify()`](https://mharinga.github.io/insurancerating/reference/fisher_classify.md)
  [`fisher()`](https://mharinga.github.io/insurancerating/reference/fisher_classify.md)
  : Fisher's natural breaks classification
- [`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
  [`period_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
  : Split periods into monthly intervals
- [`rows_per_date()`](https://mharinga.github.io/insurancerating/reference/rows_per_date.md)
  : Find active rows per date
- [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  [`reduce()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  : Reduce portfolio by merging redundant date ranges
- [`summary(`*`<reduce>`*`)`](https://mharinga.github.io/insurancerating/reference/summary.reduce.md)
  : Summarize reduce objects

## Data

- [`MTPL`](https://mharinga.github.io/insurancerating/reference/MTPL.md)
  : Motor Third Party Liability (MTPL) portfolio
- [`MTPL2`](https://mharinga.github.io/insurancerating/reference/MTPL2.md)
  : Motor Third Party Liability (MTPL) portfolio (3,000 policyholders)

## Deprecated

Legacy functions retained for backward compatibility. New code should
use the updated API.

- [`rating_factors2()`](https://mharinga.github.io/insurancerating/reference/rating_factors2.md)
  **\[deprecated\]** : Include reference group in regression output
- [`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  **\[experimental\]** : Extract model data
