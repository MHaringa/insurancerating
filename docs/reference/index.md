# Package index

## Portfolio analysis

Analyse portfolio behaviour and inspect risk factors before model
estimation.

- [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  [`univariate()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  : Factor analysis for discrete risk factors
- [`autoplot(`*`<factor_analysis>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.factor_analysis.md)
  : Automatically create a ggplot for objects obtained from factor
  analysis
- [`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  [`histbin()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  : Portfolio histogram with tail bins

## Tariff classes

Analyse continuous risk factors and convert them to tariff classes.

- [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  [`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  : Fit a GAM for a continuous risk factor

- [`autoplot(`*`<riskfactor_gam>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.riskfactor_gam.md)
  :

  Autoplot for GAM objects from
  [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)

- [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md)
  : Construct insurance tariff classes

- [`autoplot(`*`<tariff_classes>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.tariff_classes.md)
  : Autoplot for tariff class objects

## Modelling and interpretation

Estimate pricing models and interpret fitted coefficients in tariff
terms.

- [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  : Build rating tables from fitted pricing models

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
  **\[experimental\]** : Plot a model refinement step
- [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  : Add smoothing to a refinement workflow
- [`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
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

## Validation

Assess model fit, predictive stability, dispersion, and residual
behaviour.

- [`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md)
  : Performance of fitted GLMs
- [`rmse()`](https://mharinga.github.io/insurancerating/reference/rmse.md)
  : Root Mean Squared Error (RMSE)
- [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md)
  [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md)
  : Bootstrapped model performance
- [`autoplot(`*`<bootstrap_performance>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.bootstrap_performance.md)
  : Autoplot for bootstrap_performance objects
- [`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md)
  : Check overdispersion of a Poisson claim frequency model
- [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md)
  : Check simulation-based model residuals
- [`autoplot(`*`<check_residuals>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.check_residuals.md)
  : Autoplot for check_residuals objects

## Model structure

Extract model data and analyse observed rating-grid combinations.

- [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md)
  [`model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md)
  **\[experimental\]** : Extract model data
- [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  [`construct_model_points()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  : Construct observed rating-grid points from model data or a data
  frame

## Distribution helpers

Helper functions for truncated and actuarial distribution workflows.

- [`fit_truncated_dist()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_dist.md)
  **\[experimental\]** : Fit severity distributions to truncated claim
  data
- [`autoplot(`*`<truncated_dist>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.truncated_dist.md)
  : Plot a fitted truncated severity distribution
- [`rlnormt()`](https://mharinga.github.io/insurancerating/reference/rlnormt.md)
  : Generate random samples from a truncated lognormal distribution
- [`rgammat()`](https://mharinga.github.io/insurancerating/reference/rgammat.md)
  : Generate random samples from a truncated gamma distribution

## Utilities

Supporting functions used across pricing workflows.

- [`biggest_reference()`](https://mharinga.github.io/insurancerating/reference/biggest_reference.md)
  : Set reference group to the group with largest exposure
- [`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
  [`period_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
  : Split policy periods into monthly rows
- [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  [`rows_per_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  : Find active portfolio rows for event dates
- [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  [`reduce()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  : Reduce portfolio periods by merging adjacent date ranges

## Data

- [`MTPL`](https://mharinga.github.io/insurancerating/reference/MTPL.md)
  : Motor Third Party Liability (MTPL) portfolio
- [`MTPL2`](https://mharinga.github.io/insurancerating/reference/MTPL2.md)
  : Motor Third Party Liability (MTPL) portfolio (3,000 policyholders)

## Deprecated

Legacy functions retained for backward compatibility. New code should
use the updated API.

- [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  [`univariate()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  : Factor analysis for discrete risk factors
- [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  [`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  : Fit a GAM for a continuous risk factor
- [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  : Build rating tables from fitted pricing models
- [`rating_factors2()`](https://mharinga.github.io/insurancerating/reference/rating_factors2.md)
  **\[deprecated\]** : Include reference group in regression output
- [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md)
  [`model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md)
  **\[experimental\]** : Extract model data
- [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  [`construct_model_points()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  : Construct observed rating-grid points from model data or a data
  frame
- [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md)
  [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md)
  : Bootstrapped model performance
- [`fisher_classify()`](https://mharinga.github.io/insurancerating/reference/fisher_classify.md)
  [`fisher()`](https://mharinga.github.io/insurancerating/reference/fisher_classify.md)
  : Fisher's natural breaks classification
- [`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  [`histbin()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  : Portfolio histogram with tail bins
- [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  [`restrict_coef()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  : Add coefficient restrictions to a refinement workflow
- [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  : Add smoothing to a refinement workflow
- [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
  [`update_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
  : Refit a GLM model or refinement workflow
- [`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
  [`period_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
  : Split policy periods into monthly rows
- [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  [`rows_per_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  : Find active portfolio rows for event dates
- [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  [`reduce()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  : Reduce portfolio periods by merging adjacent date ranges
