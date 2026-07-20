# Package index

## Portfolio analysis

Analyse portfolio behaviour and inspect risk factors before model
estimation.

- [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  : Factor analysis for discrete risk factors
- [`autoplot(`*`<factor_analysis>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.factor_analysis.md)
  : Automatically create a ggplot for objects obtained from factor
  analysis
- [`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  : Portfolio histogram with tail bins

## Severity and large claims

Analyse claim amounts, capped or truncated severity, and large-loss
components that can be added to technical risk premium models.

- [`plot_severity_distribution()`](https://mharinga.github.io/insurancerating/reference/plot_severity_distribution.md)
  : Exploratory severity diagnostics by category
- [`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md)
  : Assess possible excess-loss thresholds
- [`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
  : Convert an object to a gt table
- [`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md)
  : Decompose claim amounts into capped and excess parts
- [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
  : Allocate excess loss to a pricing portfolio
- [`autoplot(`*`<excess_allocation>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.excess_allocation.md)
  : Plot an excess-loss allocation
- [`apply_excess_loading()`](https://mharinga.github.io/insurancerating/reference/apply_excess_loading.md)
  : Apply allocated excess losses or loadings to a pricing portfolio
- [`fit_truncated_severity()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_severity.md)
  **\[experimental\]** : Fit severity distributions to truncated claim
  data
- [`autoplot(`*`<truncated_severity>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.truncated_severity.md)
  : Plot a fitted truncated severity distribution
- [`rlnormt()`](https://mharinga.github.io/insurancerating/reference/rlnormt.md)
  : Generate random samples from a truncated lognormal distribution
- [`rgammat()`](https://mharinga.github.io/insurancerating/reference/rgammat.md)
  : Generate random samples from a truncated gamma distribution

## Tariff segments

Analyse continuous risk factors and convert them to tariff segments.

- [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  : Fit a GAM for a continuous risk factor

- [`autoplot(`*`<riskfactor_gam>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.riskfactor_gam.md)
  :

  Autoplot for GAM objects from
  [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)

- [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)
  : Derive insurance tariff segments

- [`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md)
  : Add derived tariff segments to portfolio data

- [`autoplot(`*`<tariff_segments>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.tariff_segments.md)
  : Autoplot for tariff segment objects

## Modelling and interpretation

Estimate pricing models and interpret fitted coefficients in tariff
terms.

- [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  : Build rating tables from fitted pricing models

- [`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md)
  : Add portfolio experience to a rating table

- [`autoplot(`*`<rating_table>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_table.md)
  :

  Plot risk factor effects from
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  results

- [`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md)
  : Add model predictions to a pricing data set

## Refinement workflow

Apply structured tariff adjustments such as smoothing, restrictions, and
relativities.

- [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md)
  : Prepare a model refinement workflow
- [`autoplot(`*`<rating_refinement>`*`)`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_refinement.md)
  : Plot a model refinement step
- [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  : Add smoothing to a refinement workflow
- [`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
  : Edit an existing smoothing step in a refinement workflow
- [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  : Add coefficient restrictions to a refinement workflow
- [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
  : Add expert-based relativities to a refinement workflow
- [`relativities()`](https://mharinga.github.io/insurancerating/reference/relativities.md)
  : Combine multiple level splits into relativities
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
  **\[experimental\]** : Extract model data
- [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  : Construct observed rating-grid points from model data or a data
  frame

## Utilities

Supporting functions used across pricing workflows.

- [`set_reference_level()`](https://mharinga.github.io/insurancerating/reference/set_reference_level.md)
  : Set the reference level of a factor
- [`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
  : Split policy periods into monthly rows
- [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  : Find active portfolio rows for event dates
- [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  : Reduce portfolio periods by merging adjacent date ranges

## Data

- [`MTPL`](https://mharinga.github.io/insurancerating/reference/MTPL.md)
  : Motor Third Party Liability (MTPL) portfolio
- [`MTPL2`](https://mharinga.github.io/insurancerating/reference/MTPL2.md)
  : Motor Third Party Liability (MTPL) portfolio (3,000 policyholders)

## Deprecated

Legacy functions retained for backward compatibility. New code should
use the updated API.

- [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
  :

  Deprecated alias for
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)

- [`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
  :

  Deprecated alias for
  [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)

- [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/fit_gam.md)
  :

  Deprecated NSE wrapper for
  [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)

- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  :

  Deprecated alias for
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)

- [`rating_factors2()`](https://mharinga.github.io/insurancerating/reference/rating_factors2.md)
  **\[deprecated\]** : Deprecated single-model rating table helper

- [`add_observed_experience()`](https://mharinga.github.io/insurancerating/reference/add_observed_experience.md)
  :

  Deprecated alias for
  [`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md)

- [`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  :

  Deprecated alias for
  [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md)

- [`construct_model_points()`](https://mharinga.github.io/insurancerating/reference/construct_model_points.md)
  :

  Deprecated alias for
  [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)

- [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md)
  :

  Deprecated alias for
  [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)

- [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_rmse.md)
  :

  Deprecated alias for
  [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md)

- [`fit_truncated_dist()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_dist.md)
  :

  Deprecated alias for
  [`fit_truncated_severity()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_severity.md)

- [`fisher_classify()`](https://mharinga.github.io/insurancerating/reference/fisher_classify.md)
  : Fisher's natural breaks classification

- [`fisher()`](https://mharinga.github.io/insurancerating/reference/fisher.md)
  :

  Deprecated alias for
  [`fisher_classify()`](https://mharinga.github.io/insurancerating/reference/fisher_classify.md)

- [`histbin()`](https://mharinga.github.io/insurancerating/reference/histbin.md)
  :

  Deprecated alias for
  [`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)

- [`biggest_reference()`](https://mharinga.github.io/insurancerating/reference/biggest_reference.md)
  :

  Deprecated alias for
  [`set_reference_level()`](https://mharinga.github.io/insurancerating/reference/set_reference_level.md)

- [`restrict_coef()`](https://mharinga.github.io/insurancerating/reference/restrict_coef.md)
  : Deprecated restriction helper

- [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
  : Deprecated smoothing helper

- [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
  : Deprecated refit wrapper

- [`update_glm()`](https://mharinga.github.io/insurancerating/reference/update_glm.md)
  :

  Deprecated alias for
  [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)

- [`period_to_months()`](https://mharinga.github.io/insurancerating/reference/period_to_months.md)
  :

  Deprecated alias for
  [`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)

- [`rows_per_date()`](https://mharinga.github.io/insurancerating/reference/rows_per_date.md)
  :

  Deprecated alias for
  [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)

- [`reduce()`](https://mharinga.github.io/insurancerating/reference/reduce.md)
  :

  Deprecated alias for
  [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
