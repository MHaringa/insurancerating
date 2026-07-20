# Changelog

## insurancerating (development version)

### Main API updates

#### Rating tables

- [`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md)
  is now the primary API for enriching a
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  object with observed portfolio experience.
- [`add_observed_experience()`](https://mharinga.github.io/insurancerating/reference/add_observed_experience.md)
  is deprecated and remains available as a compatibility wrapper.
- [`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md)
  can calculate portfolio experience automatically from portfolio data
  for all risk factors in a
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md),
  and accepts multiple existing
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  objects.
- [`autoplot.rating_table()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_table.md)
  accepts `metric` to choose the attached portfolio experience metric at
  plot time.

## insurancerating 0.8.0

CRAN release: 2026-06-02

### Main API updates

#### Portfolio analysis

- [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  is now the primary function for univariate/factor-level portfolio
  analysis. It returns objects with primary class `"factor_analysis"`
  while retaining `"univariate"` for compatibility.
- [`plot_severity_distribution()`](https://mharinga.github.io/insurancerating/reference/plot_severity_distribution.md)
  was added for exploratory severity diagnostics by category. It shows
  individual claim observations with mean and median markers, optional
  direct labels, and optional firebrick highlighting for claims above a
  user-supplied threshold.
- [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
  is deprecated and remains available as a compatibility wrapper. The
  old NSE interface is still supported through the deprecated wrapper.
- [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  now validates metric columns and grouping variables early, with
  clearer error messages for missing columns.
- Metrics with a zero or missing denominator now return `NA_real_`
  instead of `Inf` or `NaN`.
- [`autoplot.factor_analysis()`](https://mharinga.github.io/insurancerating/reference/autoplot.factor_analysis.md)
  is the primary plot method. The deprecated `show_plots` argument has
  been replaced by `metrics`.
- The factor-analysis plot keeps the established package styling and now
  uses a consistent grid, axis-line, tick and secondary-axis style.

#### Outlier histograms

- [`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  has clearer argument names: `lower`, `upper`, `density`, `bar_fill`,
  `bar_color`, `tail_fill`, `tail_color`, and `density_color`.
- Deprecated argument names remain supported: `left`, `right`, `line`,
  `fill`, `color`, and `fill_outliers`.
- The default colours now align with the package palette: light grey
  bars, orange tail bins, and blue density curve.
- Input validation has been expanded for missing columns, non-numeric
  variables, invalid cutoffs, invalid colours, invalid bin counts,
  constant variables and all-missing variables.
- [`histbin()`](https://mharinga.github.io/insurancerating/reference/histbin.md)
  is deprecated and remains available as a compatibility wrapper.

#### Risk factor GAMs and tariff segmentation

- [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  is the primary spelling for fitting GAMs to continuous risk factors.
  [`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
  and
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/fit_gam.md)
  remain available for compatibility, with
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/fit_gam.md)
  deprecated.
- [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  returns objects with primary class `"risk_factor_gam"`; compatibility
  classes are retained for older code.
- `model = "pure_premium"` replaces the older `model = "burning"`
  wording. The old value remains supported with a lifecycle warning.
- Input validation and documentation for frequency, severity and
  pure-premium GAMs have been improved.
- [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)
  replaces
  [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md)
  as the primary API for deriving tariff segments from a fitted
  risk-factor GAM.
- [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)
  returns objects with primary class `"tariff_segments"`.
- [`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md)
  can add derived tariff segments back to a portfolio.
- [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md)
  remains available as a deprecated compatibility wrapper.
- Split extraction now handles decimal split points correctly.
- Tree-fitting and split-extraction failures now fail clearly instead of
  silently returning one broad tariff interval.
- The tariff-segment plot now recognises confidence interval columns
  produced by the package’s own GAM output.

#### Rating tables

- [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  is the primary API for interpreting fitted GLM coefficients in
  tariff-table form.
  [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  and
  [`rating_factors2()`](https://mharinga.github.io/insurancerating/reference/rating_factors2.md)
  are deprecated wrappers.
- [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  now returns objects with primary class `"rating_table"` while
  retaining the legacy `"riskfactor"` class for compatibility.
- `exposure_output` replaces the older `exposure_name` argument.
- `significance` replaces the older `signif_stars` argument.
- Deprecated rating-table wrappers and plotting code have been separated
  more clearly in the source structure.
- [`add_observed_experience()`](https://mharinga.github.io/insurancerating/reference/add_observed_experience.md)
  was added to attach
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  output to a
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  object before plotting. This replaces the earlier direct
  `univariate_*` arguments in
  [`autoplot.rating_table()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_table.md).
- [`autoplot.rating_table()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_table.md)
  now plots attached observed experience from
  [`add_observed_experience()`](https://mharinga.github.io/insurancerating/reference/add_observed_experience.md)
  and uses cleaner, package-consistent plot styling, including a subtle
  secondary exposure axis.

#### Prediction helpers

- [`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md)
  now has clearer naming arguments: `predictions`, `prefix`,
  `confidence`, and `interval_names`.
- `var` and `conf_int` are deprecated in favour of `predictions` and
  `confidence`.
- Confidence interval columns now use `_lower` and `_upper` suffixes by
  default.
- The function now validates `alpha`, confidence settings, duplicate
  output names, name collisions with existing columns, missing models
  and non-GLM inputs.

#### Model data and rating grids

- [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md)
  replaces
  [`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  as the primary API for extracting model data from fitted models.
- [`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  is deprecated and remains available as a wrapper.
- [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  now uses base R internally and returns a regular `data.frame`.
- Plain GLM metadata extraction has been improved so `rating_grid(glm)`
  groups by model terms as expected.
- Refinement metadata is now joined by the related original/new factor
  columns instead of being cross-joined onto every rating-grid row.

#### Model refinement

- The refinement API has been clarified around
  `prepare_refinement() |> add_*() |> refit()`.
- A new excess-loss workflow was added for capped severity and
  pure-premium modelling:
  [`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md),
  [`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md),
  [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
  and
  [`apply_excess_loading()`](https://mharinga.github.io/insurancerating/reference/apply_excess_loading.md).
- [`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md)
  compares candidate large-loss thresholds and shows the impact on
  excess loss, capped loss and pure premium.
- [`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md)
  now performs only the deterministic historical decomposition into
  capped and excess claim amounts.
- [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
  handles allocation and bootstrap uncertainty modelling. It supports
  observed or bootstrap excess burdens, portfolio, risk-factor and
  partial allocation, and optional severity noise in the bootstrap.
- [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
  now uses clearer allocation argument names: `allocation_weight`,
  `risk_factor`, `receives_allocation`, `allocation`, `n_bootstrap`,
  `bootstrap_seed` and `preserve_total_excess`.
- Automatic credibility in
  [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
  now uses the transparent formula `Z = n / (n + credibility_threshold)`
  with `credibility_basis = "claims"`, `"excess_claims"` or
  `"allocation_weight"`.
- [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
  now uses `preserve_total_excess = TRUE` by default so that partial
  allocation redistributes the selected excess burden without changing
  the total allocated excess loss.
- [`apply_excess_loading()`](https://mharinga.github.io/insurancerating/reference/apply_excess_loading.md)
  adds the allocated excess loading to pricing data and returns
  `base_premium`, `excess_loading` and `loaded_premium`.
- [`apply_excess_loading()`](https://mharinga.github.io/insurancerating/reference/apply_excess_loading.md)
  now treats premium amounts as the default workflow and keeps the
  distinction between absolute `expected_excess_loss` and per-weight
  `blended_excess_loading` explicit.
- [`apply_excess_loading()`](https://mharinga.github.io/insurancerating/reference/apply_excess_loading.md)
  now uses `base_value` for the existing premium amount or rate. With
  rate output, an existing rate can be used directly or a monetary
  amount can be converted using `allocation_weight`. The standard
  allocation columns are now detected automatically.
- [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  now uses `model_variable` and `source_variable` as the primary
  argument names.
- [`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
  now uses clearer in-object editing arguments for adjusting smoothing
  settings without supplying an external data frame.
- [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  can now accept a partial restriction data frame. Missing levels are
  automatically filled with the already fitted GLM relativities, so
  users can adjust only selected levels.
- [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
  now uses `model_variable` and `split_variable`.
- [`relativities()`](https://mharinga.github.io/insurancerating/reference/relativities.md)
  replaces `relativities_list()` as the helper for building relativity
  specifications.
- [`restrict_coef()`](https://mharinga.github.io/insurancerating/reference/restrict_coef.md),
  [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
  and
  [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
  remain deprecated compatibility wrappers and now link clearly to
  [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
  [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  and
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
- [`autoplot.rating_refinement()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_refinement.md)
  no longer carries an experimental badge and uses the package plot
  theme.
- The refinement documentation has been expanded with applied examples
  and a clearer explanation of smoothing, restrictions, relativities and
  refitting.

#### Reference levels

- [`set_reference_level()`](https://mharinga.github.io/insurancerating/reference/set_reference_level.md)
  replaces
  [`biggest_reference()`](https://mharinga.github.io/insurancerating/reference/biggest_reference.md)
  as the primary helper for choosing factor reference levels.
- The default method is `method = "largest_weight"`.
- A manual `level` argument was added so a specific reference level can
  be selected explicitly.
- [`biggest_reference()`](https://mharinga.github.io/insurancerating/reference/biggest_reference.md)
  remains available as a deprecated compatibility wrapper.

#### Time utilities

- [`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md),
  [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  and
  [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  now avoid mutating caller-visible input data.
- [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  replaces
  [`rows_per_date()`](https://mharinga.github.io/insurancerating/reference/rows_per_date.md)
  as the primary API for matching event dates, such as claim dates, to
  active portfolio rows.
- [`period_to_months()`](https://mharinga.github.io/insurancerating/reference/period_to_months.md),
  [`rows_per_date()`](https://mharinga.github.io/insurancerating/reference/rows_per_date.md)
  and
  [`reduce()`](https://mharinga.github.io/insurancerating/reference/reduce.md)
  remain available as deprecated compatibility wrappers.
- Date interval validation, column validation, aggregation validation,
  `nomatch` validation and `mult` validation have been improved.
- R CMD check notes from data.table helper columns in
  [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  have been resolved.

#### Model validation and performance

- [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md)
  now has an explicit `metric = "rmse"` argument.
- `sampling = c("bootstrap", "split")` was added to distinguish
  bootstrap out-of-bag evaluation from split validation.
- Deprecated arguments `n` and `frac` remain supported as aliases for
  `n_resamples` and `sample_fraction`.
- Character and factor rating variables are handled more robustly across
  resamples so prediction does not fail when a level is absent from a
  training sample.
- [`autoplot.bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/autoplot.bootstrap_performance.md)
  now uses a package-consistent visual style: subtle grey histogram,
  transparent blue density, orange original-model reference line, subtle
  confidence interval lines and no gap between the bars and x-axis.
- [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_rmse.md)
  remains available as a deprecated compatibility wrapper and returned
  objects retain class `"bootstrap_rmse"` for older code.
- [`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md)
  now validates non-GLM input, checks for Poisson models and fails
  clearly when residual degrees of freedom are not positive.
- `print.overdispersion()` now bases its conclusion on the original
  p-value rather than a rounded display value.
- [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md)
  now validates inputs, uses all scaled residuals for the KS test,
  handles empty residual vectors clearly and documents the DHARMa-based
  residual workflow for actuarial users.
- [`autoplot.check_residuals()`](https://mharinga.github.io/insurancerating/reference/autoplot.check_residuals.md)
  now has a controllable `max_points` argument and uses ASCII messages
  and the package plot theme.

#### Truncated severity distributions

- [`fit_truncated_severity()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_severity.md)
  replaces
  [`fit_truncated_dist()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_dist.md)
  as the primary API for fitting distributions to truncated claim
  severities.
- Returned objects use primary class `"truncated_severity"` while
  compatibility with `"truncated_dist"` is retained.
- [`fit_truncated_dist()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_dist.md)
  remains available as a deprecated compatibility wrapper.
- Observations outside the truncation interval now fail clearly instead
  of only warning and continuing.
- Validation has been expanded for truncation bounds, optimisation
  starts, grid sizes, reporting options and random generator arguments.
- Public random generators
  [`rlnormt()`](https://mharinga.github.io/insurancerating/reference/rlnormt.md)
  and
  [`rgammat()`](https://mharinga.github.io/insurancerating/reference/rgammat.md)
  now validate sample size, distribution parameters, finite intervals
  and positive truncation mass.
- Plot argument names were modernised to `ecdf_geom`, `x_label`,
  `y_label`, `show_title`, `digits` and `truncation_digits`, with old
  names supported for compatibility.

#### Fisher-Jenks classification

- [`fisher_classify()`](https://mharinga.github.io/insurancerating/reference/fisher_classify.md)
  and
  [`fisher()`](https://mharinga.github.io/insurancerating/reference/fisher.md)
  are deprecated because Fisher-Jenks classification is a
  general-purpose grouping method and is not directly tied to the
  insurance-rating workflow.
- `classInt` moved from `Imports` to `Suggests`.

### Documentation, website and tests

- The README and vignettes have been revised to present the package as a
  set of actuarial pricing building blocks rather than a prescribed
  pricing method.
- The former “Pricing principles” vignette was replaced by “Pricing
  workflow building blocks”.
- The refinement vignette was rewritten with a more practical tone and
  current API examples.
- pkgdown reference sections were reorganised; deprecated functions are
  grouped under “Deprecated” and internal S3 methods are no longer
  listed as primary reference topics.
- New and expanded tests cover tariff segmentation, rating tables,
  observed experience plotting, refinement workflows, model-data
  extraction, model performance, overdispersion, residual checks,
  outlier histograms, truncated distributions, time utilities and factor
  analysis.

## insurancerating 0.7.5

CRAN release: 2024-10-09

- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  now always returns correct output when column with exposure in data is
  not named `exposure`
- `intercept_only` in
  [`update_glm()`](https://mharinga.github.io/insurancerating/reference/update_glm.md)
  is added to apply the manual changes and refit the intercept, ensuring
  that the changes have no impact on the other variables.
- `smoothing` in
  [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
  is added to choose smoothing specification
- The README has been revised

## insurancerating 0.7.4

CRAN release: 2024-05-20

- [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_rmse.md)
  now uses `after_stat(density)` instead of the deprecated dot-dot
  notation
- `custom_theme` in `autoplot.univariate()` is added to customize the
  theme

## insurancerating 0.7.3

CRAN release: 2024-05-09

- `autoplot.univariate()` now generates a plot even when there are
  missing values in the rows
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  now always returns the correct coefficients when used on a
  ‘refitsmooth’ or ‘refitrestricted’ class of GLM.

## insurancerating 0.7.2

CRAN release: 2022-12-20

- [`update_glm()`](https://mharinga.github.io/insurancerating/reference/update_glm.md)
  now always returns the correct interval in case the function is used
  in combination with
  [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)

## insurancerating 0.7.1

CRAN release: 2022-09-06

- `rotate_angle` in `autoplot.univariate()` is added to rotate x-labels
- [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
  now accepts external vectors for `x`; `vec_ext()` must be used

## insurancerating 0.7.0

CRAN release: 2022-07-08

- [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
  now gives correct results for intervals with scientific notation
- [`reduce()`](https://mharinga.github.io/insurancerating/reference/reduce.md)
  now returns no errors anymore for columns with dates in POSIXt format

## insurancerating 0.6.9

CRAN release: 2021-12-11

- [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
  is renamed to
  [`update_glm()`](https://mharinga.github.io/insurancerating/reference/update_glm.md)
- [`construct_model_points()`](https://mharinga.github.io/insurancerating/reference/construct_model_points.md)
  and
  [`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  are added to create model points

## insurancerating 0.6.8

CRAN release: 2021-11-10

- `show_total` in `autoplot.univariate()` is added to add line for total
  of groups in case `by` is used in
  [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md);
  `total_color` can be used to change the color of the line, and
  `total_name` is added to change the name of the legend for the line
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  now accepts GLMs with an intercept only
- [`fit_truncated_dist()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_dist.md)
  is added to fit the original distribution (gamma, lognormal) from
  truncated severity data
- `join_to_nearest()` now returns NA in case NA is used as input

## insurancerating 0.6.7

CRAN release: 2021-07-28

- [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
  now returns an error message when intervals are not obtained by cut()
- `get_data()` is added to return the data used in
  [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)

## insurancerating 0.6.6

CRAN release: 2021-05-19

- `summary.reduce()` now gives correct aggregation for periods “months”
  and “quarters”
- [`rows_per_date()`](https://mharinga.github.io/insurancerating/reference/rows_per_date.md)
  is added to determine active portfolio for a certain date

## insurancerating 0.6.5

CRAN release: 2021-03-22

- [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
  and
  [`restrict_coef()`](https://mharinga.github.io/insurancerating/reference/restrict_coef.md)
  are added for model refinement
- [`histbin()`](https://mharinga.github.io/insurancerating/reference/histbin.md)
  now uses darkblue as default fill color

## insurancerating 0.6.4

CRAN release: 2021-01-12

- In `summary.reduce()`, `name` can be used to change the name of the
  new column in the output.
- Dataset `MTPL` now contains extra columns for `power`, `bm`, and
  `zip`.
- Some functions in `insight` are renamed, therefore
  `insight::format_table()` is replaced with `insight::export_table()`.

## insurancerating 0.6.3

CRAN release: 2020-10-28

- [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/fit_gam.md)
  for pure premium is now using average premium for each x calculated as
  sum(pure_premium \* exposure) / sum(exposure) instead of
  sum(pure_premium) / sum(exposure)
  ([\#2](https://github.com/MHaringa/insurancerating/issues/2)).
- [`histbin()`](https://mharinga.github.io/insurancerating/reference/histbin.md)
  is added to create histograms with outliers
- `reduce` now returns a data.frame as output

## insurancerating 0.6.2

CRAN release: 2020-06-08

- `check_normality()` is now depreciated; use
  [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md)
  instead to detect overall deviations from the expected distribution
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  now shows significance stars for p-values
- [`period_to_months()`](https://mharinga.github.io/insurancerating/reference/period_to_months.md)
  arithmetic operations with dates are rewritten; much faster
- [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
  now has argument `by` to determine summary statistics for different
  subgroups

## insurancerating 0.6.1

CRAN release: 2020-04-29

- `univariate_all()` and `autoplot.univ_all()` are now depreciated; use
  [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
  and `autoplot.univariate()` instead
- [`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md),
  `check_normality()`,
  [`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md),
  [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_rmse.md),
  and
  [`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md)
  are added to test model quality and return performance metrics
- [`reduce()`](https://mharinga.github.io/insurancerating/reference/reduce.md)
  is added to reduce an insurance portfolio by merging redundant date
  ranges

## insurancerating 0.6.0

CRAN release: 2020-04-10

- `label_width` in
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  is added to wrap long labels in multiple lines
- `sort_manual` in
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  is added to sort risk factors into an own ordering
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  now works without manually loading package `ggplot2` and `patchwork`
  first
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  now returns an object of class `riskfactor`
- `autoplot.riskfactor()` is added to create the corresponding plots to
  the output given by
  [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)

## insurancerating 0.5.2

CRAN release: 2020-03-30

- `autoplot.univ_all()` now gives correct labels on the x-axis when
  `ncol` \> 1.

## insurancerating 0.5.1

CRAN release: 2020-03-29

- A package website is added using pkgdown.
- [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md)
  and
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/fit_gam.md)
  now only returns tariff classes and fitted gam respectively; other
  items are stored as attributes.
- `univariate_frequency()`, `univariate_average_severity()`,
  `univariate_risk_premium()`, `univariate_loss_ratio()`,
  `univariate_average_premium()`, `univariate_exposure()`, and
  `univariate_all()` are added to perform an univariate analysis on an
  insurance portfolio.
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  creates the corresponding plots to the summary statistics calculated
  by `univariate_*`.

## insurancerating 0.5.0

CRAN release: 2020-03-12

- [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md)
  is now split in
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/fit_gam.md)
  and
  [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md).
- A vignette is added on how to use the package.

## insurancerating 0.4.3

CRAN release: 2019-11-01

- [`period_to_months()`](https://mharinga.github.io/insurancerating/reference/period_to_months.md)
  is added to split rows with a time period longer than one month to
  multiple rows with a time period of exactly one month each.

## insurancerating 0.4.2

CRAN release: 2019-05-31

- In
  [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md),
  `model` now also accepts ‘severity’ as specification.
