# insurancerating (development version)

## Overview of changes since 0.8.0

- The excess-loss workflow has been made more portfolio-oriented and easier to
  audit. Threshold assessments now preserve input column names, support
  portfolio-level claim counts and can be presented with `as_gt()`.
  `calculate_excess_loss()` returns an enriched data frame with dynamically
  named capped and excess columns, while `allocate_excess_loss()` returns the
  original portfolio rows with clearly named allocation results.
- Excess-loss allocation now distinguishes explicitly between rows that
  contribute observed excess losses and rows that receive an allocation.
  Portfolio, risk-factor and partial allocation use a consistent output
  structure, transparent credibility weighting and optional bootstrap
  uncertainty. Allocation summaries preserve the original risk-factor,
  claim-count and allocation-weight column names.
- `apply_excess_loading()` now has a smaller API that directly consumes the
  standard output of `allocate_excess_loss()`. It supports both premium amounts
  and rates through `output`, `base_value` and `allocation_weight` without
  requiring users to specify internal allocation-column names.
- `add_portfolio_experience()` is now the primary helper for attaching observed
  portfolio experience to `rating_table()` objects. The former
  `add_observed_experience()` interface remains available as a deprecated
  compatibility wrapper.
- The documentation for `rating_table()`, `add_prediction()` and the excess-loss
  workflow has been expanded with more applied portfolio examples. The pkgdown
  configuration and website build workflow have also been updated.

## Main API updates

### Rating tables

- `add_portfolio_experience()` is now the primary API for enriching a
  `rating_table()` object with observed portfolio experience.
- `add_observed_experience()` is deprecated and remains available as a
  compatibility wrapper.
- `add_portfolio_experience()` can calculate portfolio experience
  automatically from portfolio data for all risk factors in a `rating_table()`,
  and accepts multiple existing `factor_analysis()` objects.
- `autoplot.rating_table()` accepts `metric` to choose the attached portfolio
  experience metric at plot time.

### Excess-loss workflow

- `assess_excess_threshold()` now returns class `"threshold_assessment"`,
  preserves the original group and exposure column names, supports an optional
  claim-count column and uses `as_gt()` for report-ready threshold comparisons.
- `calculate_excess_loss()` now returns a regular enriched data frame. Capped,
  excess and indicator column names are derived from the supplied claim-amount
  column, and metadata allows the next allocation step to find them
  automatically.
- `allocate_excess_loss()` now returns the original portfolio data with class
  `"excess_allocation"` and appended allocation columns. The
  `receives_allocation` argument controls which rows receive a share without
  excluding their observed excess losses from the total being allocated.
- Allocation output now distinguishes risk-factor, portfolio and blended excess
  loadings from the row-level `expected_excess_loss`. The allocation summary
  retains original input column names and provides an auditable comparison of
  observed and allocated excess loss.
- `apply_excess_loading()` now uses the compact API `output`, `base_value` and
  `allocation_weight`. It automatically reads `expected_excess_loss` and
  `blended_excess_loading` from the allocation object.

# insurancerating 0.8.0

## Main API updates

### Portfolio analysis

- `factor_analysis()` is now the primary function for univariate/factor-level
  portfolio analysis. It returns objects with primary class `"factor_analysis"`
  while retaining `"univariate"` for compatibility.
- `plot_severity_distribution()` was added for exploratory severity diagnostics
  by category. It shows individual claim observations with mean and median
  markers, optional direct labels, and optional firebrick highlighting for
  claims above a user-supplied threshold.
- `univariate()` is deprecated and remains available as a compatibility wrapper.
  The old NSE interface is still supported through the deprecated wrapper.
- `factor_analysis()` now validates metric columns and grouping variables early,
  with clearer error messages for missing columns.
- Metrics with a zero or missing denominator now return `NA_real_` instead of
  `Inf` or `NaN`.
- `autoplot.factor_analysis()` is the primary plot method. The deprecated
  `show_plots` argument has been replaced by `metrics`.
- The factor-analysis plot keeps the established package styling and now uses a
  consistent grid, axis-line, tick and secondary-axis style.

### Outlier histograms

- `outlier_histogram()` has clearer argument names:
  `lower`, `upper`, `density`, `bar_fill`, `bar_color`, `tail_fill`,
  `tail_color`, and `density_color`.
- Deprecated argument names remain supported: `left`, `right`, `line`, `fill`,
  `color`, and `fill_outliers`.
- The default colours now align with the package palette: light grey bars,
  orange tail bins, and blue density curve.
- Input validation has been expanded for missing columns, non-numeric
  variables, invalid cutoffs, invalid colours, invalid bin counts, constant
  variables and all-missing variables.
- `histbin()` is deprecated and remains available as a compatibility wrapper.

### Risk factor GAMs and tariff segmentation

- `risk_factor_gam()` is the primary spelling for fitting GAMs to continuous
  risk factors. `riskfactor_gam()` and `fit_gam()` remain available for
  compatibility, with `fit_gam()` deprecated.
- `risk_factor_gam()` returns objects with primary class `"risk_factor_gam"`;
  compatibility classes are retained for older code.
- `model = "pure_premium"` replaces the older `model = "burning"` wording. The
  old value remains supported with a lifecycle warning.
- Input validation and documentation for frequency, severity and pure-premium
  GAMs have been improved.
- `derive_tariff_segments()` replaces `construct_tariff_classes()` as the
  primary API for deriving tariff segments from a fitted risk-factor GAM.
- `derive_tariff_segments()` returns objects with primary class
  `"tariff_segments"`.
- `add_tariff_segments()` can add derived tariff segments back to a portfolio.
- `construct_tariff_classes()` remains available as a deprecated compatibility
  wrapper.
- Split extraction now handles decimal split points correctly.
- Tree-fitting and split-extraction failures now fail clearly instead of
  silently returning one broad tariff interval.
- The tariff-segment plot now recognises confidence interval columns produced
  by the package's own GAM output.

### Rating tables

- `rating_table()` is the primary API for interpreting fitted GLM coefficients
  in tariff-table form. `rating_factors()` and `rating_factors2()` are
  deprecated wrappers.
- `rating_table()` now returns objects with primary class `"rating_table"` while
  retaining the legacy `"riskfactor"` class for compatibility.
- `exposure_output` replaces the older `exposure_name` argument.
- `significance` replaces the older `signif_stars` argument.
- Deprecated rating-table wrappers and plotting code have been separated more
  clearly in the source structure.
- `add_observed_experience()` was added to attach `factor_analysis()` output to
  a `rating_table()` object before plotting. This replaces the earlier direct
  `univariate_*` arguments in `autoplot.rating_table()`.
- `autoplot.rating_table()` now plots attached observed experience from
  `add_observed_experience()` and uses cleaner, package-consistent plot
  styling, including a subtle secondary exposure axis.

### Prediction helpers

- `add_prediction()` now has clearer naming arguments: `predictions`, `prefix`,
  `confidence`, and `interval_names`.
- `var` and `conf_int` are deprecated in favour of `predictions` and
  `confidence`.
- Confidence interval columns now use `_lower` and `_upper` suffixes by default.
- The function now validates `alpha`, confidence settings, duplicate output
  names, name collisions with existing columns, missing models and non-GLM
  inputs.

### Model data and rating grids

- `extract_model_data()` replaces `model_data()` as the primary API for
  extracting model data from fitted models.
- `model_data()` is deprecated and remains available as a wrapper.
- `rating_grid()` now uses base R internally and returns a regular
  `data.frame`.
- Plain GLM metadata extraction has been improved so `rating_grid(glm)` groups
  by model terms as expected.
- Refinement metadata is now joined by the related original/new factor columns
  instead of being cross-joined onto every rating-grid row.

### Model refinement

- The refinement API has been clarified around
  `prepare_refinement() |> add_*() |> refit()`.
- A new excess-loss workflow was added for capped severity and pure-premium
  modelling: `assess_excess_threshold()`, `calculate_excess_loss()`,
  `allocate_excess_loss()` and `apply_excess_loading()`.
- `assess_excess_threshold()` compares candidate large-loss thresholds and shows
  the impact on excess loss, capped loss and pure premium.
- `calculate_excess_loss()` now performs only the deterministic historical
  decomposition into capped and excess claim amounts.
- `allocate_excess_loss()` handles allocation and bootstrap uncertainty
  modelling. It supports observed or bootstrap excess burdens, portfolio,
  risk-factor and partial allocation, and optional severity noise in the
  bootstrap.
- `allocate_excess_loss()` now uses clearer allocation argument names:
  `allocation_weight`, `risk_factor`, `receives_allocation`, `allocation`,
  `n_bootstrap`, `bootstrap_seed` and `preserve_total_excess`.
- Automatic credibility in `allocate_excess_loss()` now uses the transparent
  formula `Z = n / (n + credibility_threshold)` with
  `credibility_basis = "claims"`, `"excess_claims"` or `"allocation_weight"`.
- `allocate_excess_loss()` now uses `preserve_total_excess = TRUE` by default so that
  partial allocation redistributes the selected excess burden without changing the
  total allocated excess loss.
- `apply_excess_loading()` adds the allocated excess loading to pricing data and
  returns `base_premium`, `excess_loading` and `loaded_premium`.
- `apply_excess_loading()` now treats premium amounts as the default workflow and
  keeps the distinction between absolute `expected_excess_loss` and per-weight
  `blended_excess_loading` explicit.
- `add_smoothing()` now uses `model_variable` and `source_variable` as the
  primary argument names.
- `edit_smoothing()` now uses clearer in-object editing arguments for adjusting
  smoothing settings without supplying an external data frame.
- `add_restriction()` can now accept a partial restriction data frame. Missing
  levels are automatically filled with the already fitted GLM relativities, so
  users can adjust only selected levels.
- `add_relativities()` now uses `model_variable` and `split_variable`.
- `relativities()` replaces `relativities_list()` as the helper for building
  relativity specifications.
- `restrict_coef()`, `smooth_coef()` and `refit_glm()` remain deprecated
  compatibility wrappers and now link clearly to `add_restriction()`,
  `add_smoothing()` and `refit()`.
- `autoplot.rating_refinement()` no longer carries an experimental badge and
  uses the package plot theme.
- The refinement documentation has been expanded with applied examples and a
  clearer explanation of smoothing, restrictions, relativities and refitting.

### Reference levels

- `set_reference_level()` replaces `biggest_reference()` as the primary helper
  for choosing factor reference levels.
- The default method is `method = "largest_weight"`.
- A manual `level` argument was added so a specific reference level can be
  selected explicitly.
- `biggest_reference()` remains available as a deprecated compatibility wrapper.

### Time utilities

- `split_periods_to_months()`, `merge_date_ranges()` and
  `active_rows_by_date()` now avoid mutating caller-visible input data.
- `active_rows_by_date()` replaces `rows_per_date()` as the primary API for
  matching event dates, such as claim dates, to active portfolio rows.
- `period_to_months()`, `rows_per_date()` and `reduce()` remain available as
  deprecated compatibility wrappers.
- Date interval validation, column validation, aggregation validation,
  `nomatch` validation and `mult` validation have been improved.
- R CMD check notes from data.table helper columns in `active_rows_by_date()`
  have been resolved.

### Model validation and performance

- `bootstrap_performance()` now has an explicit `metric = "rmse"` argument.
- `sampling = c("bootstrap", "split")` was added to distinguish bootstrap
  out-of-bag evaluation from split validation.
- Deprecated arguments `n` and `frac` remain supported as aliases for
  `n_resamples` and `sample_fraction`.
- Character and factor rating variables are handled more robustly across
  resamples so prediction does not fail when a level is absent from a training
  sample.
- `autoplot.bootstrap_performance()` now uses a package-consistent visual style:
  subtle grey histogram, transparent blue density, orange original-model
  reference line, subtle confidence interval lines and no gap between the bars
  and x-axis.
- `bootstrap_rmse()` remains available as a deprecated compatibility wrapper and
  returned objects retain class `"bootstrap_rmse"` for older code.
- `check_overdispersion()` now validates non-GLM input, checks for Poisson
  models and fails clearly when residual degrees of freedom are not positive.
- `print.overdispersion()` now bases its conclusion on the original p-value
  rather than a rounded display value.
- `check_residuals()` now validates inputs, uses all scaled residuals for the
  KS test, handles empty residual vectors clearly and documents the DHARMa-based
  residual workflow for actuarial users.
- `autoplot.check_residuals()` now has a controllable `max_points` argument and
  uses ASCII messages and the package plot theme.

### Truncated severity distributions

- `fit_truncated_severity()` replaces `fit_truncated_dist()` as the primary API
  for fitting distributions to truncated claim severities.
- Returned objects use primary class `"truncated_severity"` while compatibility
  with `"truncated_dist"` is retained.
- `fit_truncated_dist()` remains available as a deprecated compatibility
  wrapper.
- Observations outside the truncation interval now fail clearly instead of only
  warning and continuing.
- Validation has been expanded for truncation bounds, optimisation starts,
  grid sizes, reporting options and random generator arguments.
- Public random generators `rlnormt()` and `rgammat()` now validate sample size,
  distribution parameters, finite intervals and positive truncation mass.
- Plot argument names were modernised to `ecdf_geom`, `x_label`, `y_label`,
  `show_title`, `digits` and `truncation_digits`, with old names supported for
  compatibility.

### Fisher-Jenks classification

- `fisher_classify()` and `fisher()` are deprecated because Fisher-Jenks
  classification is a general-purpose grouping method and is not directly tied
  to the insurance-rating workflow.
- `classInt` moved from `Imports` to `Suggests`.

## Documentation, website and tests

- The README and vignettes have been revised to present the package as a set of
  actuarial pricing building blocks rather than a prescribed pricing method.
- The former "Pricing principles" vignette was replaced by "Pricing workflow
  building blocks".
- The refinement vignette was rewritten with a more practical tone and current
  API examples.
- pkgdown reference sections were reorganised; deprecated functions are grouped
  under "Deprecated" and internal S3 methods are no longer listed as primary
  reference topics.
- New and expanded tests cover tariff segmentation, rating tables, observed
  experience plotting, refinement workflows, model-data extraction, model
  performance, overdispersion, residual checks, outlier histograms, truncated
  distributions, time utilities and factor analysis.

# insurancerating 0.7.5

* `rating_factors()` now always returns correct output when column with exposure in data is not named `exposure`
* `intercept_only` in `update_glm()` is added to apply the manual changes and refit the intercept, ensuring that the changes have no impact on the other variables.
* `smoothing` in `smooth_coef()` is added to choose smoothing specification
* The README has been revised

# insurancerating 0.7.4

* `bootstrap_rmse()` now uses `after_stat(density)` instead of the deprecated dot-dot notation
* `custom_theme` in `autoplot.univariate()` is added to customize the theme

# insurancerating 0.7.3

* `autoplot.univariate()` now generates a plot even when there are missing values in the rows
* `rating_factors()` now always returns the correct coefficients when used
on a 'refitsmooth' or 'refitrestricted' class of GLM. 

# insurancerating 0.7.2

* `update_glm()` now always returns the correct interval in case the function is used in combination with `smooth_coef()`

# insurancerating 0.7.1

* `rotate_angle` in `autoplot.univariate()` is added to rotate x-labels
* `univariate()` now accepts external vectors for `x`; `vec_ext()` must be used

# insurancerating 0.7.0

* `smooth_coef()` now gives correct results for intervals with scientific notation
* `reduce()` now returns no errors anymore for columns with dates in POSIXt format

# insurancerating 0.6.9

* `refit_glm()` is renamed to `update_glm()`
* `construct_model_points()` and `model_data()` are added to create model points 

# insurancerating 0.6.8

* `show_total` in `autoplot.univariate()` is added to add line for total of groups in case `by` is used in `univariate()`; `total_color` can be used to change the color of the line, and `total_name` is added to change the name of the legend for the line
* `rating_factors()` now accepts GLMs with an intercept only
* `fit_truncated_dist()` is added to fit the original distribution (gamma, lognormal) from truncated severity data
* `join_to_nearest()` now returns NA in case NA is used as input

# insurancerating 0.6.7

* `smooth_coef()` now returns an error message when intervals are not obtained by cut()
* `get_data()` is added to return the data used in `refit_glm()`

# insurancerating 0.6.6

* `summary.reduce()` now gives correct aggregation for periods "months" and "quarters" 
* `rows_per_date()` is added to determine active portfolio for a certain date

# insurancerating 0.6.5

* `smooth_coef()` and `restrict_coef()` are added for model refinement
* `histbin()` now uses darkblue as default fill color 

# insurancerating 0.6.4

* In `summary.reduce()`, `name` can be used to change the name of the new column in the output.
* Dataset `MTPL` now contains extra columns for `power`, `bm`, and `zip`. 
* Some functions in `insight` are renamed, therefore `insight::format_table()` is replaced with `insight::export_table()`.

# insurancerating 0.6.3

* `fit_gam()` for pure premium is now using average premium for each x calculated as sum(pure_premium * exposure) / sum(exposure) instead of sum(pure_premium) / sum(exposure) (#2).
* `histbin()` is added to create histograms with outliers
* `reduce` now returns a data.frame as output 

# insurancerating 0.6.2

* `check_normality()` is now depreciated; use `check_residuals()` instead to detect overall deviations from the expected distribution
* `rating_factors()` now shows significance stars for p-values
* `period_to_months()` arithmetic operations with dates are rewritten; much faster
* `univariate()` now has argument `by` to determine summary statistics for different subgroups 

# insurancerating 0.6.1

* `univariate_all()` and `autoplot.univ_all()` are now depreciated; use `univariate()` and `autoplot.univariate()` instead
* `check_overdispersion()`, `check_normality()`, `model_performance()`, `bootstrap_rmse()`, and `add_prediction()` are added to test model quality and return performance metrics
* `reduce()` is added to reduce an insurance portfolio by merging redundant date ranges

# insurancerating 0.6.0

* `label_width` in `autoplot()` is added to wrap long labels in multiple lines
* `sort_manual` in `autoplot()` is added to sort risk factors into an own ordering
* `autoplot()` now works without manually loading package `ggplot2` and `patchwork` first
* `rating_factors()` now returns an object of class `riskfactor`
* `autoplot.riskfactor()` is added to create the corresponding plots to the output given by `rating_factors()`

# insurancerating 0.5.2

* `autoplot.univ_all()` now gives correct labels on the x-axis when `ncol` > 1. 

# insurancerating 0.5.1

* A package website is added using pkgdown.
* `construct_tariff_classes()` and `fit_gam()` now only returns tariff classes and fitted gam respectively; other items are stored as attributes.
* `univariate_frequency()`, `univariate_average_severity()`, `univariate_risk_premium()`, `univariate_loss_ratio()`, `univariate_average_premium()`, `univariate_exposure()`, and `univariate_all()` are added to perform an univariate analysis on an insurance portfolio.
* `autoplot()` creates the corresponding plots to the summary statistics calculated by `univariate_*`.

# insurancerating 0.5.0

* `construct_tariff_classes()` is now split in `fit_gam()` and `construct_tariff_classes()`.
* A vignette is added on how to use the package.

# insurancerating 0.4.3
 
* `period_to_months()` is added to split rows with a time period longer than one month to multiple rows with a time period of exactly one month each.

# insurancerating 0.4.2

* In `construct_tariff_classes()`, `model` now also accepts 'severity' as specification.   
