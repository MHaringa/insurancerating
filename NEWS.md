# insurancerating (development version)

### Fisher-Jenks classification (`fisher_classify()` / `fisher()`)
- `fisher_classify()` and `fisher()` are deprecated as of 0.8.0 because
  Fisher-Jenks classification is a general-purpose grouping method and is not
  directly linked to the insurance rating workflow.
- `classInt` moved from `Imports` to `Suggests`.

### Tariff class construction (`construct_tariff_classes()`)
- `construct_tariff_classes()` now returns objects with primary class
  `"tariff_classes"` while retaining `"constructtariffclasses"` for backwards
  compatibility.
- Added clearer control arguments: `complexity`, `max_iterations`, and
  `population_size`. The previous `alpha`, `niterations`, and `ntrees`
  arguments remain supported with lifecycle warnings.
- Fixed split extraction for decimal-valued risk factors.
- `autoplot(..., conf_int = TRUE)` now recognizes the confidence interval
  columns produced by `riskfactor_gam()`.
- Class construction failures now produce explicit errors instead of silently
  returning a single broad interval.

### Risk factor GAMs (`riskfactor_gam()` / `fit_gam()`)
- `riskfactor_gam()` now returns objects with primary class `"riskfactor_gam"`
  while retaining `"fitgam"` for backwards compatibility.
- `model = "pure_premium"` replaces `model = "burning"` as the preferred API.
  The old `"burning"` value remains supported with a lifecycle warning.
- Improved validation for model-specific required inputs, especially severity
  `amount` and pure premium `pure_premium`.
- Documentation now correctly describes severity and pure premium models as
  Gamma GAMs with log link.

### Prediction helpers (`add_prediction()`)
- Added `predictions`, `prefix`, `confidence`, and `interval_names` as clearer
  naming and interval arguments.
- `var` and `conf_int` are deprecated in favour of `predictions` and
  `confidence`.
- Confidence interval columns now use `_lower` and `_upper` suffixes by default.
- Added validation for `alpha`, confidence settings, duplicate output names, and
  name collisions with existing data columns.

### Bootstrap model performance (`bootstrap_performance()`)
- `bootstrap_performance()` now has an explicit `metric = "rmse"` argument.
- Added `sampling = c("bootstrap", "split")` to distinguish bootstrap
  out-of-bag evaluation from train/test split sampling.
- Added validation for `n`, `frac`, `metric`, `sampling`, `show_progress`,
  `rmse_model`, and empty data.
- Character and factor rating variables are handled more robustly across
  resamples so prediction does not fail when a level is absent from an initial
  training draw.
- Deprecated `bootstrap_rmse()` objects now also retain class
  `"bootstrap_rmse"` for backward compatibility.

### Factor analysis (`factor_analysis()` / `univariate()`)
- `factor_analysis()` now returns objects with primary class
  `"factor_analysis"` while retaining `"univariate"` for backwards
  compatibility.
- Added clear validation for metric columns and `by` variables.
- Metrics with zero denominators now return `NA` instead of `Inf` or `NaN`.
- Added `autoplot.factor_analysis()` while keeping `autoplot.univariate()` as a
  compatibility method.
- `autoplot()` now fails clearly when multiple `by` variables are supplied.

### Outlier histograms (`outlier_histogram()` / `histbin()`)
- Added clear validation for inputs, bin counts, cutoffs, colors, and numeric
  data requirements.
- Constant and all-missing variables now fail early instead of producing invalid
  histogram bin widths.
- Removed unused `rlang` imports from the documentation.
- Deprecated `histbin()` now supports old NSE input, direct character input, and
  character column-name variables.

### Model data extraction (`extract_model_data()` / `model_data()`)
- `extract_model_data()` is now the primary API to retrieve cleaned model data
  from `glm`, `refitsmooth`, and `refitrestricted` objects.
- `model_data()` is deprecated as of 0.9.0 and now emits a lifecycle warning; it
  remains as a wrapper.
- `rating_grid()` now uses base R internally and always returns a regular
  `data.frame`.
- Fixed plain GLM metadata extraction so `rating_grid(glm)` groups by model
  terms instead of unrelated original data columns.
- Fixed `exposure_by` output names so split exposure columns use the exposure
  column name, for example `exposure_2020`.
- Refinement metadata is joined by the related original factor column instead
  of being cross-joined onto every rating-grid row.

### update_smoothing()
- Introduces a dedicated helper to update existing smoothing specifications without refitting the full model from scratch.
- Enables faster, more transparent iteration when fine-tuning smoothing curves.

### Rating table output (`rating_table()` / `rating_factors*()`)
- Renamed `rating_factors()` to `rating_table()` as the primary user-facing API.
- `rating_factors()` and `rating_factors2()` are deprecated as of 0.8.0 and now emit lifecycle warnings; they remain as wrappers.

### Histogram functions (`outlier_histogram()` / `histbin()`)
- Added `outlier_histogram()` as the new name for histograms with outlier bins.
- `histbin()` is deprecated as of 0.8.0 and now warns; it remains as a wrapper.
- The `x` argument must now be provided as a **string** (standard evaluation).
- No functional changes to binning or outlier handling.

## Breaking changes
- The function `fit_gam()` has been **deprecated** and replaced by `riskfactor_gam()`.
  - `fit_gam()` used **non-standard evaluation (NSE)**, allowing unquoted column names.
  - `riskfactor_gam()` now uses **standard evaluation (SE)**, requiring column names as **character strings**.  
    Example migration:
    ```r
    # old (NSE, deprecated)
    fit_gam(df, nclaims = nclaims, x = age_policyholder, exposure = exposure)

    # new (SE)
    riskfactor_gam(df, nclaims = "nclaims", x = "age_policyholder", exposure = "exposure")
    ```
  - The NSE wrapper `fit_gam()` is still available but will show a deprecation warning 
    and will be removed in a future release.
    
- The function `univariate()` has been **deprecated** and replaced by `factor_analysis()`.
  - `univariate()` used **non-standard evaluation (NSE)**, allowing unquoted column names.
  - `factor_analysis()` now uses **standard evaluation (SE)**, requiring column names as **character strings**.
    Example migration:
    ```r
    # old (NSE, deprecated)
    univariate(df, x = area, severity = amount, nclaims = nclaims, exposure = exposure)

    # new (SE)
    factor_analysis(df, x = "area", severity = "amount",
                    nclaims = "nclaims", exposure = "exposure")
    ```
  - The NSE wrapper `univariate()` is still available but will show a deprecation warning 
    and will be removed in a future release.


## Minor changes
- Improved documentation for `riskfactor_gam()`, including clearer examples 
  and migration guidance from `fit_gam()`.
  
  

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
