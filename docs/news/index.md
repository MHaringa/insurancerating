# Changelog

## insurancerating (development version)

#### Model data extraction (`extract_model_data()` / `model_data()`)

- Added
  [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  to retrieve cleaned model data from `glm`, `refitsmooth`, and
  `refitrestricted` objects.
- [`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  is deprecated as of 0.8.0 and now emits a lifecycle warning; it
  remains as a wrapper.

#### update_smoothing()

- Introduces a dedicated helper to update existing smoothing
  specifications without refitting the full model from scratch.
- Enables faster, more transparent iteration when fine-tuning smoothing
  curves.

#### Rating table output (`rating_table()` / `rating_factors*()`)

- Renamed
  [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  to
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  as the primary user-facing API.
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  and
  [`rating_factors2()`](https://mharinga.github.io/insurancerating/reference/rating_factors2.md)
  are deprecated as of 0.8.0 and now emit lifecycle warnings; they
  remain as wrappers.

#### Histogram functions (`outlier_histogram()` / `histbin()`)

- Added
  [`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  as the new name for histograms with outlier bins.
- [`histbin()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  is deprecated as of 0.8.0 and now warns; it remains as a wrapper.
- The `x` argument must now be provided as a **string** (standard
  evaluation).
- No functional changes to binning or outlier handling.

### Breaking changes

- The function
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
  has been **deprecated** and replaced by
  [`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md).
  - [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
    used **non-standard evaluation (NSE)**, allowing unquoted column
    names.

  - [`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
    now uses **standard evaluation (SE)**, requiring column names as
    **character strings**.  
    Example migration:

    ``` r
    # old (NSE, deprecated)
    fit_gam(df, nclaims = nclaims, x = age_policyholder, exposure = exposure)

    # new (SE)
    riskfactor_gam(df, nclaims = "nclaims", x = "age_policyholder", exposure = "exposure")
    ```

  - The NSE wrapper
    [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
    is still available but will show a deprecation warning and will be
    removed in a future release.
- The function
  [`univariate()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  has been **deprecated** and replaced by `univariate_summary()`.
  - [`univariate()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
    used **non-standard evaluation (NSE)**, allowing unquoted column
    names.

  - `univariate_summary()` now uses **standard evaluation (SE)**,
    requiring column names as **character strings**.  
    Example migration:

    ``` r
    # old (NSE, deprecated)
    univariate(df, x = area, severity = amount, nclaims = nclaims, exposure = exposure)

    # new (SE)
    univariate_summary(df, x = "area", severity = "amount",
                       nclaims = "nclaims", exposure = "exposure")
    ```

  - The NSE wrapper
    [`univariate()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
    is still available but will show a deprecation warning and will be
    removed in a future release.

### Minor changes

- Improved documentation for
  [`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md),
  including clearer examples and migration guidance from
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md).

## insurancerating 0.7.5

CRAN release: 2024-10-09

- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  now always returns correct output when column with exposure in data is
  not named `exposure`
- `intercept_only` in
  [`update_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
  is added to apply the manual changes and refit the intercept, ensuring
  that the changes have no impact on the other variables.
- `smoothing` in
  [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  is added to choose smoothing specification
- The README has been revised

## insurancerating 0.7.4

CRAN release: 2024-05-20

- [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md)
  now uses `after_stat(density)` instead of the deprecated dot-dot
  notation
- `custom_theme` in
  [`autoplot.univariate()`](https://mharinga.github.io/insurancerating/reference/autoplot.univariate.md)
  is added to customize the theme

## insurancerating 0.7.3

CRAN release: 2024-05-09

- [`autoplot.univariate()`](https://mharinga.github.io/insurancerating/reference/autoplot.univariate.md)
  now generates a plot even when there are missing values in the rows
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  now always returns the correct coefficients when used on a
  ‘refitsmooth’ or ‘refitrestricted’ class of GLM.

## insurancerating 0.7.2

CRAN release: 2022-12-20

- [`update_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
  now always returns the correct interval in case the function is used
  in combination with
  [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)

## insurancerating 0.7.1

CRAN release: 2022-09-06

- `rotate_angle` in
  [`autoplot.univariate()`](https://mharinga.github.io/insurancerating/reference/autoplot.univariate.md)
  is added to rotate x-labels
- [`univariate()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  now accepts external vectors for `x`; `vec_ext()` must be used

## insurancerating 0.7.0

CRAN release: 2022-07-08

- [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  now gives correct results for intervals with scientific notation
- [`reduce()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  now returns no errors anymore for columns with dates in POSIXt format

## insurancerating 0.6.9

CRAN release: 2021-12-11

- [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
  is renamed to
  [`update_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
- [`construct_model_points()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  and
  [`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  are added to create model points

## insurancerating 0.6.8

CRAN release: 2021-11-10

- `show_total` in
  [`autoplot.univariate()`](https://mharinga.github.io/insurancerating/reference/autoplot.univariate.md)
  is added to add line for total of groups in case `by` is used in
  [`univariate()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md);
  `total_color` can be used to change the color of the line, and
  `total_name` is added to change the name of the legend for the line
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  now accepts GLMs with an intercept only
- [`fit_truncated_dist()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_dist.md)
  is added to fit the original distribution (gamma, lognormal) from
  truncated severity data
- `join_to_nearest()` now returns NA in case NA is used as input

## insurancerating 0.6.7

CRAN release: 2021-07-28

- [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  now returns an error message when intervals are not obtained by cut()
- `get_data()` is added to return the data used in
  [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)

## insurancerating 0.6.6

CRAN release: 2021-05-19

- [`summary.reduce()`](https://mharinga.github.io/insurancerating/reference/summary.reduce.md)
  now gives correct aggregation for periods “months” and “quarters”
- [`rows_per_date()`](https://mharinga.github.io/insurancerating/reference/rows_per_date.md)
  is added to determine active portfolio for a certain date

## insurancerating 0.6.5

CRAN release: 2021-03-22

- [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  and
  [`restrict_coef()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  are added for model refinement
- [`histbin()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  now uses darkblue as default fill color

## insurancerating 0.6.4

CRAN release: 2021-01-12

- In
  [`summary.reduce()`](https://mharinga.github.io/insurancerating/reference/summary.reduce.md),
  `name` can be used to change the name of the new column in the output.
- Dataset `MTPL` now contains extra columns for `power`, `bm`, and
  `zip`.
- Some functions in `insight` are renamed, therefore
  [`insight::format_table()`](https://easystats.github.io/insight/reference/format_table.html)
  is replaced with
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html).

## insurancerating 0.6.3

CRAN release: 2020-10-28

- [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
  for pure premium is now using average premium for each x calculated as
  sum(pure_premium \* exposure) / sum(exposure) instead of
  sum(pure_premium) / sum(exposure)
  ([\#2](https://github.com/MHaringa/insurancerating/issues/2)).
- [`histbin()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  is added to create histograms with outliers
- `reduce` now returns a data.frame as output

## insurancerating 0.6.2

CRAN release: 2020-06-08

- `check_normality()` is now depreciated; use
  [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md)
  instead to detect overall deviations from the expected distribution
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  now shows significance stars for p-values
- [`period_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
  arithmetic operations with dates are rewritten; much faster
- [`univariate()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  now has argument `by` to determine summary statistics for different
  subgroups

## insurancerating 0.6.1

CRAN release: 2020-04-29

- `univariate_all()` and `autoplot.univ_all()` are now depreciated; use
  [`univariate()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  and
  [`autoplot.univariate()`](https://mharinga.github.io/insurancerating/reference/autoplot.univariate.md)
  instead
- [`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md),
  `check_normality()`,
  [`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md),
  [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md),
  and
  [`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md)
  are added to test model quality and return performance metrics
- [`reduce()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
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
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  now returns an object of class `riskfactor`
- [`autoplot.riskfactor()`](https://mharinga.github.io/insurancerating/reference/autoplot.riskfactor.md)
  is added to create the corresponding plots to the output given by
  [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)

## insurancerating 0.5.2

CRAN release: 2020-03-30

- `autoplot.univ_all()` now gives correct labels on the x-axis when
  `ncol` \> 1.

## insurancerating 0.5.1

CRAN release: 2020-03-29

- A package website is added using pkgdown.
- [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md)
  and
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
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
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
  and
  [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md).
- A vignette is added on how to use the package.

## insurancerating 0.4.3

CRAN release: 2019-11-01

- [`period_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
  is added to split rows with a time period longer than one month to
  multiple rows with a time period of exactly one month each.

## insurancerating 0.4.2

CRAN release: 2019-05-31

- In
  [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md),
  `model` now also accepts ‘severity’ as specification.
