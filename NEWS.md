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



