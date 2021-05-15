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



