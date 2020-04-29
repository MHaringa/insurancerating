## Resubmission
This is a resubmission. In this version:

* `univariate_all()` and `autoplot.univ_all()` are now depreciated; use `univariate()` and `autoplot.univariate()` instead
* `check_overdispersion()`, `check_normality()`, `model_performance()`, `bootstrap_rmse()`, and `add_prediction()` are added to test model quality and return performance metrics
* `reduce()` is added to reduce an insurance portfolio by merging redundant date ranges

## Test environments
* local OS X install, R 4.0.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
I have also run R CMD check on downstream dependencies of insurancerating.
All packages that I could install passed.


