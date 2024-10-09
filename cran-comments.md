## Resubmission
This is a resubmission. In this version:

* `rating_factors()` now always returns correct output when column with exposure in data is not named `exposure`
* `intercept_only` in `update_glm()` is added to apply the manual changes and refit the intercept, ensuring that the changes have no impact on the other variables.
* `smoothing` in `smooth_coef()` is added to choose smoothing specification
* The README has been revised

## Test environments
* local OS X install, R 4.4.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
I have also run R CMD check on downstream dependencies of insurancerating.
All packages that I could install passed.


