## Resubmission
This is a resubmission. In this version:

* `show_total` in `autoplot.univariate()` is added to add line for total of groups in case `by` is used in `univariate()`; `total_color` can be used to change the color of the line, and `total_name` is added to change the name of the legend for the line
* `rating_factors()` now accepts GLMs with an intercept only
* `fit_truncated_dist()` is added to fit the original distribution (gamma, lognormal) from truncated severity data
* `join_to_nearest()` now returns NA in case NA is used as input

## Test environments
* local OS X install, R 4.1.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
I have also run R CMD check on downstream dependencies of insurancerating.
All packages that I could install passed.


