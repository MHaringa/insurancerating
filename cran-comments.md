## Resubmission
This is a resubmission. In this version:

* `label_width` in `autoplot()` is added to wrap long labels in multiple lines
* `sort_manual` in `autoplot()` is added to sort risk factors into an own ordering
* `autoplot()` now works without manually loading package `ggplot2` and `patchwork` first
* `rating_factors()` now returns an object of class `riskfactor`
* `autoplot.riskfactor()` is added to create the corresponding plots to the output given by `rating_factors()`

## Test environments
* local OS X install, R 3.6.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
I have also run R CMD check on downstream dependencies of insurancerating.
All packages that I could install passed.


