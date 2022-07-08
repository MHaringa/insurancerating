## Resubmission
This is a resubmission. In this version:

* `smooth_coef()` now gives correct results for intervals with scientific notation
* `reduce()` now returns no errors anymore for columns with dates in POSIXt format

## Test environments
* local OS X install, R 4.2.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
I have also run R CMD check on downstream dependencies of insurancerating.
All packages that I could install passed.


