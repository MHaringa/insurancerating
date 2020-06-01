## Resubmission
This is a resubmission. In this version:

* "Warning: S4 exports specified in 'NAMESPACE' but not defined in package 'insurancerating'" is (hopefully) solved by removing the unused MASS package from the NAMESPACE
* `rating_factors()` now shows significance stars for p-values
* `period_to_months()` arithmetic operations with dates are rewritten (much faster)

## Test environments
* local OS X install, R 4.0.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
I have also run R CMD check on downstream dependencies of insurancerating.
All packages that I could install passed.


