## Resubmission
This is a resubmission. In this version:

* `autoplot.univariate()` now generates a plot even when there are missing 
values in the rows
* `rating_factors()` now always returns the correct coefficients when used
on a 'refitsmooth' or 'refitrestricted' class of GLM. 

## Test environments
* local OS X install, R 4.4.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
I have also run R CMD check on downstream dependencies of insurancerating.
All packages that I could install passed.


