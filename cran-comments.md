## Resubmission
This is a resubmission. In this version:

* `fit_gam()` for pure premium is now using average premium for each x calculated as sum(pure_premium * exposure) / sum(exposure) instead of sum(pure_premium) / sum(exposure) (#2).
* `histbin()` is added to create histograms with outliers
* `reduce` now returns a data.frame as output 

## Test environments
* local OS X install, R 4.0.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
I have also run R CMD check on downstream dependencies of insurancerating.
All packages that I could install passed.


