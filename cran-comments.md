## Resubmission

This is a resubmission.

The previously reported invalid vignette URI has been fixed.

The accidentally included hidden .github directory has been excluded from the package tarball.

Thank you for your review.

## Release summary

This is a major update of insurancerating. The release modernises the public API
while retaining deprecated compatibility wrappers for older workflows.

Main changes include:

* Added `factor_analysis()` as the primary portfolio analysis function.
* Added tariff segmentation helpers through `risk_factor_gam()`,
  `derive_tariff_segments()` and `add_tariff_segments()`.
* Added a capped-severity excess-loss workflow with
  `assess_excess_threshold()`, `calculate_excess_loss()`,
  `allocate_excess_loss()` and `apply_excess_loading()`.
* Added exploratory severity distribution plots and expanded model validation
  helpers.
* Renamed and documented several APIs for consistency, with deprecated wrappers
  retained where needed.
* Expanded tests and documentation across the package.

## Test environments
* local macOS Tahoe 26.5, R 4.5.3

## R CMD check results

Local check with vignette rebuilding:

0 errors | 0 warnings | 1 note

The note is a local environment note:

* the current time could not be verified locally.

CRAN incoming checks, URL checks, examples, tests, vignettes and manual checks
completed successfully.

## Downstream dependencies
There are no downstream dependencies listed for insurancerating on CRAN.
