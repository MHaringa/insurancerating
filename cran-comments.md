## Release summary

This is a new release of insurancerating, version 0.8.2.

Main changes include:

* Extended the model-refinement workflow with smoothing edits, shrinkage,
  rebasing, calibration, replacement restrictions and auditable refinement
  summaries.
* Added interpretation tools for continuous smoothing effects, including
  premium-change summaries and incremental-change plots.
* Added database-backed portfolio reduction with `rating_grid_db()` and
  `merge_date_ranges_db()` for large insurance portfolios.
* Extended rating tables with configurable ordering, estimate names,
  significance output and portfolio-experience comparisons.
* Revised the package documentation and vignettes to provide a consistent,
  modular actuarial workflow without prescribing a single methodology.

## Test environments

* local macOS Tahoe 26.5.2, R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are no downstream dependencies listed for insurancerating on CRAN.
