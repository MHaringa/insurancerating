## Resubmission
This is a resubmission. In this version:

* In `summary.reduce()`, `name` can be used to change the name of the new column in the output.
* Dataset `MTPL` now contains extra columns for `power`, `bm`, and `zip`. 
* Some functions in `insight` are renamed, therefore `insight::format_table()` is replaced with `insight::export_table()`.

## Test environments
* local OS X install, R 4.0.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
I have also run R CMD check on downstream dependencies of insurancerating.
All packages that I could install passed.


