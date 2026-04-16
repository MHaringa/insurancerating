# Summarize reduce objects

Method for [`summary()`](https://rdrr.io/r/base/summary.html) applied to
objects of class `"reduce"`, as produced by
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md).
It counts how many customers (or policies) are new or lost within a
given period, optionally grouped by other columns.

## Usage

``` r
# S3 method for class 'reduce'
summary(object, ..., period = "days", name = "count")
```

## Arguments

- object:

  An object of class `"reduce"`, created by
  [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md).

- ...:

  Names of columns in `object` to aggregate counts by.

- period:

  Character string indicating the aggregation period. Options are
  `"quarters"`, `"months"`, `"weeks"`, or `"days"` (default = `"days"`).

- name:

  Character string: name of the new count column in the output. Defaults
  to `"count"`.

## Value

A `data.frame` containing aggregated counts of new (`"in"`) and lost
(`"out"`) records, per chosen period (and per grouping variables if
supplied).

## See also

[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pt <- merge_date_ranges(portfolio, begin = begin_dat, end = end_dat,
                        policy_nr, productgroup, product)
summary(pt, period = "months", policy_nr, productgroup)
} # }
```
