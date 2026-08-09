# Split portfolio periods into calendar months

Split policy periods that cross calendar-month boundaries into separate
monthly records. Numeric amounts such as earned exposure and earned
premium can be allocated over those records while preserving the amount
of each original portfolio row.

## Usage

``` r
split_periods_to_months(
  data = NULL,
  period_start = NULL,
  period_end = NULL,
  prorate_cols = NULL,
  df = NULL,
  begin = NULL,
  end = NULL,
  cols = NULL
)
```

## Arguments

- data:

  A `data.frame` or `data.table` containing policy or exposure periods.

- period_start:

  Character string. Name of the column with policy period start dates.

- period_end:

  Character string. Name of the column with policy period end dates.

- prorate_cols:

  Character vector with names of numeric columns to prorate over the
  monthly rows, for example exposure or premium.

- df, begin, end, cols:

  Deprecated argument names kept for backward compatibility. Use `data`,
  `period_start`, `period_end`, and `prorate_cols`.

## Value

A regular `data.frame` with one row for each calendar month covered by
an original portfolio record. The original columns are retained and an
`id` column identifies the source row. Columns supplied through
`prorate_cols` contain their allocated monthly amounts.

## Details

Pricing, reserving and monitoring analyses often require exposure and
premium by calendar month, whereas policy administration data generally
contains periods with arbitrary start and end dates. The function
converts those periods into a monthly representation before aggregation,
modelling or reporting.

This is a temporal expansion rather than a portfolio reduction. See
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
for consolidating connected periods and
[`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
for matching dated events to active periods.

Prorated columns are distributed according to the part of the policy
period represented by each monthly row. Full months receive weight 1 and
partial months use a 30-day convention. The monthly weights are
normalised within each source row. Consequently, monthly exposure and
premium sum to their original values, including for periods that contain
partial months.

Column names are supplied as character strings, for example
`period_start = "begin_date"`. The deprecated
[`period_to_months()`](https://mharinga.github.io/insurancerating/reference/period_to_months.md)
interface used unquoted column names and is retained only for backward
compatibility.

Expansion and proration are performed internally with
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
on a local copy. The supplied object is not modified by reference, and
the returned object is always a regular `data.frame`.

## See also

[`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md),
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md),
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  policy_id = c("P001", "P002", "P003"),
  sector = c("Industry", "Retail", "Services"),
  coverage_start = as.Date(c("2025-01-15", "2025-02-01", "2025-03-20")),
  coverage_end = as.Date(c("2025-03-31", "2025-02-28", "2025-05-10")),
  earned_exposure = c(0.21, 0.08, 0.14),
  earned_premium = c(420, 160, 280)
)

# Allocate each policy period and its amounts over calendar months.
monthly_portfolio <- split_periods_to_months(
  portfolio,
  period_start = "coverage_start",
  period_end = "coverage_end",
  prorate_cols = c("earned_exposure", "earned_premium")
)
monthly_portfolio
#>   id policy_id   sector coverage_start coverage_end earned_exposure
#> 1  1      P001 Industry     2025-01-15   2025-01-31      0.04760000
#> 2  1      P001 Industry     2025-02-01   2025-02-28      0.07840000
#> 3  1      P001 Industry     2025-03-01   2025-03-31      0.08400000
#> 4  2      P002   Retail     2025-02-01   2025-02-28      0.08000000
#> 5  3      P003 Services     2025-03-20   2025-03-31      0.03230769
#> 6  3      P003 Services     2025-04-01   2025-04-30      0.08076923
#> 7  3      P003 Services     2025-05-01   2025-05-10      0.02692308
#>   earned_premium
#> 1       95.20000
#> 2      156.80000
#> 3      168.00000
#> 4      160.00000
#> 5       64.61538
#> 6      161.53846
#> 7       53.84615

# The allocated monthly amounts reconcile to the source portfolio.
aggregate(
  cbind(earned_exposure, earned_premium) ~ id,
  data = monthly_portfolio,
  FUN = sum
)
#>   id earned_exposure earned_premium
#> 1  1            0.21            420
#> 2  2            0.08            160
#> 3  3            0.14            280

# Deprecated interface with unquoted column names
if (FALSE) { # \dontrun{
period_to_months(
  portfolio,
  coverage_start,
  coverage_end,
  earned_exposure,
  earned_premium
)
} # }
```
