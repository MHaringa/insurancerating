# Reduce portfolio periods by merging adjacent date ranges

Combine overlapping or nearly adjacent coverage periods for the same
policy, risk or portfolio segment, optionally aggregating amounts such
as premium or exposure. Separate periods remain separate rows.

## Usage

``` r
merge_date_ranges(
  data = NULL,
  ...,
  period_start = NULL,
  period_end = NULL,
  group_by = NULL,
  aggregate_cols = NULL,
  aggregate_fun = "sum",
  merge_gap_days = 1,
  allow_na_in = NULL,
  df = NULL,
  begin = NULL,
  end = NULL,
  agg_cols = NULL,
  agg = NULL,
  min.gapwidth = NULL
)
```

## Arguments

- data:

  A `data.frame` or `data.table` containing the portfolio periods.

- period_start:

  Character string. Name of the column with period start dates (class
  `Date`).

- period_end:

  Character string. Name of the period-end column (class `Date`).

- group_by:

  Character vector with columns that identify the portfolio entity or
  rating segment within which date ranges should be merged.

- aggregate_cols:

  Character vector with numeric columns to aggregate over merged ranges,
  for example premium or exposure.

- aggregate_fun:

  Function or function name used to combine `aggregate_cols` within a
  merged interval. The default, `"sum"`, is generally appropriate for
  additive measures such as premium or exposure.

- merge_gap_days:

  Non-negative whole number. Ranges with fewer uncovered days than this
  value are treated as continuous. The default, `1`, merges overlapping
  periods and periods that start on the day after the preceding period
  ends. Use `0` to merge overlapping periods only. A value above `1`
  also bridges short uncovered gaps and should represent an explicit
  administrative assumption.

- allow_na_in:

  Optional character vector naming columns in `group_by` where missing
  values may form a distinct grouping value. Missing values in any other
  grouping column cause an error. The default, `NULL`, allows none.
  Unlike `drop_na` in
  [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md),
  this argument does not filter rows out.

- df, begin, end, ..., agg_cols, agg, min.gapwidth:

  Deprecated argument names kept for backward compatibility. Use `data`,
  `period_start`, `period_end`, `group_by`, `aggregate_cols`,
  `aggregate_fun`, and `merge_gap_days`.

## Value

A `data.frame` with one row per consolidated period, containing the
grouping columns, dates and requested aggregates. It has classes
`"merged_date_ranges"` and `"reduce"`, and attributes:

- `period_start`: name of the period-start column

- `period_end`: name of the period-end column

- `group_by`: original grouping columns

## Details

### Connected periods

Periods merge only when their `group_by` values match. Include the
policy or risk identifier when unrelated contracts must remain separate.

Dates are inclusive: a period ending on 31 January and one starting on 1
February have no uncovered days and merge by default. With
`merge_gap_days = 5`, gaps of up to four uncovered days are also
bridged.

Use e.g. `allow_na_in = "coverage"` to treat missing coverage as a
distinct group value. Missing values match each other, not known values;
other grouping values and the date-gap rule still apply. Avoid allowing
missing identifiers if this could combine unrelated risks.

### Aggregation

`aggregate_cols` are combined over source rows, not prorated over
calendar days. Sum only distinct additive amounts: merging overlapping
dates does not remove duplicated premium or exposure. Resolve such
duplicates first.

The input is not modified. For database-backed processing, use
[`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md).
To aggregate identical rating-factor combinations rather than connected
periods, use
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md).

### Summaries

[`summary()`](https://rdrr.io/r/base/summary.html) counts consolidated
periods entering and leaving the portfolio. Choose `period = "days"`
(default), `"weeks"`, `"months"`, `"quarters"` or `"years"`, and
optionally rename the count column with `name`. Supply grouping columns
explicitly, e.g. `summary(x, coverage, period = "months")`; the original
`group_by` is not applied automatically. These are period movement
counts, not counts of unique policies.

If you rename a date column, update its `period_start` or `period_end`
attribute so [`summary()`](https://rdrr.io/r/base/summary.html) can find
it. Older saved objects with `begin` and `end` attributes remain
supported when the new attributes are absent.

## See also

[`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md),
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md),
[`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md),
[`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md),
[`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  policy_id = rep(c("P001", "P002"), each = 3),
  coverage = rep(c("Fire", "Liability"), each = 3),
  period_start = as.Date(c(
    "2024-01-01", "2024-02-01", "2024-04-01",
    "2024-01-01", "2024-02-03", "2024-03-03"
  )),
  period_end = as.Date(c(
    "2024-01-31", "2024-02-29", "2024-04-30",
    "2024-01-31", "2024-03-02", "2024-03-31"
  )),
  earned_premium = c(100, 110, 120, 80, 90, 95)
)

# Reduce directly adjacent periods within each policy and coverage.
pt1 <- merge_date_ranges(
  portfolio,
  period_start = "period_start",
  period_end = "period_end",
  group_by = c("policy_id", "coverage")
)

# Count movements by month, policy and coverage.
summary(pt1, period = "months", policy_id, coverage)
#>     month type count policy_id  coverage
#> 1 2024M05  out     1      P001      Fire
#> 2 2024M04   in     1      P001      Fire
#> 3 2024M04  out     1      P002 Liability
#> 4 2024M03  out     1      P001      Fire
#> 5 2024M02   in     1      P002 Liability
#> 6 2024M02  out     1      P002 Liability
#> 7 2024M01   in     1      P001      Fire
#> 8 2024M01   in     1      P002 Liability

# Bridge a short administrative gap and retain additive premium totals.
pt2 <- merge_date_ranges(
  portfolio,
  period_start = "period_start",
  period_end = "period_end",
  group_by = c("policy_id", "coverage"),
  aggregate_cols = "earned_premium",
  # Explicitly bridge administrative gaps of up to four uncovered days.
  merge_gap_days = 5
)

pt2
#>   policy_id  coverage period_start period_end earned_premium
#> 1      P001      Fire   2024-01-01 2024-02-29            210
#> 2      P001      Fire   2024-04-01 2024-04-30            120
#> 3      P002 Liability   2024-01-01 2024-03-31            265
```
