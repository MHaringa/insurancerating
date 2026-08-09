# Reduce portfolio periods by merging adjacent date ranges

Combine overlapping or nearly adjacent coverage periods for the same
policy, risk or portfolio segment. The result provides a consolidated
time basis for exposure calculations, active-policy counts and
period-based reporting.

Together with
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md),
this function belongs to the portfolio reduction workflow. Both
functions reduce row-level portfolio data while retaining selected
totals. `merge_date_ranges()` reduces temporally connected records;
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
reduces records with identical risk-factor values.

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

  Character string. Name of the column with period start dates.

- period_end:

  Character string. Name of the column with period end dates.

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

- df, begin, end, ..., agg_cols, agg, min.gapwidth:

  Deprecated argument names kept for backward compatibility. Use `data`,
  `period_start`, `period_end`, `group_by`, `aggregate_cols`,
  `aggregate_fun`, and `merge_gap_days`.

## Value

A regular `data.frame` with classes `"merged_date_ranges"` and
`"reduce"`, and attributes:

- `begin` — name of the period-start column

- `end` — name of the period-end column

- `cols` — grouping columns

## Details

### Portfolio reduction

`merge_date_ranges()` performs temporal portfolio reduction. It combines
connected periods within the same `group_by` values and retains the
selected additive amounts. Use
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
for the complementary categorical reduction of records with identical
risk-factor combinations.

### Connected periods

Insurance portfolio extracts often contain multiple rows for the same
policy or risk because of renewals, endorsements, product changes, or
short administrative gaps. Before calculating portfolio in/outflow,
active exposure windows, or policy counts, it can be useful to reduce
those rows to stable coverage intervals.

`merge_date_ranges()` merges date ranges within each `group_by`
combination. Ranges with a gap smaller than `merge_gap_days` are treated
as one continuous interval. If `aggregate_cols` is supplied, those
columns are aggregated over the merged interval. The grouping columns
should identify records for which combining coverage periods is
actuarially and operationally meaningful; periods belonging to different
risks or contracts should not be pooled. Missing values are not
permitted in `group_by`, because such records cannot be assigned
reliably to the same policy, risk or segment.

Start and end dates are treated as inclusive. Consequently, two periods
for which the second starts one day after the first ends have zero
uncovered days. They are merged with the default `merge_gap_days = 1`. A
value of `5` merges periods with at most four uncovered days between
them.

### Aggregation and implementation

Aggregated amounts are combined over the source rows, not prorated over
calendar days. Summing premium or exposure is appropriate when each
source row contains a distinct additive amount. If overlapping rows
already contain amounts for the same covered days, users should resolve
that overlap before aggregation to avoid double counting.

Each output row represents one consolidated period within a `group_by`
combination. This is a temporal reduction of the portfolio; records with
the same risk-factor values are not combined when their periods remain
separate.

Internally, interval construction and aggregation use
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
on a local copy. The supplied object is not modified by reference. For a
portfolio that should remain outside R memory, use
[`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md)
to perform the same interval reduction in DuckDB.

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

summary(pt2, period = "months", policy_id, coverage)
#>     month type count policy_id  coverage
#> 1 2024M05  out     1      P001      Fire
#> 2 2024M04   in     1      P001      Fire
#> 3 2024M04  out     1      P002 Liability
#> 4 2024M03  out     1      P001      Fire
#> 5 2024M01   in     1      P001      Fire
#> 6 2024M01   in     1      P002 Liability
```
