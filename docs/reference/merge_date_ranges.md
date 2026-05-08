# Reduce portfolio periods by merging adjacent date ranges

Merges overlapping or nearly adjacent policy periods within portfolio
groups.

## Usage

``` r
merge_date_ranges(
  df,
  ...,
  period_start = NULL,
  period_end = NULL,
  group_by = NULL,
  aggregate_cols = NULL,
  aggregate_fun = "sum",
  merge_gap_days = 5,
  begin = NULL,
  end = NULL,
  agg_cols = NULL,
  agg = NULL,
  min.gapwidth = NULL
)

reduce(df, begin, end, ..., agg_cols = NULL, agg = "sum", min.gapwidth = 5)
```

## Arguments

- df:

  A `data.frame` or `data.table`.

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

  Aggregation function or function name. Defaults to `"sum"`.

- merge_gap_days:

  Non-negative whole number. Ranges with a gap smaller than this number
  of days are merged. Defaults to 5.

- begin, end, ..., agg_cols, agg, min.gapwidth:

  Deprecated NSE argument names kept for backward compatibility.

## Value

A `data.table` of class `"reduce"`, with attributes:

- `begin` — name of the period-start column

- `end` — name of the period-end column

- `cols` — grouping columns

## Details

Insurance portfolio extracts often contain multiple rows for the same
policy or risk because of renewals, endorsements, product changes, or
short administrative gaps. Before calculating portfolio in/outflow,
active exposure windows, or policy counts, it can be useful to reduce
those rows to stable coverage intervals.

`merge_date_ranges()` merges date ranges within each `group_by`
combination. Ranges with a gap smaller than `merge_gap_days` are treated
as one continuous interval. If `aggregate_cols` is supplied, those
columns are aggregated over the merged interval.

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  policy_nr   = rep("12345", 11),
  productgroup= rep("fire", 11),
  product     = rep("contents", 11),
  begin_dat   = as.Date(c(16709,16740,16801,17410,17440,17805,17897,
                          17956,17987,18017,18262), origin="1970-01-01"),
  end_dat     = as.Date(c(16739,16800,16831,17439,17531,17896,17955,
                          17986,18016,18261,18292), origin="1970-01-01"),
  premium     = c(89,58,83,73,69,94,91,97,57,65,55)
)

# Merge periods
pt1 <- merge_date_ranges(
  portfolio,
  period_start = "begin_dat",
  period_end = "end_dat",
  group_by = c("policy_nr", "productgroup", "product"),
  merge_gap_days = 5
)

# Aggregate per period
summary(pt1, period = "days", policy_nr, productgroup, product)
#>         date type count policy_nr productgroup  product
#> 1 2020-02-01  out     1     12345         fire contents
#> 2 2018-10-01   in     1     12345         fire contents
#> 3 2018-01-01  out     1     12345         fire contents
#> 4 2017-09-01   in     1     12345         fire contents
#> 5 2016-02-01  out     1     12345         fire contents
#> 6 2015-10-01   in     1     12345         fire contents

# Merge periods and sum premium per period
pt2 <- merge_date_ranges(
  portfolio,
  period_start = "begin_dat",
  period_end = "end_dat",
  group_by = c("policy_nr", "productgroup", "product"),
  aggregate_cols = "premium",
  merge_gap_days = 5
)

# Create summary with aggregation per week
summary(pt2, period = "weeks", policy_nr, productgroup, product)
#> Warning: The default behavior of week() is changing. Previously ('legacy' mode), week numbers advanced every 7th day of the year. The new 'sequential' mode ensures the first week always has 7 days. For example, as.IDate('2023-01-07') returns week 2 in legacy mode but week 1 in sequential mode (week 2 starts on '2023-01-08'). To adopt the new behavior now, set options(datatable.week = 'sequential'). To keep the old results and silence this warning, set options(datatable.week = 'legacy'). See https://github.com/Rdatatable/data.table/issues/2611
#> Warning: The default behavior of week() is changing. Previously ('legacy' mode), week numbers advanced every 7th day of the year. The new 'sequential' mode ensures the first week always has 7 days. For example, as.IDate('2023-01-07') returns week 2 in legacy mode but week 1 in sequential mode (week 2 starts on '2023-01-08'). To adopt the new behavior now, set options(datatable.week = 'sequential'). To keep the old results and silence this warning, set options(datatable.week = 'legacy'). See https://github.com/Rdatatable/data.table/issues/2611
#>      week type count policy_nr productgroup  product
#> 1 2020W06  out     1     12345         fire contents
#> 2 2018W40   in     1     12345         fire contents
#> 3 2018W02  out     1     12345         fire contents
#> 4 2017W35   in     1     12345         fire contents
#> 5 2016W06  out     1     12345         fire contents
#> 6 2015W40   in     1     12345         fire contents
```
