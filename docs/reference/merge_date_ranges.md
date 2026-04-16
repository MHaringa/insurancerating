# Reduce portfolio by merging redundant date ranges

Transform all the date ranges together as a set to produce a new set of
date ranges. Ranges separated by a gap of at least `min.gapwidth` days
are not merged.

## Usage

``` r
merge_date_ranges(
  df,
  begin,
  end,
  ...,
  agg_cols = NULL,
  agg = "sum",
  min.gapwidth = 5
)

reduce(df, begin, end, ..., agg_cols = NULL, agg = "sum", min.gapwidth = 5)
```

## Arguments

- df:

  A `data.frame` or `data.table`.

- begin:

  Name of column in `df` with begin dates.

- end:

  Name of column in `df` with end dates.

- ...:

  Names of columns in `df` used to group date ranges by.

- agg_cols:

  List of columns in `df` to aggregate (default = NULL).

- agg:

  Aggregation function as character string (default = `"sum"`).

- min.gapwidth:

  Ranges separated by at least `min.gapwidth` days are not merged
  (default = 5).

## Value

A `data.table` of class `"reduce"`, with attributes:

- `begin` — name of the begin-date column

- `end` — name of the end-date column

- `cols` — grouping columns

## Details

This function is inspired by `IRanges::reduce()`, adapted for insurance
portfolios.

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
pt1 <- merge_date_ranges(portfolio, begin = begin_dat, end = end_dat,
    policy_nr, productgroup, product, min.gapwidth = 5)

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
pt2 <- merge_date_ranges(portfolio, begin = begin_dat, end = end_dat,
    policy_nr, productgroup, product, agg_cols = list(premium),
    min.gapwidth = 5)

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
