# Deprecated alias for `merge_date_ranges()`

`reduce()` is deprecated as of version 0.8.0. Use
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
instead.

## Usage

``` r
reduce(df, begin, end, ..., agg_cols = NULL, agg = "sum", min.gapwidth = 5)
```

## Arguments

- df:

  A `data.frame` or `data.table`.

- begin:

  Deprecated NSE argument. Use `period_start` instead.

- end:

  Deprecated NSE argument. Use `period_end` instead.

- ...:

  Deprecated NSE grouping columns. Use `group_by` instead.

- agg_cols:

  Deprecated NSE argument. Use `aggregate_cols` instead.

- agg:

  Deprecated. Use `aggregate_fun` instead.

- min.gapwidth:

  Deprecated. Use `merge_gap_days` instead.

## Value

See
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md).
