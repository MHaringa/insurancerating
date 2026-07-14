# Deprecated alias for `split_periods_to_months()`

`period_to_months()` is deprecated as of version 0.8.0. Use
[`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
instead.

## Usage

``` r
period_to_months(df, begin, end, ...)
```

## Arguments

- df:

  A `data.frame` or `data.table`.

- begin:

  Deprecated NSE argument. Use `period_start` instead.

- end:

  Deprecated NSE argument. Use `period_end` instead.

- ...:

  Deprecated NSE columns to prorate. Use `prorate_cols` instead.

## Value

See
[`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md).
