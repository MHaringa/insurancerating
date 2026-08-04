# Deprecated alias for `active_rows_by_date()`

`rows_per_date()` is deprecated as of version 0.9.0. Use
[`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
instead.

## Usage

``` r
rows_per_date(
  df,
  dates,
  df_begin,
  df_end,
  dates_date,
  ...,
  nomatch = NULL,
  mult = "all"
)
```

## Arguments

- df:

  Deprecated. Use `portfolio` instead.

- dates:

  A `data.frame` or `data.table` with event or snapshot dates.

- df_begin:

  Deprecated NSE argument. Use `period_start` instead.

- df_end:

  Deprecated NSE argument. Use `period_end` instead.

- dates_date:

  Deprecated NSE argument. Use `date` instead.

- ...:

  Deprecated NSE join columns. Use `by` instead.

- nomatch:

  Controls event dates for which no active portfolio row is found. With
  `NULL`, unmatched events are omitted. With `NA`, they are retained
  with missing portfolio information.

- mult:

  Controls the result when an event date matches multiple active
  portfolio rows. Use `"all"` to retain every match, or `"first"` or
  `"last"` to retain one matching row. The default is `"all"`.

## Value

See
[`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md).
