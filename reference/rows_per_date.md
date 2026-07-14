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

  When a row (with interval say, `[a,b]`) in x has no match in y,
  nomatch=NA means NA is returned for y's non-by.y columns for that row
  of x. nomatch=NULL (default) means no rows will be returned for that
  row of x.

- mult:

  When multiple rows in y match to the row in x, `mult` controls which
  values are returned - "all" (default), "first" or "last".

## Value

See
[`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md).
