# Deprecated alias for `rating_grid()`

`construct_model_points()` is deprecated in favour of
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md).

## Usage

``` r
construct_model_points(
  x,
  group_by = NULL,
  exposure = NULL,
  exposure_by = NULL,
  aggregate_cols = NULL,
  drop_na = FALSE,
  group_vars = NULL,
  agg_cols = NULL
)
```

## Arguments

- x:

  A `data.frame`, an object of class `"model_data"` returned by
  [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md),
  or a fitted model that can be passed to
  [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md).

- group_by:

  See
  [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md).
  The legacy `exposure` and `exposure_by` arguments correspond to
  `split` and `split_by`.

- exposure, exposure_by:

  See `split` and `split_by` in
  [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md).

- aggregate_cols:

  Optional character vector with additional numeric columns to sum per
  model point using `sum(na.rm = TRUE)`. These totals are not split
  across output columns.

- drop_na:

  Logical. With `FALSE` (default), missing values in `group_by` form a
  distinct grouping value and those rows still contribute to the totals.
  With `TRUE`, a missing value in any `group_by` column excludes the
  entire row from counts, exposure and other totals. Only `group_by`
  columns are checked; missing values in the split or aggregation
  columns do not trigger this filter.

- group_vars, agg_cols:

  Deprecated argument names. Use `group_by` and `aggregate_cols`
  instead.

## Value

See
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md).
