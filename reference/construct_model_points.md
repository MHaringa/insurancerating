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

  Optional character vector with the variables that define the
  rating-grid points. If `NULL` and `x` is a `"model_data"` object, the
  risk-factor variables stored in the object are used. If `NULL` and `x`
  is a plain `data.frame`, all columns except those listed in
  `exposure`, `exposure_by`, and `aggregate_cols` are used.

- exposure:

  Optional character; name of the exposure column to aggregate.

- exposure_by:

  Optional character; name of a column used to split exposure or counts,
  for example a year variable.

- aggregate_cols:

  Optional character vector with additional numeric columns to aggregate
  using `sum(na.rm = TRUE)`.

- drop_na:

  Logical; if `TRUE`, rows with missing values in `group_by` are removed
  before aggregation. Default is `FALSE`.

- group_vars, agg_cols:

  Deprecated argument names. Use `group_by` and `aggregate_cols`
  instead.

## Value

See
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md).
