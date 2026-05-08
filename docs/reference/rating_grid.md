# Construct observed rating-grid points from model data or a data frame

`rating_grid()` constructs rating-grid points by collapsing rows with
identical combinations of grouping variables to a single row.

The function returns only combinations that are actually observed in the
input data. It does **not** create the full Cartesian product of all
unique values. This keeps the output compact and suitable for model
diagnostics, portfolio summaries, and prediction analysis.

When `x` is an object returned by
[`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md),
the function uses the extracted model metadata to determine the grouping
variables if `group_by` is not supplied. When `x` is a plain
`data.frame`, it is recommended to supply `group_by` explicitly.

`construct_model_points()` is deprecated in favour of `rating_grid()`.

## Usage

``` r
rating_grid(
  x,
  group_by = NULL,
  exposure = NULL,
  exposure_by = NULL,
  aggregate_cols = NULL,
  drop_na = FALSE,
  group_vars = NULL,
  agg_cols = NULL
)

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

A `data.frame` with one row per observed rating-grid point.

## Details

The implementation uses base R only. Output is always a regular
`data.frame`, not a tibble or data.table.

If `exposure_by` is supplied, exposure or row counts are split across
levels of that variable and returned in wide format, for example
`"exposure_2020"` or `"count_2020"`.

For objects returned by
[`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md),
refinement mappings are joined by their original factor column. They are
not cross-joined onto every row.

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
rating_grid(mtcars, group_by = c("cyl", "vs"))

rating_grid(
  mtcars,
  group_by = c("cyl", "vs"),
  exposure = "disp",
  exposure_by = "gear",
  aggregate_cols = "mpg"
)

pmodel <- glm(
  breaks ~ wool + tension,
  data = warpbreaks,
  family = poisson(link = "log")
)

pmodel |>
  extract_model_data() |>
  rating_grid()
} # }
```
