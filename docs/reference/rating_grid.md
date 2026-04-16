# Construct observed rating-grid points from model data or a data frame

`rating_grid()` constructs rating-grid points by collapsing rows with
identical combinations of grouping variables to a single row.

The function is intended for analytical use cases where it is convenient
to work with one row per observed combination of risk factors or model
variables. In many raw datasets, the same combination occurs multiple
times. For model diagnostics, portfolio summaries, and prediction
analysis, it is often more useful to aggregate these repeated
combinations than to keep all original rows.

By default, the function returns only combinations that are actually
observed in the input data. It does **not** create the full Cartesian
product of all unique values, because that can become very large and is
often not needed for analysis.

In other words, the function:

- identifies the rating-grid dimensions;

- groups rows with identical combinations of these variables;

- aggregates exposure and optional numeric columns to one row per
  observed combination.

This is especially useful for:

- analysing model structure,

- summarising portfolios at rating-grid level,

- comparing observed and fitted values,

- creating compact input for further analysis or plotting.

When `x` is an object returned by
[`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md),
the function uses the extracted model metadata to determine the grouping
variables if `group_vars` is not supplied. When `x` is a plain
`data.frame`, it is recommended to supply `group_vars` explicitly.

`construct_model_points()` is deprecated in favour of `rating_grid()`.

## Usage

``` r
rating_grid(
  x,
  group_vars = NULL,
  exposure = NULL,
  exposure_by = NULL,
  agg_cols = NULL,
  drop_na = FALSE
)

construct_model_points(
  x,
  group_vars = NULL,
  exposure = NULL,
  exposure_by = NULL,
  agg_cols = NULL,
  drop_na = FALSE
)
```

## Arguments

- x:

  A `data.frame` or an object of class `"model_data"` returned by
  [`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md).

- group_vars:

  Optional character vector with the variables that define the
  rating-grid points. If `NULL` and `x` is a `"model_data"` object, the
  risk-factor variables stored in the object are used. If `NULL` and `x`
  is a plain `data.frame`, all columns except those listed in
  `exposure`, `exposure_by`, and `agg_cols` are used.

- exposure:

  Optional character; name of the exposure column to aggregate.

- exposure_by:

  Optional character; name of a column used to split exposure or counts,
  for example a year variable.

- agg_cols:

  Optional character vector with additional numeric columns to aggregate
  using `sum(na.rm = TRUE)`.

- drop_na:

  Logical; if `TRUE`, rows with missing values in `group_vars` are
  removed before aggregation. Default is `FALSE`.

## Value

A `data.frame` with one row per observed rating-grid point.

## Details

The function does **not** construct all theoretically possible
rating-grid combinations. Instead, it only keeps combinations that
actually occur in the input data and aggregates duplicates.

If `exposure_by` is supplied, exposure or row counts are split across
levels of that variable and returned in wide format, for example
`"exposure_2020"` or `"count_2020"`.

For objects returned by
[`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md),
additional refinement variables stored in the object attributes may be
retained when they are not already part of the regular grouping
variables.

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

# With a data.frame
mtcars |>
  dplyr::select(cyl, vs) |>
  rating_grid(group_vars = c("cyl", "vs"))

mtcars |>
  dplyr::select(cyl, vs, disp) |>
  rating_grid(
    group_vars = c("cyl", "vs"),
    exposure = "disp"
  )

mtcars |>
  dplyr::select(cyl, vs, disp, gear) |>
  rating_grid(
    group_vars = c("cyl", "vs"),
    exposure = "disp",
    exposure_by = "gear"
  )

mtcars |>
  dplyr::select(cyl, vs, disp, gear, mpg) |>
  rating_grid(
    group_vars = c("cyl", "vs"),
    exposure = "disp",
    exposure_by = "gear",
    agg_cols = c("mpg")
  )

# With extracted model data
pmodel <- glm(
  breaks ~ wool + tension,
  data = warpbreaks,
  family = poisson(link = "log")
)

pmodel |>
  model_data() |>
  rating_grid()
} # }
```
