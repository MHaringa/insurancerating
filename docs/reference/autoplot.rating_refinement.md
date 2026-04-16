# Automatically create a ggplot for objects obtained from refinement

**\[experimental\]** Takes an object produced by
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
or
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
and creates a plot comparing the adjusted coefficients with the original
coefficients obtained from the model.

For objects produced by
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md),
original levels that are split into new levels are removed from the
connected original line and from the x-axis. Instead, the original level
is shown as a horizontal blue segment spanning all child categories,
with the original level label centred above the segment.

## Usage

``` r
# S3 method for class 'rating_refinement'
autoplot(
  object,
  variable = NULL,
  step = NULL,
  remove_underscores = FALSE,
  rotate_angle = NULL,
  custom_theme = NULL,
  ...
)
```

## Arguments

- object:

  Object produced by
  [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  or
  [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

- remove_underscores:

  Logical; if `TRUE`, underscores are replaced by spaces in the x-axis
  label. Default is `FALSE`.

- rotate_angle:

  Optional numeric value for the angle of x-axis labels.

- custom_theme:

  Optional list passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- ...:

  Additional plotting arguments passed to ggplot2 geoms.

## Value

A `ggplot2` object.

## Author

Martin Haringa
