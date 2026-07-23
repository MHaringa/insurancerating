# Plot a model refinement step

Takes a `rating_refinement` object and plots one refinement step before
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called. This is useful for checking whether manual tariff
restrictions, smoothing or expert-based relativities behave as intended
before they are used in a refined pricing model.

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
  x_max = NULL,
  y_max = NULL,
  remove_underscores = FALSE,
  rotate_angle = NULL,
  custom_theme = NULL,
  ...
)
```

## Arguments

- object:

  Object of class `rating_refinement`.

- variable:

  Optional character string specifying the risk factor to plot. If
  `NULL` (default), all available variables in the refinement object are
  shown. If specified, only the selected risk factor is plotted.

- step:

  Optional integer specifying which refinement step to plot. This is
  mainly relevant when multiple refinement steps have been applied (e.g.
  multiple calls to
  [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
  [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
  or
  [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)).

  - If `NULL` (default), the latest refinement step is shown.

  - If specified, the corresponding step in the refinement sequence is
    used.

  This makes it possible to inspect intermediate refinement stages
  before calling
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).

- x_max:

  Optional single finite numeric value. Maximum value displayed on the
  x-axis of a smoothing plot. This changes only the visible plotting
  range; it does not remove observations, alter the fitted smoothing
  curve or affect
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
  It is useful when a small number of extreme values would otherwise
  compress the range containing most portfolio risks. For example, use
  `x_max = 1e7` to display insured values up to 10 million. This
  argument is only available for smoothing steps.

- y_max:

  Optional single finite numeric value. Maximum relativity displayed on
  the y-axis of a smoothing plot. Like `x_max`, this changes only the
  visible plotting range and does not alter the smoothing fit,
  refinement data or
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
  This argument is only available for smoothing steps.

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
