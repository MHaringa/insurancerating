# Plot risk factor effects from `rating_table()` results

Create a ggplot visualisation of a `rating_table` object produced by
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).
Estimates are plotted per risk factor, with optional exposure bars.
Observed portfolio experience can be added first with
[`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md).

When observed experience is attached, it is plotted as an additional
line. The scaling is controlled by
[`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md).

## Usage

``` r
# S3 method for class 'rating_table'
autoplot(
  object,
  risk_factors = NULL,
  metric = NULL,
  ncol = 1,
  show_exposure_labels = TRUE,
  decimal_mark = ",",
  y_label = "Relativity",
  bar_fill = NULL,
  model_color = NULL,
  use_linetype = FALSE,
  rotate_angle = NULL,
  custom_theme = NULL,
  remove_underscores = FALSE,
  labels = NULL,
  dec.mark = NULL,
  ylab = NULL,
  fill = NULL,
  color = NULL,
  linetype = NULL,
  ...
)
```

## Arguments

- object:

  A `rating_table` object returned by
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).

- risk_factors:

  Character vector specifying which risk factors to plot. Defaults to
  all risk factors.

- metric:

  Optional character string. Observed-experience metric to plot when
  observed experience has been attached with
  [`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md).
  Common choices are `"frequency"`, `"severity"`/`"average_severity"`
  and `"risk_premium"`.

- ncol:

  Number of columns in the patchwork layout. Default is 1.

- show_exposure_labels:

  Logical; if `TRUE`, show exposure values as labels on the bars.
  Default is `TRUE`.

- decimal_mark:

  Character; decimal separator, either `","` (default) or `"."`.

- y_label:

  Character; label for the y-axis. Default is `"Relativity"`.

- bar_fill:

  Fill color for the exposure bars. If `NULL`, taken from the internal
  palette.

- model_color:

  Optional override for model line colors. If `NULL`, colors are taken
  from the internal discrete palette.

- use_linetype:

  Logical; if `TRUE`, use different line types for models. Default is
  `FALSE`.

- rotate_angle:

  Numeric value for angle of labels on the x-axis (degrees).

- custom_theme:

  List with customised theme options.

- remove_underscores:

  Logical; remove underscores from labels.

- labels:

  Deprecated alias for `show_exposure_labels`.

- dec.mark:

  Deprecated alias for `decimal_mark`.

- ylab:

  Deprecated alias for `y_label`.

- fill:

  Deprecated alias for `bar_fill`.

- color:

  Deprecated alias for `model_color`.

- linetype:

  Deprecated alias for `use_linetype`.

- ...:

  Additional arguments passed to ggplot2 layers.

## Value

A `ggplot`/`patchwork` object.
