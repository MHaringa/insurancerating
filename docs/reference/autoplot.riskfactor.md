# Plot risk factor effects from `rating_table()` results

Create a ggplot visualisation of a `riskfactor` object produced by
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).
Estimates are plotted per risk factor, with optional exposure bars and,
optionally, an additional univariate line.

When `univariate_scale = "reference"`, the univariate line is scaled to
the model reference group. The reference group is determined as the
level with model coefficient equal to 1. If no level is exactly equal to
1, the level with coefficient closest to 1 is used.

## Usage

``` r
# S3 method for class 'riskfactor'
autoplot(
  object,
  risk_factors = NULL,
  ncol = 1,
  labels = TRUE,
  dec.mark = ",",
  ylab = "Relativity",
  fill = NULL,
  color = NULL,
  linetype = FALSE,
  univariate = NULL,
  univariate_var = "risk_premium",
  univariate_name = "Univariate",
  univariate_color = NULL,
  univariate_scale = c("reference", "mean"),
  rotate_angle = NULL,
  custom_theme = NULL,
  remove_underscores = FALSE,
  ...
)
```

## Arguments

- object:

  A `riskfactor` object returned by
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).

- risk_factors:

  Character vector specifying which risk factors to plot. Defaults to
  all risk factors.

- ncol:

  Number of columns in the patchwork layout. Default is 1.

- labels:

  Logical; if `TRUE`, show exposure values as labels on the bars.
  Default is `TRUE`.

- dec.mark:

  Character; decimal separator, either `","` (default) or `"."`.

- ylab:

  Character; label for the y-axis. Default is `"Relativity"`.

- fill:

  Fill color for the exposure bars. If `NULL`, taken from the internal
  palette.

- color:

  Optional override for model line colors. If `NULL`, colors are taken
  from the internal discrete palette.

- linetype:

  Logical; if `TRUE`, use different line types for models. Default is
  `FALSE`.

- univariate:

  Optional `univariate` object returned by
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md).
  If supplied, the selected univariate statistic is added as an extra
  line.

- univariate_var:

  Character; statistic from `univariate` to plot. Default is
  `"risk_premium"`.

- univariate_name:

  Character; legend label for the univariate line. Default is
  `"Univariate"`.

- univariate_color:

  Optional override for the univariate line color. If `NULL`, the
  internal risk premium color is used.

- univariate_scale:

  Character; scaling applied to the univariate line. One of
  `"reference"` (default) or `"mean"`.

- rotate_angle:

  Numeric value for angle of labels on the x-axis (degrees).

- custom_theme:

  List with customised theme options.

- remove_underscores:

  Logical; remove underscores from labels.

- ...:

  Additional arguments passed to ggplot2 layers.

## Value

A `ggplot`/`patchwork` object.
