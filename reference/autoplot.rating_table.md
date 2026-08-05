# Compare fitted risk-factor effects graphically

Plot the coefficients or relativities stored in a
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
object by risk factor. Multiple fitted models can be compared, exposure
can be shown as background bars, and observed portfolio experience
attached with
[`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md)
can be added as a separate line.

## Usage

``` r
# S3 method for class 'rating_table'
autoplot(
  object,
  risk_factors = NULL,
  metric = NULL,
  ncol = 1,
  legend_position = c("auto", "right", "bottom", "top", "left", "none"),
  show_exposure_labels = TRUE,
  decimal_mark = ",",
  y_label = "Relativity",
  bar_fill = NULL,
  model_color = NULL,
  use_linetype = FALSE,
  abbreviate_labels = TRUE,
  label_width = 20,
  label_abbreviations = NULL,
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

  A `"rating_table"` object returned by
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).

- risk_factors:

  Optional character vector specifying the risk factors to plot. If
  `NULL`, all available risk factors are shown.

- metric:

  Optional character string. Observed-experience metric to plot when
  observed experience has been attached with
  [`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md).
  Common choices are `"frequency"`, `"severity"`/`"average_severity"`
  and `"risk_premium"`.

- ncol:

  Positive integer specifying the number of columns in the patchwork
  layout.

- legend_position:

  Character string specifying the legend position. The default,
  `"auto"`, hides the legend when only one fitted model is shown and no
  observed-experience line is present. It places the legend on the right
  when multiple fitted models or an observed-experience comparison are
  shown. Use `"right"`, `"bottom"`, `"top"`, `"left"` or `"none"` to
  override this behaviour.

- show_exposure_labels:

  Logical. If `TRUE`, print exposure values on the background bars.

- decimal_mark:

  Character string, either `","` or `"."`, controlling number labels.

- y_label:

  Character string for the primary y-axis.

- bar_fill:

  Optional colour for exposure bars. If `NULL`, the package palette is
  used.

- model_color:

  Optional single colour overriding the model-line palette.

- use_linetype:

  Logical. If `TRUE`, distinguish fitted models by line type as well as
  colour.

- abbreviate_labels:

  Logical. If `TRUE`, long risk-factor level labels are shortened to
  `label_width` characters. A shortened label ends in one period; for
  example, `"Bouwnijverheid"` becomes `"Bouwn."` when `label_width = 6`.
  Only the displayed axis labels are changed.

- label_width:

  Positive whole number of at least 2. Maximum number of characters in
  automatically shortened level labels.

- label_abbreviations:

  Optional named character vector with explicit display labels, for
  example
  `c("Bouwnijverheid" = "Bouwn.", "Onroerend goed" = "Onr. goed")`.
  Explicit labels take precedence over automatic shortening.

- rotate_angle:

  Optional numeric angle for risk-factor level labels.

- custom_theme:

  Optional named list passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- remove_underscores:

  Logical. If `TRUE`, replace underscores with spaces in risk-factor
  axis labels.

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

  Additional arguments reserved for method compatibility.

## Value

A `patchwork` object containing one `ggplot2` panel per selected risk
factor.

## Details

### Plot contents

One panel is produced for each selected risk factor. Model effects use
the primary y-axis. When exposure is available, bars are rescaled to the
plotting range and the original exposure scale is shown on the secondary
y-axis. Panel and level order follow the input
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
object. This keeps the reference level and any explicit actuarial review
order consistent between the data frame,
[`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
and the plot.

Observed experience is plotted only after it has been attached with
[`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md).
The selected `metric` is converted to the relative scale recorded in
that object, using either the model reference level or the portfolio
mean.

### Actuarial interpretation

The plot supports comparison of fitted tariff effects, portfolio volume
and unadjusted observed experience. Differences between the observed and
modelled lines may indicate portfolio-mix effects, sparse levels, model
smoothing or genuine lack of fit. The chart does not separate these
explanations and should be reviewed together with claim counts, residual
diagnostics and stability across periods.

When models are compared, the analyst should ensure that response
definitions, link functions and relativity scales are sufficiently
comparable. Exposure bars provide volume context but are not confidence
intervals.

## See also

[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md),
[`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md),
[`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md),
[`as_gt.rating_table()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- MTPL
portfolio$zip <- as.factor(portfolio$zip)

frequency <- glm(
  nclaims ~ bm + zip + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

effects <- rating_table(
  frequency,
  model_data = portfolio,
  exposure = "exposure"
)

autoplot(effects, risk_factors = "zip", show_exposure_labels = FALSE)

```
