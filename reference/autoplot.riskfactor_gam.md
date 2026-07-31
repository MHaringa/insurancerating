# Inspect a smooth continuous risk-factor effect

Plot the smooth effect estimated by
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).
Observed aggregated experience and pointwise confidence intervals can be
added to assess how the fitted pattern relates to the available
portfolio information.

## Usage

``` r
# S3 method for class 'riskfactor_gam'
autoplot(
  object,
  confidence = FALSE,
  color_gam = "steelblue",
  show_observations = FALSE,
  x_stepsize = NULL,
  size_points = 1,
  color_points = "black",
  rotate_labels = FALSE,
  remove_outliers = NULL,
  conf_int = NULL,
  ...
)
```

## Arguments

- object:

  An object of class `"riskfactor_gam"` returned by
  [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).

- confidence:

  Logical. If `TRUE`, add 95% confidence intervals around the fitted
  curve. Default is `FALSE`.

- color_gam:

  Colour for the fitted GAM line.

- show_observations:

  Logical. If `TRUE`, add observed frequency/severity or risk-premium
  values used for fitting.

- x_stepsize:

  Numeric. Step size for tick marks on the x-axis. If `NULL`, breaks are
  determined automatically.

- size_points:

  Numeric. Point size for observed experience.

- color_points:

  Colour for observed experience.

- rotate_labels:

  Logical. If `TRUE`, rotate x-axis labels by 45 degrees to reduce
  overlap.

- remove_outliers:

  Optional numeric upper display limit for observed points. The fitted
  curve remains unchanged.

- conf_int:

  Deprecated. Use `confidence` instead.

- ...:

  Additional arguments reserved for method compatibility.

## Value

A `ggplot2` object.

## Details

The line is the fitted response on its natural scale: frequency, average
severity or risk premium. Observed points represent experience
aggregated at the continuous risk-factor values used for fitting.

Confidence intervals describe uncertainty in the fitted curve
conditional on the selected GAM specification. They do not include
uncertainty from model selection, omitted risk factors or future
portfolio changes. Wide intervals and isolated observations in the tails
should therefore be reviewed together with exposure and claim volume.

`remove_outliers` affects only displayed observed points. It does not
remove data from the fitted GAM or alter the prediction curve.

## See also

[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md),
[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md),
[`autoplot.tariff_segments()`](https://mharinga.github.io/insurancerating/reference/autoplot.tariff_segments.md)

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- risk_factor_gam(
  MTPL,
  risk_factor = "age_policyholder",
  claim_count = "nclaims",
  exposure = "exposure"
)

autoplot(fit, confidence = TRUE, show_observations = TRUE)
} # }
```
