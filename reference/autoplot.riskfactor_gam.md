# Autoplot for GAM objects from `risk_factor_gam()`

Generates a `ggplot2` visualization of a fitted GAM created with
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).
The plot shows the fitted curve, and optionally confidence intervals and
observed data points.

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

  Color for the fitted GAM line, specified by name (e.g., `"red"`) or
  hex code (e.g., `"#FF1234"`). Default is `"steelblue"`.

- show_observations:

  Logical. If `TRUE`, add observed frequency/severity points
  corresponding to the underlying data.

- x_stepsize:

  Numeric. Step size for tick marks on the x-axis. If `NULL`, breaks are
  determined automatically.

- size_points:

  Numeric. Point size for observed data. Default is `1`.

- color_points:

  Color for the observed data points. Default is `"black"`.

- rotate_labels:

  Logical. If `TRUE`, rotate x-axis labels by 45 degrees to reduce
  overlap.

- remove_outliers:

  Numeric. If specified, observations greater than this threshold are
  omitted from the plot.

- conf_int:

  Deprecated. Use `confidence` instead.

- ...:

  Additional arguments passed to underlying `ggplot2` functions.

## Value

A `ggplot` object representing the fitted GAM.

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
fit <- risk_factor_gam(MTPL,
                       risk_factor = "age_policyholder",
                       claim_count = "nclaims",
                       exposure = "exposure")

autoplot(fit, show_observations = TRUE)
} # }
```
