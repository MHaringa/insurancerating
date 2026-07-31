# Inspect simulation-based residual uniformity

Plot the scaled residuals returned by
[`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md)
against their theoretical uniform quantiles. Systematic departures from
the diagonal can indicate remaining structure, distributional mismatch
or influential observations in a fitted pricing model.

## Usage

``` r
# S3 method for class 'check_residuals'
autoplot(object, show_message = TRUE, max_points = 1000, ...)
```

## Arguments

- object:

  An object produced by
  [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md).

- show_message:

  Logical. If `TRUE`, print a concise interpretation of the
  Kolmogorov-Smirnov p-value.

- max_points:

  Maximum number of QQ-plot points to display. If the residual check
  contains more points, an evenly spaced subset is shown. Use `Inf` to
  display all points.

- ...:

  Currently unused.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

The subtitle reports the uniformity-test p-value stored in `object`.
This p-value is a diagnostic signal rather than a stand-alone model
acceptance criterion. The shape and location of deviations should be
assessed together with exposure, fitted values and relevant risk-factor
levels.

Reducing `max_points` affects only the displayed QQ points; it does not
change the residual calculation or the reported test.

## See also

[`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md),
[`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md)

## Author

Martin Haringa
