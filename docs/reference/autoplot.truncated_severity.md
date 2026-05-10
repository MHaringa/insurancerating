# Plot a fitted truncated severity distribution

Creates a plot of the empirical cumulative distribution function (ECDF)
of the observed truncated claim amounts together with the fitted
truncated CDF.

## Usage

``` r
# S3 method for class 'truncated_severity'
autoplot(
  object,
  ecdf_geom = c("point", "step"),
  x_label = NULL,
  y_label = NULL,
  y_limits = c(0, 1),
  x_limits = NULL,
  show_title = TRUE,
  digits = 2,
  truncation_digits = 2,
  geom_ecdf = NULL,
  xlab = NULL,
  ylab = NULL,
  ylim = NULL,
  xlim = NULL,
  print_title = NULL,
  print_dig = NULL,
  print_trunc = NULL,
  ...
)
```

## Arguments

- object:

  An object produced by
  [`fit_truncated_severity()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_severity.md).

- ecdf_geom:

  Character string indicating how to display the empirical CDF. Must be
  one of `"point"` or `"step"`.

- x_label:

  Title of the x axis. Defaults to `"severity"`.

- y_label:

  Title of the y axis. Defaults to `"cumulative proportion"`.

- y_limits:

  Numeric vector of length 2 specifying y-axis limits.

- x_limits:

  Optional numeric vector of length 2 specifying x-axis limits.

- show_title:

  Logical. If `TRUE`, print title and subtitle.

- digits:

  Integer. Number of digits for parameter estimates in the subtitle.

- truncation_digits:

  Integer. Number of digits used for truncation bounds.

- geom_ecdf, xlab, ylab, ylim, xlim, print_title, print_dig,
  print_trunc:

  Deprecated argument names kept for backward compatibility.

- ...:

  Currently unused.

## Value

A `ggplot2` object.

## Details

The plot compares the empirical distribution of the observed, truncated
claim severities with the fitted distribution conditional on the same
truncation interval. This is a visual check of whether the selected
severity distribution is plausible for the part of the portfolio that is
actually observed.

## Author

Martin Haringa
