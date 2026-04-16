# Autoplot for check_residuals objects

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method for objects created by
[`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md).
Produces a simulation-based uniform QQ-plot of the residuals, with the
Kolmogorov-Smirnov p-value shown in the subtitle. Optionally prints a
message about whether deviations are detected.

## Usage

``` r
# S3 method for class 'check_residuals'
autoplot(object, show_message = TRUE, ...)
```

## Arguments

- object:

  An object of class `"check_residuals"`, produced by
  [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md).

- show_message:

  Logical. If TRUE (default), prints a short message based on the
  p-value from the KS test.

- ...:

  Additional arguments passed to
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Author

Martin Haringa
