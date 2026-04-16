# Autoplot for tariff class objects

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method for objects created by
[`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md).
Produces a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
of the fitted GAM together with the constructed tariff class splits.
Optionally, confidence intervals and observed data points can be added.

## Usage

``` r
# S3 method for class 'constructtariffclasses'
autoplot(
  object,
  conf_int = FALSE,
  color_gam = "steelblue",
  show_observations = FALSE,
  color_splits = "grey50",
  size_points = 1,
  color_points = "black",
  rotate_labels = FALSE,
  remove_outliers = NULL,
  ...
)
```

## Arguments

- object:

  An object of class `"constructtariffclasses"`, produced by
  [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md).

- conf_int:

  Logical, whether to plot 95% confidence intervals. Default = `FALSE`.

- color_gam:

  Color of the fitted GAM line. Default = `"steelblue"`.

- show_observations:

  Logical, whether to add observed data points for each level of the
  risk factor. Default = `FALSE`.

- color_splits:

  Color of the vertical split lines. Default = `"grey50"`.

- size_points:

  Numeric, size of points if `show_observations = TRUE`. Default = 1.

- color_points:

  Color of observed points. Default = `"black"`.

- rotate_labels:

  Logical, whether to rotate x-axis labels by 45 degrees. Default =
  `FALSE`.

- remove_outliers:

  Numeric, exclude observations above this value from the plot (helps
  with extreme outliers). Default = `NULL`.

- ...:

  Additional arguments passed to
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Author

Martin Haringa
