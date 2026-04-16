# Histogram with outlier bins

Visualize the distribution of a continuous variable using bins. Values
below `left` or above `right` can be grouped into outlier bins for
compact display when the range of values is wide.

`histbin()` is deprecated as of version 0.8.0. Please use
`outlier_histogram()` instead.

In addition, note that `x` must now be passed as **string** (standard
evaluation).

## Usage

``` r
outlier_histogram(
  data,
  x,
  left = NULL,
  right = NULL,
  line = FALSE,
  bins = 30,
  fill = "steelblue",
  color = "white",
  fill_outliers = "#a7d1a7"
)

histbin(
  data,
  x,
  left = NULL,
  right = NULL,
  line = FALSE,
  bins = 30,
  fill = "steelblue",
  color = "white",
  fill_outliers = "#a7d1a7"
)
```

## Arguments

- data:

  A data.frame containing the variable to plot.

- x:

  Variable name in `data` to map on the x-axis.

- left:

  Optional numeric, floor of the range. Values below are binned
  together.

- right:

  Optional numeric, ceiling of the range. Values above are binned
  together.

- line:

  Logical. If TRUE, add a density line. Default = FALSE.

- bins:

  Integer. Number of bins to use. Default = 30.

- fill:

  Fill color for bars. If NULL, a default is chosen.

- color:

  Line color for bars. If NULL, a default is chosen.

- fill_outliers:

  Fill color for outlier bins. Default = "#a7d1a7".

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

This is a wrapper around
[`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).
The method for handling outliers is based on
<https://edwinth.github.io/blog/outlier-bin/>.

## Author

Martin Haringa

## Examples

``` r
outlier_histogram(MTPL2, "premium")

outlier_histogram(MTPL2, "premium", left = 30, right = 120, bins = 30)

```
