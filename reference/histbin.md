# Deprecated alias for `outlier_histogram()`

`histbin()` is deprecated as of version 0.8.0. Please use
[`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
instead.

In addition, note that `x` must now be passed as **string** (standard
evaluation).

## Usage

``` r
histbin(
  data,
  x,
  left = NULL,
  right = NULL,
  line = FALSE,
  bins = 30,
  fill = "#E6E6E6",
  color = "white",
  fill_outliers = "#F28E2B"
)
```

## Arguments

- data:

  A data.frame containing the portfolio variable to inspect.

- x:

  Character; numeric column in `data` to plot.

- left, right:

  Deprecated aliases for `lower` and `upper`.

- line:

  Deprecated alias for `density`.

- bins:

  Integer. Number of bins used for the displayed range. Default = 30.

- fill, color, fill_outliers:

  Deprecated aliases for `bar_fill`, `bar_color`, and `tail_fill`.

## Value

See
[`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md).
