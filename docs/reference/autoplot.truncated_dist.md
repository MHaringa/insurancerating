# Automatically create a ggplot for objects obtained from fit_truncated_dist()

Creates a plot of the empirical cumulative distribution function (ECDF)
of the observed truncated data together with the fitted truncated CDF.

## Usage

``` r
# S3 method for class 'truncated_dist'
autoplot(
  object,
  geom_ecdf = c("point", "step"),
  xlab = NULL,
  ylab = NULL,
  ylim = c(0, 1),
  xlim = NULL,
  print_title = TRUE,
  print_dig = 2,
  print_trunc = 2,
  ...
)
```

## Arguments

- object:

  An object produced by
  [`fit_truncated_dist()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_dist.md).

- geom_ecdf:

  Character string indicating how to display the ECDF. Must be one of
  `"point"` or `"step"`.

- xlab:

  Title of the x axis. Defaults to `"severity"`.

- ylab:

  Title of the y axis. Defaults to `"cumulative proportion"`.

- ylim:

  Numeric vector of length 2 specifying y-axis limits.

- xlim:

  Numeric vector of length 2 specifying x-axis limits.

- print_title:

  Logical. If `TRUE`, print title and subtitle.

- print_dig:

  Integer. Number of digits for parameter estimates in the subtitle.

- print_trunc:

  Integer. Number of digits used for truncation bounds.

- ...:

  Currently unused.

## Value

A `ggplot2` object.

## Author

Martin Haringa
