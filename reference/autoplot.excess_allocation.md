# Plot an excess-loss allocation

Visualise the blended excess loading, expected excess loss or
credibility by allocation group.

## Usage

``` r
# S3 method for class 'excess_allocation'
autoplot(
  object,
  y = c("blended_excess_loading", "expected_excess_loss", "credibility"),
  top_n = NULL,
  show_labels = FALSE,
  ...
)
```

## Arguments

- object:

  An object returned by
  [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md).

- y:

  Character. Measure to plot on the y-axis.

- top_n:

  Optional positive whole number. If supplied, only the largest `top_n`
  groups by `y` are shown.

- show_labels:

  Logical. If `TRUE`, add direct value labels to the bars.

- ...:

  Unused.

## Value

A `ggplot` object.

## Author

Martin Haringa
